
rm_list <- ls(pattern = "data|model|fit|result", envir = .GlobalEnv)
rm(list = rm_list, envir = .GlobalEnv)
cat("已清理的对象:", paste(rm_list, collapse = ", "), "\n")


# 1. 加载必要的库
library(caret)    # 机器学习核心框架
library(rms)      # 回归模型与 RCS
library(pROC)     # ROC 曲线绘制
library(xgboost)  # 极致梯度提升
library(e1071)    # SVM & Naive Bayes 支持
library(nnet)     # 神经网络支持
library(rpart)    # 决策树支持
install.packages('klaR')
library(klaR)
library(readr)
test_data <- read.csv("C:/Users/lenovo/Desktop/大论文文档/先标准化/标准化后验证集.csv", fileEncoding = "GBK")
train_data <- read.csv("C:/Users/lenovo/Desktop/大论文文档/先标准化/标准化后训练集.csv", fileEncoding = "GBK")
View(test_data)#初步查看我的数据
View(train_data)#初步查看我的数据

# --- 准备工作：变量定义 ---
# 根据Wald 检验和森林图结果定义
vars_full <- c("碱性磷酸酶", "中淋", "尿酸肌酐", "空腹", "空腹1h", "白细胞", "血红蛋白", "纤维蛋白原", "谷丙转氨酶")
vars_reduced <- c("碱性磷酸酶", "尿酸肌酐", "空腹", "血红蛋白", "谷丙转氨酶")

# 确保因变量是因子类型且级别为 "No", "Yes" (caret 训练需要)
train_data$是否 <- factor(train_data$是否, levels = c(0, 1), labels = c("No", "Yes"))
test_data$是否 <- factor(test_data$是否, levels = c(0, 1), labels = c("No", "Yes"))

# --- 第一步：数据标准化 (基于训练集参数防止泄露) ---
# 对所有连续变量进行标准化
preProcParams <- preProcess(train_data[, vars_full], method = c("center", "scale"))
train_std <- predict(preProcParams, train_data)
test_std <- predict(preProcParams, test_data)

# --- 第二步：配置 rms 环境 (用于 RCS) ---

dd <- datadist(train_std)
options(datadist = "dd")


set.seed(2026)
# 训练控制：10折交叉验证 + 开启参数搜索
fitControl_robust <- trainControl(
  method = "cv", 
  number = 10, 
  classProbs = TRUE, 
  summaryFunction = twoClassSummary,
  sampling = "down"  # 核心：平衡样本分布，显著减少过拟合
)

# ==========================================
# 3. 核心建模函数 (带强参数约束)
# ==========================================
run_robust_analysis <- function(v_list, d_train) {
  f_base <- as.formula(paste("是否 ~", paste(v_list, collapse = " + ")))
  models <- list()
  
  # 1. GBM (限制 interaction.depth = 1 是最强的抗过拟合手段)
  cat("Training Robust GBM...\n")
  set.seed(2026)
  models$GBM <- train(f_base, data = d_train, method = "gbm", 
                      trControl = fitControl_robust,
                      tuneGrid = expand.grid(interaction.depth = 1, 
                                             n.trees = c(50, 100), 
                                             shrinkage = c(0.01, 0.1), 
                                             n.minobsinnode = 20),
                      metric = "ROC", verbose = FALSE)
  
  # 2. RF (增加 nodesize 限制树的生长)
  cat("Training Robust RF...\n")
  set.seed(2026)
  models$RF <- train(f_base, data = d_train, method = "rf", 
                     trControl = fitControl_robust,
                     tuneGrid = expand.grid(mtry = c(2, 3)),
                     nodesize = 20, # 强制增加末端节点样本量
                     metric = "ROC")
  
  # 3. SVM (低 Cost 策略)
  models$SVM <- train(f_base, data = d_train, method = "svmRadial", 
                      trControl = fitControl_robust,
                      tuneGrid = expand.grid(sigma = c(0.01), C = c(0.25, 0.5)), metric = "ROC")
  
  # 4. KNN (大 K 策略平滑边界)
  set.seed(2026)
  models$KNN <- train(f_base, data = d_train, method = "knn", 
                      trControl = fitControl_robust,
                      tuneGrid = expand.grid(k = c(11, 15, 21)), metric = "ROC")
  
  # 5. ANN (高 Decay 策略)
  set.seed(2026)
  models$ANN <- train(f_base, data = d_train, method = "nnet", 
                      trControl = fitControl_robust,
                      tuneGrid = expand.grid(size = c(1, 3), decay = c(0.5, 1)), 
                      trace = FALSE, metric = "ROC")
  
  # 6. DT (增加剪枝)
  set.seed(2026)
  models$DT <- train(f_base, data = d_train, method = "rpart", 
                     trControl = fitControl_robust, tuneLength = 5, metric = "ROC")
  
  # 7. NB
  set.seed(2026)
  models$NB <- train(f_base, data = d_train, method = "nb", trControl = fitControl_robust, metric = "ROC")
  
  # 8. LR_RCS
  set.seed(2026)
  f_rcs <- as.formula(paste("是否 ~ rcs(碱性磷酸酶, 3) +", paste(setdiff(v_list, "碱性磷酸酶"), collapse = " + ")))
  models$LR_RCS <- lrm(f_rcs, data = d_train, x = TRUE, y = TRUE)
  
  return(models)
}

# ==========================================
# 4. 性能提取函数 (基于训练集阈值)
# ==========================================
get_performance_table <- function(model_list, d_train, d_test, group_label) {
  perf_list <- lapply(names(model_list), function(name) {
    m <- model_list[[name]]
    
    # 获取概率
    if (name == "LR_RCS") {
      tr_p <- as.numeric(1/(1+exp(-predict(m, d_train, type="lp"))))
      te_p <- as.numeric(1/(1+exp(-predict(m, d_test, type="lp"))))
    } else {
      tr_p <- predict(m, d_train, type="prob")$Yes
      te_p <- predict(m, d_test, type="prob")$Yes
    }
    
    # 基于训练集 Youden 指数确定阈值
    roc_tr <- roc(d_train$是否, tr_p, quiet = TRUE)
    thresh <- coords(roc_tr, "best", ret="threshold", transpose = TRUE)[1]
    
    # 评价内部函数
    eval_func <- function(actual, prob, ds_name) {
      pred <- factor(ifelse(prob >= thresh, "Yes", "No"), levels=c("No", "Yes"))
      cm <- confusionMatrix(pred, actual, positive="Yes")
      data.frame(Group = group_label, Dataset = ds_name, Model = name, Threshold = round(thresh, 3),
                 AUC = round(as.numeric(auc(roc(actual, prob, quiet=T))), 3),
                 Accuracy = round(cm$overall[["Accuracy"]], 3), # 新增准确率
                 Sen = round(cm$byClass["Sensitivity"], 3),
                 Spe = round(cm$byClass["Specificity"], 3),
                 PPV = round(cm$byClass["Precision"],3),
                 NPV <- round(cm$byClass["Neg Pred Value"],3))
    }
    
    rbind(eval_func(d_train$是否, tr_p, "Train"), eval_func(d_test$是否, te_p, "Test"))
  })
  return(do.call(rbind, perf_list))
}

# ==========================================
# 5. 执行建模与结果对比
# ==========================================
cat("--- 正在运行全量组模型 ---")
models_full <- run_robust_analysis(vars_full, train_std)
report_full <- get_performance_table(models_full, train_std, test_std, "Full")

cat("--- 正在运行精简组模型 ---")
models_reduced <- run_robust_analysis(vars_reduced, train_std)
report_reduced <- get_performance_table(models_reduced, train_std, test_std, "Reduced")

# 合并并展示最终报表
final_report <- rbind(report_full, report_reduced)
print(final_report)

# 保存到本地
write.csv(final_report, "Robust_Model_Comparison_Results.csv", row.names = FALSE)


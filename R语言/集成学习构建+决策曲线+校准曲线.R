集成模型
# ---------------------------------------------------------
# 1. 准备 Stacking 训练数据
# ---------------------------------------------------------
# 提取训练集概率
train_p_gbm <- predict(models_full$GBM, train_std, type = "prob")[, "Yes"]
train_p_lr  <- as.numeric(1 / (1 + exp(-predict(models_full$LR_RCS, train_std))))

# 构造 Stacking 专用数据集
stack_train <- train_std
stack_train$gbm_prob <- train_p_gbm
stack_train$lr_prob  <- train_p_lr

# ---------------------------------------------------------
# 2. 训练元模型 (Meta-Model)
# ---------------------------------------------------------
# 使用逻辑回归作为元模型，将两个概率进行非线性融合
# 这里我们依然保留 RCS 以处理可能存在的非线性
library(rms)
dd <- datadist(stack_train); options(datadist='dd')

ensemble_model <- lrm(是否 ~ rcs(gbm_prob, 3) + rcs(lr_prob, 3), 
                      data = stack_train, x=TRUE, y=TRUE)

# ---------------------------------------------------------
# 3. 验证集评估
# ---------------------------------------------------------
# 提取验证集概率
test_p_gbm <- predict(models_full$GBM, test_std, type = "prob")[, "Yes"]
test_p_lr  <- as.numeric(1 / (1 + exp(-predict(models_full$LR_RCS, test_std))))

stack_test <- test_std
stack_test$gbm_prob <- test_p_gbm
stack_test$lr_prob  <- test_p_lr

# 获取集成模型的最终概率
final_p_ensemble <- as.numeric(1 / (1 + exp(-predict(ensemble_model, stack_test))))

library(pROC)
# 1. 计算集成模型的 AUC
roc_ensemble <- roc(stack_test$是否, final_p_ensemble, quiet = TRUE)
cat("集成模型验证集 AUC:", round(auc(roc_ensemble), 3), "\n")

# 2. 计算集成模型的校准指标 (S:P 值)
v_ensemble <- val.prob(final_p_ensemble, as.numeric(stack_test$是否)-1, pl=FALSE)
hl_p_ensemble <- hoslem.test(as.numeric(stack_test$是否)-1, final_p_ensemble, g=10)$p.value

cat("集成模型 Brier Score:", round(v_ensemble["Brier"], 4), "\n")
cat("集成模型 HL 检验 P 值:", round(hl_p_ensemble, 4), "\n")
cat("是否满足 P > 0.05:", ifelse(hl_p_ensemble > 0.05, "Yes! (校准成功)", "No"), "\n")

性能指标：
library(pROC)
library(caret)
library(ResourceSelection)

# ==========================================
# 1. 定义集成模型预测与评估核心函数
# ==========================================
evaluate_ensemble <- function(data_std, gbm_model, lr_model, meta_model, group_name) {
  
  # 自动获取正类标签 (假设为 "Yes")
  pos_label <- levels(data_std$是否)[2]
  
  # 提取基础模型概率
  p_gbm <- predict(gbm_model, data_std, type = "prob")[, pos_label]
  p_lr  <- as.numeric(1 / (1 + exp(-predict(lr_model, data_std))))
  
  # 构造元模型输入
  df_meta <- data_std
  df_meta$gbm_prob <- p_gbm
  df_meta$lr_prob  <- p_lr
  
  # 获取集成模型最终概率 (Link function: logit)
  p_ensemble <- as.numeric(1 / (1 + exp(-predict(meta_model, df_meta))))
  
  # 生成 ROC 对象 (用于后续提取阈值)
  roc_obj <- roc(data_std$是否, p_ensemble, quiet = TRUE)
  
  return(list(p_val = p_ensemble, roc = roc_obj, group = group_name))
}

# ==========================================
# 2. 执行初步预测获取预测值
# ==========================================
# 获取训练集预测结果
train_eval <- evaluate_ensemble(train_std, models_full$GBM, models_full$LR_RCS, ensemble_model, "Train")

# 获取验证集预测结果
test_eval  <- evaluate_ensemble(test_std, models_full$GBM, models_full$LR_RCS, ensemble_model, "Test")

# ==========================================
# 3. 基于训练集寻找最优阈值 (Youden Index)
# ==========================================
# 从训练集的 ROC 对象中寻找约登指数最大的点
best_coords_train <- coords(train_eval$roc, x = "best", best.method = "youden", 
                            ret = c("threshold", "specificity", "sensitivity"))

# 锁定训练集产生的临床阈值
opt_cut <- best_coords_train$threshold
cat("\n--- 临床决策阈值确定 ---\n")
cat("基于训练集确定的最优阈值 (Cut-off):", round(opt_cut, 4), "\n\n")

# ==========================================
# 4. 定义基于选定阈值的全性能指标计算函数
# ==========================================
get_final_stats <- function(prob_vec, true_label_factor, cut_off, group_name) {
  
  pos_label <- levels(true_label_factor)[2]
  neg_label <- levels(true_label_factor)[1]
  
  # 按照锁定阈值分类
  preds_cat <- factor(ifelse(prob_vec >= cut_off, pos_label, neg_label), 
                      levels = levels(true_label_factor))
  
  # 构建混淆矩阵
  cm <- table(Prediction = preds_cat, Reference = true_label_factor)
  
  # 提取基础统计量
  TP <- cm[pos_label, pos_label]
  TN <- cm[neg_label, neg_label]
  FP <- cm[pos_label, neg_label]
  FN <- cm[neg_label, pos_label]
  
  # 计算临床核心指标
  auc_val <- as.numeric(auc(roc(true_label_factor, prob_vec, quiet = TRUE)))
  sens <- TP / (TP + FN) # 敏感度
  spec <- TN / (TN + FP) # 特异度
  ppv  <- TP / (TP + FP) # 阳性预测值
  npv  <- TN / (TN + FN) # 阴性预测值
  acc  <- (TP + TN) / (TP + TN + FP + FN)
  f1   <- 2 * (ppv * sens) / (ppv + sens)
  
  # 计算校准度指标 (Brier & HL-Test)
  y_true_num <- as.numeric(true_label_factor) - 1
  brier <- mean((prob_vec - y_true_num)^2)
  hl_p  <- hoslem.test(y_true_num, prob_vec, g = 10)$p.value
  
  # 返回结果行
  data.frame(
    Cohort = group_name,
    AUC = round(auc_val, 4),
    Sensitivity = round(sens, 4),
    Specificity = round(spec, 4),
    PPV = round(ppv, 4),
    NPV = round(npv, 4),
    Accuracy = round(acc, 4),
    F1_Score = round(f1, 4),
    Brier_Score = round(brier, 4),
    HL_P_Value = round(hl_p, 4)
  )
}

# ==========================================
# 5. 汇总训练集与验证集的性能报表
# ==========================================
# 统一使用训练集确定的 opt_cut
final_train_metrics <- get_final_stats(train_eval$p_val, train_std$是否, opt_cut, "Training Set")
final_test_metrics  <- get_final_stats(test_eval$p_val, test_std$是否, opt_cut, "Validation Set")

# 合并展示结果
final_ensemble_report <- rbind(final_train_metrics, final_test_metrics)

cat("--- 集成模型性能评估汇总表 ---\n")
print(final_ensemble_report)

# 导出 CSV 文件供撰写论文使用
write.csv(final_ensemble_report, "Final_Ensemble_Report.csv", row.names = FALSE)

绘制校准曲线：
library(ggplot2)

# --- 准备校准曲线数据 ---
# 整合训练集和验证集的预测结果
cal_data_train <- data.frame(prob = train_eval$p_val, obs = as.numeric(train_std$是否)-1, Group = "Training Set")
cal_data_test  <- data.frame(prob = test_eval$p_val,  obs = as.numeric(test_std$是否)-1,  Group = "Testing Set")
cal_plot_df    <- rbind(cal_data_train, cal_data_test)

# --- 绘图 ---
ggplot(cal_plot_df, aes(x = prob, y = obs, color = Group)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey60") + # 理想线
  geom_smooth(method = "loess", se = FALSE, size = 1.2, span = 0.75) + # 圆滑校准线
  scale_color_manual(values = c("Training Set" = "#2E59A7", "Testing Set" = "#E03C39")) +
  theme_bw() +
  labs(
    title = "Ensemble Model Calibration Curve",
    x = "Predicted Probability",
    y = "Observed Frequency",
    color = "Cohort"
  ) +
  theme(
    legend.position = c(0.8, 0.2),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid.minor = element_blank()
  ) +
  scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1))

ggsave("Ensemble_Calibration_Combined.png", width = 7, height = 6, dpi = 300)


决策曲线：
library(dcurves)

# --- 准备 DCA 数据 ---
# 构造适合 dcurves 包的数据框
dca_train_df <- data.frame(outcome = as.numeric(train_std$是否)-1, Ensemble = train_eval$p_val)
dca_test_df  <- data.frame(outcome = as.numeric(test_std$是否)-1,  Ensemble = test_eval$p_val)

# --- 绘制验证集 DCA (论文通常重点展示验证集) ---
dca_res <- dca(outcome ~ Ensemble, 
               data = dca_test_df,
               thresholds = seq(0, 1, by = 0.01)) # 聚焦临床常用阈值区间

dca_plot <- dca_res %>%
  plot(smooth = TRUE) +
  theme_classic() +
  scale_color_manual(values = c("Ensemble" = "#E03C39", "All" = "grey70", "None" = "black")) +
  labs(
    title = "Decision Curve Analysis (Testing Set)",
    x = "Threshold Probability",
    y = "Net Benefit"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "right"
  )

print(dca_plot)
ggsave("Ensemble_DCA_Validation.png", width = 8, height = 6, dpi = 300)

ROC曲线：
library(pROC)
library(ggplot2)

library(pROC)
library(ggplot2)

# 1. 提取绘图数据
# 训练集 ROC
roc_train <- train_eval$roc
df_train <- data.frame(
  Specificity = roc_train$specificities,
  Sensitivity = roc_train$sensitivities,
  Cohort = paste0("Training Set (AUC: ", round(auc(roc_train), 3), ")")
)

# 验证集 ROC
roc_test <- test_eval$roc
df_test <- data.frame(
  Specificity = roc_test$specificities,
  Sensitivity = roc_test$sensitivities,
  Cohort = paste0("Testing Set (AUC: ", round(auc(roc_test), 3), ")")
)

# 合并数据
df_roc <- rbind(df_train, df_test)

# 2. 绘图（去除了标注点和文字）
ggplot(df_roc, aes(x = 1 - Specificity, y = Sensitivity, color = Cohort)) +
  geom_line(size = 1.2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey60") + # 灰色虚线对角线
  scale_color_manual(values = c("Training Set (AUC: 0.867)" = "#2E59A7", 
                                "Testing Set (AUC: 0.879)" = "#E03C39")) +
  # 注意：如果上面的 AUC 数值变了，请手动修改颜色对应标签，或者直接使用下面的自动配色：
  # scale_color_brewer(palette = "Set1") + 
  theme_bw() +
  labs(
    title = "ROC Curves of Ensemble Model",
    x = "1 - Specificity",
    y = "Sensitivity",
    color = "Model Performance"
  ) +
  theme(
    legend.position = c(0.75, 0.2), # 图例放在右下角
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.background = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  coord_fixed() # 保持正方形比例

# 保存图片
ggsave("Ensemble_ROC_Pure.png", width = 6, height = 6, dpi = 300)



比较训练集与验证集DCA
library(dcurves)
library(dplyr)
library(ggplot2)
library(patchwork) # 用于拼接图片，如果没有请先 install.packages("patchwork")

# 1. --- 分别计算 DCA ---
# 训练集
dca_train <- dca(outcome ~ Ensemble, 
                 data = data.frame(outcome = as.numeric(train_std$是否)-1, Ensemble = train_eval$p_val),
                 thresholds = seq(0, 1, by = 0.01))

# 验证集
dca_test <- dca(outcome ~ Ensemble, 
                data = data.frame(outcome = as.numeric(test_std$是否)-1, Ensemble = test_eval$p_val),
                thresholds = seq(0, 1, by = 0.01))

# 2. --- 绘制训练集图 ---
p_train <- dca_train %>%
  plot(smooth = TRUE) +
  theme_classic() +
  scale_color_manual(values = c("Ensemble" = "#E03C39", "All" = "grey70", "None" = "black")) +
  labs(title = "A: Training Set", x = "Threshold Probability", y = "Net Benefit") +
  theme(legend.position = "none") # 隐藏左图图例

# 3. --- 绘制验证集图 ---
p_test <- dca_test %>%
  plot(smooth = TRUE) +
  theme_classic() +
  scale_color_manual(values = c("Ensemble" = "#0072B2", "All" = "grey70", "None" = "black")) + # 验证集用蓝色区分
  labs(title = "B: Testing Set", x = "Threshold Probability", y = "Net Benefit")

# 4. --- 拼接图片 ---
# 使用 patchwork 的 + 号即可水平拼接
combined_plot <- p_train + p_test + 
  plot_layout(guides = "collect") & # 自动合并图例
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5, face = "bold"))

# 5. --- 显示与保存 ---
print(combined_plot)




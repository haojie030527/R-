# ==========================================
# 1. 设置训练控制（无平衡技术）
# ==========================================
set.seed(2026)
fitControl_no_balance <- trainControl(
  method = "cv", 
  number = 10, 
  classProbs = TRUE, 
  summaryFunction = twoClassSummary
  # 注意：这里没有 sampling = "down"
)

# ==========================================
# 2. 定义建模函数（无平衡技术，8个模型）
# ==========================================
run_analysis_no_balance <- function(v_list, d_train) {
  f_base <- as.formula(paste("是否 ~", paste(v_list, collapse = " + ")))
  models <- list()
  
  # 1. GBM
  cat("Training GBM (no balancing)...\n")
  set.seed(2026)
  models$GBM <- train(f_base, data = d_train, method = "gbm", 
                      trControl = fitControl_no_balance,
                      tuneGrid = expand.grid(interaction.depth = 1, 
                                             n.trees = c(50, 100), 
                                             shrinkage = c(0.01, 0.1), 
                                             n.minobsinnode = 20),
                      metric = "ROC", verbose = FALSE)
  
  # 2. RF
  cat("Training RF (no balancing)...\n")
  set.seed(2026)
  models$RF <- train(f_base, data = d_train, method = "rf", 
                     trControl = fitControl_no_balance,
                     tuneGrid = expand.grid(mtry = c(2, 3)),
                     nodesize = 20, 
                     metric = "ROC")
  
  # 3. SVM
  cat("Training SVM (no balancing)...\n")
  models$SVM <- train(f_base, data = d_train, method = "svmRadial", 
                      trControl = fitControl_no_balance,
                      tuneGrid = expand.grid(sigma = c(0.01), C = c(0.25, 0.5)), 
                      metric = "ROC")
  
  # 4. KNN
  cat("Training KNN (no balancing)...\n")
  set.seed(2026)
  models$KNN <- train(f_base, data = d_train, method = "knn", 
                      trControl = fitControl_no_balance,
                      tuneGrid = expand.grid(k = c(11, 15, 21)), 
                      metric = "ROC")
  
  # 5. ANN
  cat("Training ANN (no balancing)...\n")
  set.seed(2026)
  models$ANN <- train(f_base, data = d_train, method = "nnet", 
                      trControl = fitControl_no_balance,
                      tuneGrid = expand.grid(size = c(1, 3), decay = c(0.5, 1)), 
                      trace = FALSE, metric = "ROC")
  
  # 6. DT
  cat("Training Decision Tree (no balancing)...\n")
  set.seed(2026)
  models$DT <- train(f_base, data = d_train, method = "rpart", 
                     trControl = fitControl_no_balance, 
                     tuneLength = 5, metric = "ROC")
  
  # 7. NB
  cat("Training Naive Bayes (no balancing)...\n")
  set.seed(2026)
  models$NB <- train(f_base, data = d_train, method = "nb", 
                     trControl = fitControl_no_balance, metric = "ROC")
  
  # 8. LR_RCS (使用 lrm，不依赖 trainControl，无需改动)
  cat("Training LR_RCS (no balancing)...\n")
  set.seed(2026)
  f_rcs <- as.formula(paste("是否 ~ rcs(碱性磷酸酶, 3) +", 
                            paste(setdiff(v_list, "碱性磷酸酶"), collapse = " + ")))
  models$LR_RCS <- lrm(f_rcs, data = d_train, x = TRUE, y = TRUE)
  
  return(models)
}

# ==========================================
# 3. 运行模型（示例）
# ==========================================
# 假设您已经定义好 v_list（特征名列表）和 d_train（训练数据框）
# models_no_balance <- run_analysis_no_balance(v_list, d_train)
cat("--- 正在运行全量组模型 ---")
models_full <- run_analysis_no_balance(vars_full, train_std)
report_full <- get_performance_table(models_full, train_std, test_std, "Full")

cat("--- 正在运行精简组模型 ---")
models_reduced <- run_analysis_no_balance(vars_reduced, train_std)
report_reduced <- get_performance_table(models_reduced, train_std, test_std, "Reduced")

# 合并并展示最终报表
final_report <- rbind(report_full, report_reduced)
print(final_report)

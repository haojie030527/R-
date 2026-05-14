# ---------------------------------------------------------
# 1. 加载必要的库
# ---------------------------------------------------------
library(ggplot2)
library(pROC)
library(dplyr)
library(purrr)
library(rms)
library(caret)

get_learning_curve_point <- function(frac, d_train, d_test, v_list) {
  # 1. 随机抽取指定比例的训练集
  set.seed(2026 + frac * 100) 
  n <- nrow(d_train)
  sample_idx <- sample(1:n, size = floor(frac * n))
  sub_train <- d_train[sample_idx, ]
  
  # 2. 基础模型：GBM
  m_gbm <- train(是否 ~ ., data = sub_train[, c("是否", v_list)], 
                 method = "gbm", 
                 trControl = trainControl(method = "none"), 
                 tuneGrid = expand.grid(interaction.depth = 1, n.trees = 100, 
                                        shrinkage = 0.1, n.minobsinnode = 20),
                 verbose = FALSE)
  
  # 3. 基础模型：LR_RCS (核心修复点)
  # 强制将 datadist 分配到全局环境，确保 rms 能够识别 [cite: 401-403]
  dd_sub <- datadist(sub_train)
  assign("dd_sub_global", dd_sub, envir = .GlobalEnv)
  options(datadist = 'dd_sub_global')
  
  f_rcs <- as.formula(paste("是否 ~ rcs(碱性磷酸酶, 3) +", 
                            paste(setdiff(v_list, "碱性磷酸酶"), collapse = " + ")))
  m_lr <- lrm(f_rcs, data = sub_train, x = TRUE, y = TRUE)
  
  # 4. 构造 Stacking 概率特征 [cite: 9-11, 55-58]
  p_gbm_tr <- predict(m_gbm, sub_train, type = "prob")$Yes
  p_lr_tr  <- as.numeric(1 / (1 + exp(-predict(m_lr, sub_train))))
  
  stack_sub <- sub_train
  stack_sub$gbm_prob <- p_gbm_tr
  stack_sub$lr_prob  <- p_lr_tr
  
  # 5. 训练元模型 (重新设置 datadist 以包含新特征)
  dd_meta <- datadist(stack_sub)
  assign("dd_meta_global", dd_meta, envir = .GlobalEnv)
  options(datadist = 'dd_meta_global')
  
  m_meta <- lrm(是否 ~ rcs(gbm_prob, 3) + rcs(lr_prob, 3), data = stack_sub)
  
  # 6. 计算性能指标 [cite: 113, 118]
  # 训练集 AUC
  tr_final_p <- as.numeric(1 / (1 + exp(-predict(m_meta, stack_sub))))
  auc_tr <- as.numeric(auc(roc(sub_train$是否, tr_final_p, quiet = TRUE)))
  
  # 验证集 AUC (使用之前保存的模型 [cite: 25, 31])
  p_gbm_te <- predict(m_gbm, d_test, type = "prob")$Yes
  p_lr_te  <- as.numeric(1 / (1 + exp(-predict(m_lr, d_test))))
  stack_test <- d_test
  stack_test$gbm_prob <- p_gbm_te
  stack_test$lr_prob  <- p_lr_te
  te_final_p <- as.numeric(1 / (1 + exp(-predict(m_meta, stack_test))))
  auc_te <- as.numeric(auc(roc(d_test$是否, te_final_p, quiet = TRUE)))
  
  # 清理全局环境中的临时对象（可选）
  # rm(dd_sub_global, dd_meta_global, envir = .GlobalEnv)
  
  return(data.frame(SampleSize = floor(frac * n), Train_AUC = auc_tr, Test_AUC = auc_te))
}

# ---------------------------------------------------------
# 3. 执行梯度训练 (从 20% 到 100% 的样本量)
# ---------------------------------------------------------
fractions <- seq(0.2, 1.0, by = 0.1)
# vars_full 为你定义的 9 个变量 [cite: 471]
learning_results <- map_df(fractions, ~get_learning_curve_point(.x, train_std, test_std, vars_full))

# ---------------------------------------------------------
# 4. 可视化
# ---------------------------------------------------------
plot_data <- learning_results %>%
  tidyr::pivot_longer(cols = c(Train_AUC, Test_AUC), names_to = "Dataset", values_to = "AUC")

ggplot(plot_data, aes(x = SampleSize, y = AUC, color = Dataset, group = Dataset)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_color_manual(values = c("Train_AUC" = "#2E59A7", "Test_AUC" = "#E03C39"),
                     labels = c("Training Set", "Validation Set")) +
  theme_bw() +
  labs(title = "Stacking Ensemble Model Learning Curve",
       subtitle = "Evaluation of Bias-Variance Tradeoff (GDM sPTB Research)",
       x = "Number of Training Samples",
       y = "AUC Performance") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "bottom") +
  ylim(0, 1.0)
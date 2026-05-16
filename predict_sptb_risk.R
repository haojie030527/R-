# 加载模型包
model_bundle <- readRDS("final_model_bundle.rds")

# 定义一个预测函数，输入一个新患者的原始数据框（变量名和顺序与训练时相同）
predict_sptb_risk <- function(new_data, model_bundle) {
  # 1. 标准化新数据（使用保存的 preProcParams）
  library(caret)
  new_data_std <- predict(model_bundle$preProcParams, new_data)
  
  # 2. 获取 GBM 和 LR 的预测概率
  p_gbm <- predict(model_bundle$gbm_model, new_data_std, type = "prob")[, "Yes"]
  p_lr  <- as.numeric(1 / (1 + exp(-predict(model_bundle$lr_model, new_data_std))))
  
  # 3. 准备元学习器的输入数据框
  input_meta <- new_data_std
  input_meta$gbm_prob <- p_gbm
  input_meta$lr_prob  <- p_lr
  
  # 4. 元学习器预测并计算最终风险概率
  lp <- predict(model_bundle$ensemble_model, input_meta)   # 线性预测值（logit）
  risk <- 1 / (1 + exp(-lp))
  return(risk)
}

# 使用示例：
# new_patient <- data.frame(碱性磷酸酶 = 120, 中淋 = 2.5, 尿酸肌酐 = 7.0, ...)
# risk_score <- predict_sptb_risk(new_patient, model_bundle)
# print(risk_score)
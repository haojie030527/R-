# 提取标准化时使用的均值和标准差 (用于后续反向映射)
means_orig <- preProcParams$mean
sds_orig   <- preProcParams$std


# 定义名称映射表 (左边是代码原名，右边是你想展示的名字)
# 定义名称映射表 (左边是代码原名，右边是你想展示的名字)
name_map_en <- c(
  "碱性磷酸酶" = "ALP (U/L)",
  "中淋" = "NLR",
  "尿酸肌酐" = "UA/Cr",
  "空腹" = "FBG (mmol/L)",
  "空腹1h" = "1h PG",
  "白细胞" = "WBC (10^9/L)",
  "血红蛋白" = "Hb (g/L)",
  "纤维蛋白原" = "FIB (g/L)",
  "谷丙转氨酶" = "ALT (U/L)"
)
library(rms)

# 2. 【核心步骤】直接修改模型对象内部的 labels
# models_full$LR_RCS 是你的标准化模型
# 我们直接把英文标签注入到它的设计矩阵信息中
attr(models_full$LR_RCS$Design, "label")[names(name_map_en)] <- name_map_en

# 3. 重新配置环境（指向原始数据的尺度，确保刻度是原数值）
# 注意：dd_orig 必须基于 vars_full 的原始数据
dd_orig <- datadist(train_data[, vars_full])
options(datadist = "dd_orig")

# 4. 绘图
# 现在 nomogram 会读取模型内部被我们刚刚修改过的 Design 标签
nomo_obj <- nomogram(models_full$LR_RCS, 
                      fun = plogis, 
                      fun.at = c(0.1, 0.3, 0.5, 0.7, 0.9),
                      funlabel = "Risk of Preterm Birth",
                      lp = FALSE)

# nomogram 对象本质上是一个列表，其 names 属性就是显示的变量名
curr_names <- names(nomo_obj)
new_names <- curr_names

for(i in seq_along(curr_names)) {
  # 如果当前的名称在我们的映射表里，就替换它
  if(curr_names[i] %in% names(name_map_en)) {
    new_names[i] <- name_map_en[curr_names[i]]
  }
}

# 将替换后的名称重新赋回对象
names(nomo_obj) <- new_names

# 5. 绘图
plot(nomo_obj, xfrac = 0.35, cex.var = 0.8, cex.axis = 0.7)


library(caret)
library(ggplot2)
library(gbm)
# 1. 提取 GBM 的重要性
imp_gbm <- varImp(models_full$GBM, scale = TRUE)
df_imp <- as.data.frame(imp_gbm$importance)
df_imp$Feature <- rownames(df_imp)

# 2. 映射英文名
df_imp$Feature_EN <- ifelse(df_imp$Feature %in% names(name_map_en), 
                            name_map_en[df_imp$Feature], 
                            df_imp$Feature)

# 3. 绘图
ggplot(df_imp, aes(x = reorder(Feature_EN, Overall), y = Overall)) +
  geom_bar(stat = "identity", fill = "#2E59A7", width = 0.7) +
  coord_flip() +
  theme_bw() +
  labs(title = "Variable Importance (GBM Model)",
       x = "Clinical Features",
       y = "Relative Importance (%)") +
  theme(axis.text.y = element_text(size = 10))



library(caret)
library(ggplot2)

# 1. 提取全量组 RF 的变量重要性
# caret 的 varImp 会自动处理随机森林的重要性计算
imp_rf <- varImp(models_full$RF, scale = TRUE)
df_imp_rf <- as.data.frame(imp_rf$importance)
df_imp_rf$Feature <- rownames(df_imp_rf)

# 2. 映射英文名 (使用你之前定义的 name_map_en)
df_imp_rf$Feature_EN <- ifelse(df_imp_rf$Feature %in% names(name_map_en), 
                               name_map_en[df_imp_rf$Feature], 
                               df_imp_rf$Feature)

# 3. 绘图 (Lollipop 图风格，比柱状图更具高级感)
ggplot(df_imp_rf, aes(x = reorder(Feature_EN, Overall), y = Overall)) +
  geom_segment(aes(xend = Feature_EN, yend = 0), color = "#377EB8", size = 1) +
  geom_point(color = "#377EB8", size = 4) +
  coord_flip() +
  theme_bw() +
  labs(title = "Variable Importance (Random Forest Model)",
       subtitle = "Full Model with Standardized Parameters",
       x = "Clinical Predictors",
       y = "Mean Decrease Gini (Scaled 0-100)") +
  theme(
    axis.text.y = element_text(size = 10, face = "bold"),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )


library(ggplot2)
library(dplyr)
library(tidyr)

# 提取各模型的预测概率 (确保 test_std 包含 '是否' 变量)
prob_data <- data.frame(
  Actual = test_std$是否,
  SVM = predict(models_full$SVM, test_std, type = "prob")$Yes,
  ANN = predict(models_full$ANN, test_std, type = "prob")$Yes,
  KNN = predict(models_full$KNN, test_std, type = "prob")$Yes
)

# 将实际标签转换为更专业的英文名
prob_data$Outcome <- ifelse(prob_data$Actual == "Yes", "Preterm Birth", "Term Birth")
prob_data$Outcome <- factor(prob_data$Outcome, levels = c("Term Birth", "Preterm Birth"))


ggplot(prob_data, aes(x = SVM, fill = Outcome)) +
  geom_density(alpha = 0.5, color = "white") +
  scale_fill_manual(values = c("Term Birth" = "#2E59A7", "Preterm Birth" = "#E03C39")) +
  theme_classic() +
  labs(title = "Probability Distribution: SVM Full Model",
       x = "Predicted Probability of Preterm Birth",
       y = "Density") +
  theme(legend.position = "top")

ggplot(prob_data, aes(x = ANN, fill = Outcome)) +
  geom_density(alpha = 0.5, color = "white") +
  scale_fill_manual(values = c("Term Birth" = "#2E59A7", "Preterm Birth" = "#E03C39")) +
  theme_classic() +
  labs(title = "Probability Distribution: ANN Full Model",
       x = "Predicted Probability of Preterm Birth",
       y = "Density") +
  theme(legend.position = "top")

ggplot(prob_data, aes(x = KNN, fill = Outcome)) +
  geom_density(alpha = 0.5, color = "white") +
  scale_fill_manual(values = c("Term Birth" = "#2E59A7", "Preterm Birth" = "#E03C39")) +
  theme_classic() +
  labs(title = "Probability Distribution: KNN Full Model",
       x = "Predicted Probability of Preterm Birth",
       y = "Density") +
  theme(legend.position = "top")
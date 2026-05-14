library(fastshap)
library(ggplot2)
library(reshape2)

library(fastshap)
library(ggplot2)
library(reshape2)
library(dplyr)

# ==========================================
# 1. 确保变量列表纯净 (vars_full 包含你的 9 个变量)
# ==========================================
# 检查 vars_full 是否包含：碱性磷酸酶, 中淋, 尿酸肌酐, 空腹, 空腹1h, 白细胞, 血红蛋白, 纤维蛋白原, 谷丙转氨酶
print(vars_full) 

# ==========================================
# 2. 修正预测包装器 (确保输入输出不含糖化/尿素)
# ==========================================
ensemble_pred_wrapper_clean <- function(object, newdata) {
  # 这里的 newdata 必须只包含训练时的 9 个变量
  p_gbm <- predict(models_full$GBM, newdata, type = "prob")[, "Yes"]
  p_lr  <- as.numeric(1 / (1 + exp(-predict(models_full$LR_RCS, newdata))))
  
  # 构造元模型输入
  df_meta <- newdata
  df_meta$gbm_prob <- p_gbm
  df_meta$lr_prob  <- p_lr
  
  # 输出集成模型最终概率 (ensemble_model 是你的 Stacking 元模型)
  p_ensemble <- as.numeric(1 / (1 + exp(-predict(object, df_meta))))
  return(p_ensemble)
}

# ==========================================
# 3. 准备解释用的特征矩阵 (严格限定 9 变量)
# ==========================================
# 仅提取 test_std 中属于全量组的列
X_explain <- test_std[, vars_full] 

# ==========================================
# 4. 计算 SHAP 值
# ==========================================
set.seed(2026)
shap_values <- fastshap::explain(
  object = ensemble_model,            # 你的 Stacking 集成模型
  X = as.matrix(X_explain),           # X 必须是矩阵或数据框
  pred_wrapper = ensemble_pred_wrapper_clean, # 使用之前定义的那个清爽版包装器
  nsim = 50,                          # 模拟次数
  adjust = TRUE                       # 确保 SHAP 值加和等于预测概率差
)
# ==========================================
# 5. 全局重要性图 (条形图 - 映射英文名)
# ==========================================
shap_imp <- data.frame(
  Variable = colnames(shap_values),
  Importance = colMeans(abs(shap_values))
)

# 映射英文名 (使用你之前的 name_map_en)
shap_imp$Variable_EN <- name_map_en[shap_imp$Variable]

ggplot(shap_imp, aes(x = reorder(Variable_EN, Importance), y = Importance)) +
  geom_col(fill = "#2E59A7", width = 0.7) +
  coord_flip() +
  theme_bw() +
  labs(
    title = "Ensemble Model Global Importance (SHAP)",
    subtitle = "Based on Full Model (9 Predictors)",
    x = "Clinical Features",
    y = "Mean |SHAP Value|"
  ) +
  theme(plot.title = element_text(face = "bold"))

# ==========================================
# 6. 决策逻辑图 (蜂群图 - 映射英文名)
# ==========================================
# 构造绘图长数据
shap_df <- as.data.frame(shap_values)
colnames(shap_df) <- name_map_en[colnames(shap_df)] # 直接把列名转为英文
shap_df$ID <- 1:nrow(shap_df)

shap_long <- melt(shap_df, id.vars = "ID", variable.name = "Variable_EN", value.name = "SHAP_Value")

# 提取标准化后的特征值用于着色
X_val_matrix <- as.matrix(X_explain)
colnames(X_val_matrix) <- name_map_en[colnames(X_val_matrix)]
X_long <- melt(X_val_matrix)
colnames(X_long) <- c("ID", "Variable_EN", "Feature_Value")

# 合并
shap_long$Feature_Value <- X_long$Feature_Value

# 绘图
# ==========================================
# 优化版 SHAP 蜂群图 (高对比度英文版)
# ==========================================
library(ggplot2)
install.packages("viridis")
library(viridis) # 必须加载这个包来调用 magma 颜色

# 1. 确保你的数据 shap_long 已经准备好
# 2. 绘图
ggplot(shap_long, aes(x = Variable_EN, y = SHAP_Value, color = Feature_Value)) +
  # 使用 geom_jitter 模拟蜂群效果
  geom_jitter(alpha = 0.8, width = 0.25, size = 1.5) +
  
  # 【核心修改】：使用 magma 调色板还原图片中的 紫-红-橙-黄 配色
  # option = "A" 即为 magma
  scale_color_viridis_c(
    option = "A", 
    name = "Feature value",
    breaks = c(min(shap_long$Feature_Value), max(shap_long$Feature_Value)),
    labels = c("Low", "High")
  ) +
  
  # 添加零位参考线
  geom_hline(yintercept = 0, linetype = "solid", color = "gray50", size = 0.5) +
  
  # 坐标轴翻转
  coord_flip() +
  
  # 模仿图片中的纯净主题
  theme_bw() + 
  labs(
    title = "SHAP Summary Plot: Ensemble Model (Original Scale)",
    subtitle = "Color represents original feature values",
    x = "",
    y = "SHAP value"
  ) +
  theme(
    plot.title = element_text(hjust = 0, face = "plain", size = 14),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "gray90"),
    panel.grid.major.y = element_blank(), # 图片中横向没有主网格线
    axis.line = element_line(color = "black"),
    legend.position = "right"
  )

# 保存高清图
ggsave("SHAP_Magma_Style.png", width = 10, height = 7, dpi = 300)
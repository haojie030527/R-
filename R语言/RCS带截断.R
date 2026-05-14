library(rms)
# 1. 使用原始数据框 df (非标准化的那个)
dd <- datadist(df)
options(datadist = 'dd')

# 2. 拟合一个只包含 ALP 的逻辑回归模型 (加上 rcs)
fit_rcs_raw <- lrm(是否 ~ rcs(碱性磷酸酶, 3), data = df)

# 3. 绘图
ggplot(Predict(fit_rcs_raw, 碱性磷酸酶, fun=exp), 
       adj.obj = list(碱性磷酸酶 = median(df$碱性磷酸酶))) +
  labs(x = "ALP (U/L) 原始数值", y = "OR值", title = "ALP 与早产风险的非线性关系")

library(rms)

# 1. 使用原始数据 df (非标准化) 重新设定环境
dd <- datadist(df)
options(datadist = 'dd')

# 2. 拟合多因素模型 (调整所有 9 个变量)
# 注意：只给 ALP 加 rcs，其他变量线性进入，以保证 ALP 是主角
fit_adj_rcs <- lrm(是否 ~ rcs(碱性磷酸酶, 3) + 中淋 + 尿酸肌酐 + 
                     空腹 + 空腹1h + 白细胞 + 血红蛋白 + 
                     纤维蛋白原 + 谷丙转氨酶, data = df)

# 3. 绘图：Predict 函数会自动将其他变量固定在均值/中位数
pred_alp <- Predict(fit_adj_rcs, 碱性磷酸酶, fun=exp, conf.int = 0.95)

# 4. 绘图 (ggplot 风格)
ggplot(pred_alp) +
  theme_minimal() +
  labs(x = "碱性磷酸酶 (ALP, U/L)", 
       y = "Adjusted OR (95% CI)",
       title = "调整其他变量后的 ALP 与早产风险非线性关系")


为了确保代码的完整性和逻辑严谨性，我将代码分为两部分：数据准备与模型拟合、RCS曲线绘制（含转折点与OR转换）。

这段代码直接使用原始数据（非标准化）进行拟合，这样横坐标就是真实的临床数值，纵轴会自动转换为临床医生最易理解的 Odds Ratio (OR)。

完整 R 代码实现
R
# 1. 加载必要的库
library(rms)
library(ggplot2)

# 2. 设置绘图环境与数据分布 (假设您的原始数据框名为 df)
# 注意：这里务必使用包含原始数值（非标准化）的数据框
dd <- datadist(df)
options(datadist = 'dd')

# 3. 拟合调整后的多因素 Logistic 回归模型
# 仅对 ALP 使用 rcs，其他变量线性进入以作为调整因素（Covariates）
# 这里的变量名请替换为您数据框中实际的名称
fit_adj_rcs <- lrm(是否 ~ rcs(碱性磷酸酶, 3) + 中淋 + 尿酸肌酐 + 
                     空腹 + 空腹1h + 白细胞 + 血红蛋白 + 
                     纤维蛋白原 + 谷丙转氨酶, 
                   data = df, x=TRUE, y=TRUE)

# 2. 获取预测数据
# 1. 强制提取转折点并检查 (如果这里报错，说明模型中没加rcs)
# 修正提取逻辑，确保能抓到数值
knots_val <- as.numeric(fit_adj_rcs$Design$parms$碱性磷酸酶)
print(paste("当前识别到的转折点为:", paste(knots_val, collapse = ", "))) 

# 2. 获取预测数据 (OR 尺度)
pred_alp <- Predict(fit_adj_rcs, 碱性磷酸酶, fun=exp)

# 3. 绘图
ggplot(data = pred_alp) +
  # 阴影层
  geom_ribbon(aes(x = 碱性磷酸酶, ymin = lower, ymax = upper), 
              fill = "#0072B2", alpha = 0.15) +
  # 核心曲线层
  geom_line(aes(x = 碱性磷酸酶, y = yhat), 
            color = "#0072B2", size = 1.2) +
  # OR = 1 参考线
  geom_hline(yintercept = 1, linetype = "dashed", color = "red", size = 0.8) +
  
  # --- 重新绘制转折点线条 (确保在最上层) ---
  geom_vline(xintercept = knots_val, linetype = "dotted", color = "black", size = 0.8) +
  
  # --- 重新绘制文字标注 (将 y 坐标设为 y 轴的中段，确保可见) ---
  annotate("text", 
           x = knots_val, 
           y = median(pred_alp$yhat), # 放在曲线高度的中位数附近
           label = paste0("Knot: ", round(knots_val, 1)), 
           angle = 90, 
           vjust = -0.5, 
           size = 4, 
           color = "black",
           fontface = "bold") +
  
  theme_bw() +
  labs(
    title = "Adjusted RCS Curve with Knots",
    x = "ALP (U/L)",
    y = "Adjusted Odds Ratio (95% CI)"
  ) +
  # 移除之前的 coord_cartesian 限制，让坐标轴自动适应
  theme(
    axis.title = element_text(size = 12, face = "bold"),
    plot.title = element_text(hjust = 0.5)
  )
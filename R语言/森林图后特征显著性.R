原数据构建森林图：（不考虑）
# 1. 安装并加载必要的包
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, rms, forestploter, broom)

# 2. 构建多因素 Logistic 回归模型 (以您的 9 个变量为例)
# 注意：这里使用原始数据，不要标准化，这样 OR 值才有临床意义
fit <- glm(是否 ~ 空腹+空腹1h+碱性磷酸酶+血红蛋白+纤维蛋白原+谷丙转氨酶+尿酸肌酐+中淋+白细胞, 
           data = train_data, family = binomial())

# 3. 提取 OR 值、置信区间和 P 值
model_results <- tidy(fit, exponentiate = TRUE, conf.int = TRUE) %>%
  filter(term != "(Intercept)") %>%
  select(variable = term, 
         OR = estimate, 
         lower = conf.low, 
         upper = conf.high, 
         p.value = p.value)

# 4. 整理绘图格式
plot_data <- model_results %>%
  mutate(across(where(is.numeric), ~round(., 3))) %>%
  mutate(
    `OR (95% CI)` = paste0(OR, " (", lower, "-", upper, ")"),
    # 创建一个空列用于画森林图的“点线”
    ` ` = paste(rep(" ", 20), collapse = " ") 
  )


# 5. 设置绘图参数
# 如果 P < 0.001 显示为 <0.001
plot_data$p.value <- ifelse(plot_data$p.value < 0.001, "<0.001", as.character(plot_data$p.value))

# 整理列顺序
final_df <- plot_data %>% 
  select(variable, `OR (95% CI)`, p.value, ` `, OR, lower, upper)

# 6. 开始绘制
res <- forest(
  data = final_df[, 1:4], # 前 4 列作为表格显示
  est = final_df$OR,
  lower = final_df$lower, 
  upper = final_df$upper,
  sizes = 0.5,            # 点的大小
  ci_column = 4,          # 森林图画在第几列
  ref_line = 1,           # 无效线设在 1.0
  xlim = c(0, 5),         # 根据你的 OR 值范围调整
  ticks_at = c(0, 1, 2, 3, 4, 5),
  theme = forest_theme(
    base_size = 10,
    # 设置点和线的颜色
    core = list(fg_col = "black"),
    ci_col = "#2c7fb8",    # 蓝色线条，比黑色更显高级
    ref_line_col = "red",  # 无效线设为红色虚线
    ref_line_lty = "dashed"
  )
)

# 7. 查看并保存图
print(res)
ggsave("Forest_Plot_Final.png", res, width = 10, height = 6, dpi = 300)



基于标准化后wald有显著性的特征贡献图：
library(dplyr)
library(ggplot2)

# 1. 提取 Wald 检验的卡方值（强度）
wald_tab <- as.data.frame(anova(fit_multi))
wald_tab$Variable <- rownames(wald_tab)

# 2. 过滤掉汇总行，只保留原始变量名
# 注意：如果是 rcs 项，anova 会产生多行，我们取每个变量的总 Chi-Square
plot_df <- wald_tab %>%
  filter(!grepl("TOTAL|ERROR|Nonlinear", Variable)) %>%
  select(Variable, Chi2 = `Chi-Square`, P = `P`)

# 3. 提取模型系数（方向）并进行匹配
# 从 lrm 模型中提取系数，系数 > 0 为危险，系数 < 0 为保护
coef_all <- coef(fit_multi)

# 定义一个函数来判断变量的真实方向
# 对于 rcs 变量，我们取其主项（第一项）的系数符号
get_direction <- function(var_name) {
  # 在系数名中寻找匹配项
  idx <- grep(var_name, names(coef_all))[1] # 找第一个匹配的系数
  if (is.na(idx)) return(NA)
  if (coef_all[idx] > 0) return("危险因素 (Risk)") 
  else return("保护因素 (Protective)")
}

# 将方向应用到绘图数据中
plot_df$Direction <- sapply(plot_df$Variable, get_direction)

# 4. 绘图：修正方向后的特征贡献图
ggplot(plot_df, aes(x = reorder(Variable, Chi2), y = Chi2, fill = Direction)) +
  geom_bar(stat = "identity", width = 0.7, color = "white") +
  coord_flip() +
  # 统一颜色：红色代表危险，蓝色代表保护
  scale_fill_manual(values = c("危险因素 (Risk)" = "#d73027", 
                               "保护因素 (Protective)" = "#4575b4")) +
  labs(
    title = "独立预测因子的方向与强度分析",
    subtitle = "基于标准化数据的 Wald 统计量 (Chi-Square)",
    x = "临床指标",
    y = "Wald Chi-Square (贡献强度)",
    fill = "影响方向"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 10, face = "bold"),
    legend.position = "bottom"
  )


# 过滤后的特征贡献图数据
plot_df_significant <- plot_df %>%
  filter(P < 0.05) # 仅保留显著的变量

# 绘图
ggplot(plot_df_significant, aes(y = reorder(Variable, Chi2), x = Chi2, fill = Direction)) +
  geom_bar(stat = "identity", width = 0.6,color = "white") +
  coord_flip() +
  # 统一颜色：红色代表危险，蓝色代表保护
  scale_fill_manual(values = c("危险因素 (Risk)" = "#d73027", 
                               "保护因素 (Protective)" = "#4575b4")) +
  # ... 其他绘图代码保持不变 ...
  labs(
    title = "展示 Wald 检验显著的独立危险因素 (P < 0.05)",
       y = "临床指标",
       x = "Wald Chi-Square (贡献强度)",
       fill = "影响方向"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 10, face = "bold"),
    legend.position = "bottom"
  )

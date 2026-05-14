# 加载必要的包
library(PRROC)
library(ggplot2)

# 提取验证集的真实标签（数值：1 = sPTB, 0 = non-sPTB）
y_test_num <- ifelse(test_std$是否 == "Yes", 1, 0)

# 使用 PRROC 计算 PR 曲线数据
pr_ensemble <- pr.curve(scores.class0 = test_eval$p_val[y_test_num == 1],
                        scores.class1 = test_eval$p_val[y_test_num == 0],
                        curve = TRUE)

# 提取 AUPRC
auprc_ensemble <- pr_ensemble$auc.integral
cat("集成模型 AUPRC:", round(auprc_ensemble, 3), "\n")

# 使用 ggplot2 绘制 PR 曲线
df_pr_ensemble <- data.frame(
  Recall = pr_ensemble$curve[, 1],
  Precision = pr_ensemble$curve[, 2]
)

p_ensemble <- ggplot(df_pr_ensemble, aes(x = Recall, y = Precision)) +
  geom_line(color = "#2c7fb8", size = 1.2) +
  labs(x = "Recall (Sensitivity)", y = "Precision (PPV)",
       title = paste0("Stacking Ensemble Model - PR Curve (AUPRC = ", round(auprc_ensemble, 3), ")")) +
  theme_bw(base_size = 14) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "gray90", linetype = "dashed"))

print(p_ensemble)

# 保存为高分辨率图片
ggsave("PR_curve_ensemble.png", p_ensemble, width = 6, height = 6, dpi = 300)
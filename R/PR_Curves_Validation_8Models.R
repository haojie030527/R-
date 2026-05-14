# 安装
install.packages("PRROC")

# 加载
library(PRROC)
# ==========================================
# 加载所需包
# ==========================================
library(PRROC)
library(pROC)      # 用于roc函数（但PR曲线用PRROC）
library(ggplot2)   # 可选，若想用ggplot美化

汇总：
# 真实标签（1=Yes, 0=No）
y_test <- ifelse(test_std$是否 == "Yes", 1, 0)

# 获取预测概率的函数
get_pred_prob <- function(model, name, data) {
  if (name == "LR_RCS") {
    lp <- predict(model, data, type = "lp")
    prob <- 1 / (1 + exp(-lp))
  } else {
    prob <- predict(model, data, type = "prob")$Yes
  }
  return(prob)
}

model_names <- names(models_full)
pr_list <- list()
auprc_list <- numeric(length(model_names))
names(auprc_list) <- model_names

for (i in seq_along(model_names)) {
  name <- model_names[i]
  cat("Processing", name, "...\n")
  prob <- get_pred_prob(models_full[[name]], name, test_std)
  pr <- pr.curve(scores.class0 = prob[y_test == 1],
                 scores.class1 = prob[y_test == 0],
                 curve = TRUE)
  pr_list[[name]] <- pr
  auprc_list[name] <- pr$auc.integral
}

# 打印 AUPRC
print(auprc_list)

# 颜色
colors <- c("GBM"="#1f77b4","RF"="#ff7f0e","SVM"="#2ca02c","KNN"="#d62728",
            "ANN"="#9467bd","DT"="#8c564b","NB"="#e377c2","LR_RCS"="#7f7f7f")

# 自定义图例标签：将 "LR_RCS" 替换为 "LR"
legend_labels <- names(auprc_list)
legend_labels[legend_labels == "LR_RCS"] <- "LR"
legend_labels <- paste0(legend_labels, " (AUPRC=", round(auprc_list, 3), ")")

# 绘图
plot(0, 0, type="n", xlim=c(0,1), ylim=c(0,1),
     xlab="Recall (Sensitivity)", ylab="Precision (PPV)",
     main="Precision-Recall Curves for All Models")
grid(col="lightgray", lty=2)
for (name in model_names) {
  lines(pr_list[[name]]$curve[,1], pr_list[[name]]$curve[,2],
        col=colors[name], lwd=2)
}
legend("bottomleft", legend = legend_labels,
       col = colors[model_names], lwd = 2, cex = 0.8, bty = "n")

单独：
# ==========================================
# 加载必要的包
# ==========================================
library(PRROC)
library(ggplot2)
library(purrr)   # 便于列表操作

# ==========================================
# 前提：您已经运行过 get_pred_prob 函数和 pr_list 计算
# 如果 pr_list 不存在，请先执行以下循环（假设已有 models_full 和 test_std）
# ==========================================
if (!exists("pr_list")) {
  y_test <- ifelse(test_std$是否 == "Yes", 1, 0)
  model_names <- names(models_full)
  pr_list <- list()
  auprc_vec <- c()
  for (name in model_names) {
    prob <- get_pred_prob(models_full[[name]], name, test_std)
    pr <- pr.curve(scores.class0 = prob[y_test == 1],
                   scores.class1 = prob[y_test == 0],
                   curve = TRUE)
    pr_list[[name]] <- pr
    auprc_vec[name] <- pr$auc.integral
  }
}

# ==========================================
# 将 PR 曲线数据转换为数据框（用于 ggplot）
# ==========================================
df_pr <- map_dfr(names(pr_list), function(name) {
  pr <- pr_list[[name]]
  # 提取曲线上的点 (Recall, Precision)
  data.frame(
    Model = ifelse(name == "LR_RCS", "LR", name),
    Recall = pr$curve[, 1],
    Precision = pr$curve[, 2],
    AUPRC = round(auprc_vec[name], 3)
  )
})

# 创建带 AUPRC 的模型标签
df_pr$Model_label <- paste0(df_pr$Model, " (AUPRC = ", df_pr$AUPRC, ")")

# ==========================================
# 绘制分面 PR 曲线（每个模型一个子图，4列）
# ==========================================
p <- ggplot(df_pr, aes(x = Recall, y = Precision)) +
  geom_line(size = 1.2, color = "#2c7fb8") +
  facet_wrap(~ Model_label, ncol = 4, scales = "fixed") +
  labs(x = "Recall (Sensitivity)", y = "Precision (PPV)",
       title = "Precision-Recall Curves for All Models (Testing Set)") +
  theme_bw(base_size = 14) +
  theme(
    strip.background = element_rect(fill = "#f0f0f0", color = NA),
    strip.text = element_text(face = "bold", size = 12),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90", linetype = "dashed")
  )

# 显示图形
print(p)

# ==========================================
# 保存为高分辨率图片（600 dpi）
# ==========================================
ggsave("PR_curves_cleaned.png", p, width = 10, height = 8, dpi = 600)
ggsave("PR_curves_cleaned.pdf", p, width = 10, height = 8)   # 矢量格式用于论文
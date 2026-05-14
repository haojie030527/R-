library(pROC)
library(ggplot2)

循环每个模型：
# 循环处理 8 个模型
for (n in names(models_full)) {
  
  # 1. 设置保存文件名
  png_filename <- paste0("ROC_Full_Vars_", n, ".png")
  png(filename = png_filename, width = 1200, height = 1200, res = 200)
  
  # 2. 提取模型
  model <- models_full[[n]]
  
  # 3. 稳健地提取概率值
  if (n == "LR_RCS") {
    # 针对 rms 包的 lrm 模型：使用 type = "lp" 或默认，然后手动转概率
    # 注意：lrm 的 predict 默认返回的就是 linear.predictors
    tr_lp <- predict(model, train_std) 
    te_lp <- predict(model, test_std)
    tr_p  <- 1 / (1 + exp(-tr_lp))
    te_p  <- 1 / (1 + exp(-te_lp))
  } else {
    # 针对 caret 训练的其他机器学习模型
    tr_p <- predict(model, train_std, type = "prob")$Yes
    te_p <- predict(model, test_std, type = "prob")$Yes
  }
  
  # 4. 计算 ROC 对象
  roc_tr <- roc(train_std$是否, tr_p, quiet = TRUE)
  roc_te <- roc(test_std$是否, te_p, quiet = TRUE)
  
  # 5. 绘图美化
  par(mar = c(5, 5, 4, 2))
  plot(roc_tr, 
       col = "#2E59A7",           # 训练集：蓝色实线
       lwd = 3, 
       main = paste("ROC Curve for", n),
       xlab = "1 - Specificity",
       ylab = "Sensitivity",
       cex.main = 1.5, cex.lab = 1.2,
       legacy.axes = TRUE)       # 确保 X 轴为 0 到 1 正向
  
  lines(roc_te, 
        col = "#D7191C",          # 验证集：红色虚线
        lwd = 3, 
        lty = 2)
  
  # 6. 添加 AUC 标注
  legend("bottomright", 
         legend = c(paste0("Training Set (AUC: ", round(auc(roc_tr), 3), ")"),
                    paste0("Testing Set (AUC: ", round(auc(roc_te), 3), ")")),
         col = c("#2E59A7", "#D7191C"), 
         lwd = 3, 
         lty = c(1, 2), 
         bty = "n", 
         cex = 1.2)
  
  dev.off()
  cat("已成功导出:", png_filename, "\n")
}

验证集与训练集分开：
library(pROC)

# 定义颜色板（8种对比鲜明的颜色）
plot_colors <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", "#A65628", "#F781BF")

# 定义一个绘制汇总 ROC 的函数
plot_combined_roc <- function(d_set, d_name) {
  # 1. 开启图片设备
  png(filename = paste0("Combined_ROC_", d_name, ".png"), width = 1400, height = 1400, res = 200)
  par(mar = c(5, 5, 4, 2))
  
  # 2. 初始化画板（先画第一个模型，这里选 GBM）
  first_model_name <- names(models_full)[1]
  m1 <- models_full[[first_model_name]]
  
  # 获取概率
  if (first_model_name == "LR_RCS") {
    p1 <- 1 / (1 + exp(-predict(m1, d_set)))
  } else {
    p1 <- predict(m1, d_set, type = "prob")$Yes
  }
  
  roc_obj1 <- roc(d_set$是否, p1, quiet = TRUE)
  
  plot(roc_obj1, col = plot_colors[1], lwd = 2.5, 
       main = paste("Comparison of 8 Models on", d_name, "Set"),
       xlab = "1 - Specificity", ylab = "Sensitivity",
       legacy.axes = TRUE)
  
  # 3. 循环添加其余 7 个模型的曲线
  auc_labels <- c(paste0(first_model_name, " (AUC: ", round(auc(roc_obj1), 3), ")"))
  
  for (i in 2:length(names(models_full))) {
    n <- names(models_full)[i]
    m <- models_full[[n]]
    
    if (n == "LR_RCS") {
      p <- 1 / (1 + exp(-predict(m, d_set)))
    } else {
      p <- predict(m, d_set, type = "prob")$Yes
    }
    
    roc_obj <- roc(d_set$是否, p, quiet = TRUE)
    lines(roc_obj, col = plot_colors[i], lwd = 2)
    
    # 记录 AUC 用于图例
    auc_labels <- c(auc_labels, paste0(n, " (AUC: ", round(auc(roc_obj), 3), ")"))
  }
  
  # 4. 添加图例
  legend("bottomright", legend = auc_labels, col = plot_colors, 
         lwd = 2, bty = "n", cex = 0.8, title = "Models & AUC")
  
  dev.off()
}

# 执行生成两张汇总图
plot_combined_roc(train_std, "Training")
plot_combined_roc(test_std, "Testing")

cat("汇总 ROC 图片已导出至工作目录：\n1. Combined_ROC_Training.png\n2. Combined_ROC_Validation.png\n")
install.packages("ggDCA")
install.packages("calibration")
library(rms)
library(ggplot2)
library(ggDCA) # 如果没有请 install.packages("ggDCA")
library(reshape2)
library(caret)
# 1. 先安装开发工具包
if (!require("devtools")) install.packages("devtools")

# 2. 从 GitHub 仓库安装 (这是最稳定的版本)
devtools::install_github("yikeshu0611/ggDCA")

# 3. 再次尝试加载
library(ggDCA)
# 统一获取概率的辅助函数
get_prob <- function(model, data, name) {
  if (name == "LR_RCS") {
    lp <- predict(model, data)
    return(1 / (1 + exp(-lp)))
  } else {
    return(predict(model, data, type = "prob")$Yes)
  }
}

决策曲线：
library(dcurves)
library(ggplot2)
library(magrittr) # 提供管道操作符 %>%

# 1. 准备验证集数据框
# 必须包含 0/1 格式的结局变量
dca_df <- data.frame(
  outcome = ifelse(test_std$是否 == "Yes", 1, 0)
)

# 2. 显式提取各模型的预测概率
# 我们选取表现最好的 4 个模型，图面更整洁；如需全部展示，按此格式添加即可
cat("正在提取预测概率...\n")

# GBM
dca_df$GBM <- predict(models_full$GBM, test_std, type = "prob")$Yes

# LR_RCS (注意 rms 包的特殊提取方式)
dca_df$LR_RCS <- as.numeric(1 / (1 + exp(-predict(models_full$LR_RCS, test_std))))


# 3. 计算并绘制 DCA
cat("正在生成决策曲线...\n")

dca(outcome ~ GBM + LR_RCS , 
    data = dca_df,
    thresholds = seq(0, 1, by = 0.01)) %>% # 设定阈值范围
  plot(smooth = TRUE) +
  theme_minimal() +
  labs(
    title = "Decision Curve Analysis: Model Comparison",
    subtitle = "Testing Set Performance",
    x = "Threshold Probability",
    y = "Net Benefit"
  ) +
  scale_color_brewer(palette = "Set1") + # 设置漂亮的颜色
  theme(legend.position = "right")

# 4. 保存
ggsave("DCA_Final_Comparison.png", width = 8, height = 6, dpi = 300)


训练集GBM与LR模型决策曲线：
# 1. 准备验证集数据框
# 必须包含 0/1 格式的结局变量
dca_df <- data.frame(
  outcome = ifelse(train_std$是否 == "Yes", 1, 0)
)

# 2. 显式提取各模型的预测概率
# 我们选取表现最好的 4 个模型，图面更整洁；如需全部展示，按此格式添加即可
cat("正在提取预测概率...\n")

# GBM
dca_df$GBM <- predict(models_full$GBM, train_std, type = "prob")$Yes

# LR_RCS (注意 rms 包的特殊提取方式)
dca2_df$LR_RCS <- as.numeric(1 / (1 + exp(-predict(models_full$LR_RCS, train_std))))


# 3. 计算并绘制 DCA
cat("正在生成决策曲线...\n")

dca(outcome ~ GBM + LR_RCS , 
    data = dca_df,
    thresholds = seq(0, 1, by = 0.01)) %>% # 设定阈值范围
  plot(smooth = TRUE) +
  theme_minimal() +
  labs(
    title = "Decision Curve Analysis: Model Comparison",
    subtitle = "Training Set Performance",
    x = "Threshold Probability",
    y = "Net Benefit"
  ) +
  scale_color_brewer(palette = "Set1") + # 设置漂亮的颜色
  theme(legend.position = "right")

# 4. 保存
ggsave("DCA_Final_trainComparison.png", width = 8, height = 6, dpi = 300)


8种模型
library(dcurves)
library(ggplot2)
library(magrittr)

# 1. 准备 DCA 专用数据框 (基于验证集 test_std)
dca_df <- data.frame(
  outcome = ifelse(test_std$是否 == "Yes", 1, 0)
)

# 2. 循环提取 8 个模型的预测概率
cat("正在处理各模型概率...\n")

for (n in names(models_full)) {
  m <- models_full[[n]]
  
  if (n == "LR_RCS") {
    # 针对 rms 包的 lrm 模型
    lp <- predict(m, test_std)
    dca_df[[n]] <- as.numeric(1 / (1 + exp(-lp)))
  } else {
    # 针对 caret 训练的模型
    dca_df[[n]] <- predict(m, test_std, type = "prob")$Yes
  }
}

# 3. 检查数据框前几行，确保概率提取无误
head(dca_df)

# 定义要展示的模型名称（对应 dca_df 的列名）
model_names <- names(models_full)

# 4. 执行 DCA 计算与绘图
dca_plot <- dca(as.formula(paste("outcome ~", paste(model_names, collapse = " + "))), 
                data = dca_df,
                thresholds = seq(0, 1, by = 0.02)) %>%
  plot(smooth = TRUE) +
  theme_bw() + # 使用经典白底主题
  scale_color_manual(values = c("#000000", "#377EB8", "#4DAF4A", "#984EA3", 
                                "#FF7F00", "#A65628", "#F781BF", "#999999",
                                "#E41A1C", "#666666")) +
  labs(
    title = "Decision Curve Analysis: Testing Set",
    x = "Threshold Probability",
    y = "Net Benefit",
    color = "Prediction Models"
  ) +
  theme(
    legend.position = "right",
    legend.text = element_text(size = 8),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

# 5. 保存高清图片
ggsave("DCA_All_Models_Full_Vars.png", width = 10, height = 7, dpi = 300)

# 直接在窗口展示
print(dca_plot)


训练集决策曲线：
library(dcurves)
library(ggplot2)
library(magrittr)

# 1. 准备训练集专用数据框 (基于 train_std)
dca_train_df <- data.frame(
  outcome = ifelse(train_std$是否 == "Yes", 1, 0)
)

# 2. 提取 8 个模型在训练集上的预测概率
cat("正在处理训练集各模型概率...\n")

for (n in names(models_full)) {
  m <- models_full[[n]]
  
  if (n == "LR_RCS") {
    # 针对 rms 包的 lrm 模型，提取训练集线性预测值并转概率
    lp_tr <- predict(m, train_std)
    dca_train_df[[n]] <- as.numeric(1 / (1 + exp(-lp_tr)))
  } else {
    # 针对 caret 训练的模型，提取训练集概率
    dca_train_df[[n]] <- predict(m, train_std, type = "prob")$Yes
  }
}

# 3. 构建公式
model_names <- names(models_full)
dca_formula <- as.formula(paste("outcome ~", paste(model_names, collapse = " + ")))

# 4. 绘图（使用简化点位和 10 个颜色值防止报错）
dca_train_plot <- dca(dca_formula, 
                      data = dca_train_df,
                      thresholds = seq(0, 1, by = 0.02)) %>% # 增大步长简化图形
  plot(smooth = TRUE) +
  theme_classic() + # 使用简洁风格
  scale_color_manual(values = c(
    "black", "#377EB8", "#4DAF4A", "#984EA3", 
    "#FF7F00", "#A65628", "#F781BF", "#999999", 
    "#E41A1C", "#666666" # 对应 All 和 None
  )) +
  labs(
    title = "Decision Curve Analysis: Training Set",
    x = "Threshold Probability",
    y = "Net Benefit",
    color = "Models"
  ) +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

# 5. 保存并展示
print(dca_train_plot)
ggsave("DCA_All_Models_Training_Set.png", width = 10, height = 7, dpi = 300)


校准曲线：
library(ggplot2)
library(reshape2)

# 定义绘制圆滑校准曲线的函数
plot_smooth_calibration <- function(d_set, d_name) {
  
  # 1. 自动获取正类标签 (假设因子变量的第二个 level 是阳性结果)
  pos_label <- levels(d_set$是否)[2]
  cat("检测到正类标签为:", pos_label, "\n")
  
  # 2. 提取所有模型的预测概率
  plot_data <- data.frame(obs = ifelse(d_set$是否 == pos_label, 1, 0))
  
  for (n in names(models_full)) {
    if (n == "LR_RCS") {
      # 逻辑回归概率转换
      lp <- predict(models_full[[n]], d_set)
      plot_data[[n]] <- as.numeric(1 / (1 + exp(-lp)))
    } else {
      # 机器学习模型提取概率 (自动适配正类列)
      probs <- predict(models_full[[n]], d_set, type = "prob")
      plot_data[[n]] <- probs[, pos_label]
    }
  }
  
  # 3. 数据重构为长格式以供 ggplot 使用
  m_data <- melt(plot_data, id.vars = "obs", variable.name = "Model", value.name = "Prob")
  
  # 4. 绘图：使用 geom_smooth 实现圆滑曲线
  p <- ggplot(m_data, aes(x = Prob, y = obs, color = Model)) +
    # 绘制理想对角线
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey50") +
    # 使用 loess 平滑算法绘制校准曲线
    geom_smooth(method = "loess", se = FALSE, size = 1.2, span = 0.75) +
    # 设置坐标轴范围
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
    theme_minimal() +
    labs(title = paste("Smooth Calibration Curves:", d_name, "Set"),
         x = "Predicted Probability",
         y = "Observed Frequency (Actual)") +
    theme(legend.position = "right",
          plot.title = element_text(hjust = 0.5, face = "bold"))
  
  # 5. 保存图片
  ggsave(paste0("Smooth_Calibration_", d_name, ".png"), plot = p, width = 8, height = 6, dpi = 300)
  return(p)
}

# 运行生成
plot_smooth_calibration(train_std, "Training")
plot_smooth_calibration(test_std, "Testing")


计算B值和S：P
library(rms)
install.packages("ResourceSelection")
library(rms)
library(ResourceSelection) # 用于 Hosmer-Lemeshow 检验

get_full_calib_metrics <- function(d_set, group_name) {
  
  pos_label <- levels(d_set$是否)[2]
  y_true <- ifelse(d_set$是否 == pos_label, 1, 0)
  
  results <- lapply(names(models_full), function(name) {
    m <- models_full[[name]]
    
    # 提取预测概率
    if (name == "LR_RCS") {
      p <- as.numeric(1 / (1 + exp(-predict(m, d_set))))
    } else {
      p <- predict(m, d_set, type = "prob")[, pos_label]
    }
    
    # 1. 使用 rms::val.prob 提取 Brier, Slope, Intercept
    # pl=FALSE 表示不画图
    v <- val.prob(p, y_true, pl=FALSE) 
    
    # 2. 计算 Hosmer-Lemeshow 检验 (g=10组)
    # 若 p 极接近 0 或 1 可能报错，使用 tryCatch 增强稳定性
    hl_p <- tryCatch({
      hl <- hoslem.test(y_true, p, g = 10)
      hl$p.value
    }, error = function(e) return(NA))
    
    data.frame(
      Group = group_name,
      Model = name,
      Brier = round(v["Brier"], 4),
      Slope = round(v["Slope"], 3),
      Intercept = round(v["Intercept"], 3),
      HL_p_value = round(hl_p, 4),
      Is_P_gt_0.05 = ifelse(hl_p > 0.05, "Yes (Good)", "No (Poor)")
    )
  })
  
  do.call(rbind, results)
}

# 2. 执行并查看结果
final_metrics_train <- get_full_calib_metrics(train_std, "Train")
final_metrics_test  <- get_full_calib_metrics(test_std, "Test")

# 合并汇总
full_report <- rbind(final_metrics_train, final_metrics_test)
print(full_report)

# 3. 导出 CSV
write.csv(full_report, "Model_Calibration_P_Values.csv", row.names = FALSE)



# 定义只绘制 LR_RCS 和 GBM 的圆滑校准曲线函数
plot_lr_gbm_calibration <- function(d_set, d_name) {
  
  # 1. 自动获取正类标签 (假设因子变量的第二个 level 是阳性结果，即 "Yes")
  pos_label <- levels(d_set$是否)[2]
  cat("正在处理", d_name, "集，检测到正类标签为:", pos_label, "\n")
  
  # 2. 提取预测概率
  plot_data <- data.frame(obs = ifelse(d_set$是否 == pos_label, 1, 0))
  
  # 【修改点】：定义需要保留的模型名称
  target_models <- c("LR_RCS", "GBM") 
  
  for (n in target_models) {
    # 检查模型是否存在于 models_full 列表中
    if (!is.null(models_full[[n]])) {
      if (n == "LR_RCS") {
        # 逻辑回归（RCS版）概率转换
        lp <- predict(models_full[[n]], d_set)
        plot_data[[n]] <- as.numeric(1 / (1 + exp(-lp)))
      } else {
        # GBM 提取概率
        probs <- predict(models_full[[n]], d_set, type = "prob")
        plot_data[[n]] <- probs[, pos_label]
      }
    } else {
      warning(paste("模型", n, "未在 models_full 中找到，请检查名称是否正确"))
    }
  }
  
  # 3. 数据重构为长格式
  library(reshape2)
  m_data <- melt(plot_data, id.vars = "obs", variable.name = "Model", value.name = "Prob")
  
  # 4. 绘图
  p <- ggplot(m_data, aes(x = Prob, y = obs, color = Model)) +
    # 绘制理想对角线
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey50") +
    # 使用 loess 平滑算法绘制校准曲线
    geom_smooth(method = "loess", se = FALSE, size = 1.2, span = 0.75) +
    # 设置颜色，让 LR 和 GBM 更具辨识度
    scale_color_manual(values = c("LR_RCS" = "#1F77B4", "GBM" = "#D62728")) + 
    # 设置坐标轴
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
    theme_minimal() +
    labs(title = paste("Calibration Curves (LR vs GBM):", d_name, "Set"),
         x = "Predicted Probability",
         y = "Actual Probability") +
    theme(legend.position = "right",
          plot.title = element_text(hjust = 0.5, face = "bold"))
  
  # 5. 保存图片
  ggsave(paste0("LR_GBM_Calibration_", d_name, ".png"), plot = p, width = 8, height = 6, dpi = 300)
  return(p)
}

# 6. 运行生成
plot_lr_gbm_calibration(train_std, "Training")
plot_lr_gbm_calibration(test_std, "Testing")
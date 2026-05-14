# 只移除特定的数据对象，保留函数等
rm_list <- ls(pattern = "data|model|fit|result", envir = .GlobalEnv)
rm(list = rm_list, envir = .GlobalEnv)
cat("已清理的对象:", paste(rm_list, collapse = ", "), "\n")


library(readr)
df <- read.csv("C:/Users/lenovo/Desktop/大论文文档/先标准化/单因素后训练集数据.csv", header = TRUE, stringsAsFactors = FALSE)
View(df)#初步查看我的数据


# 1. 安装并加载必要的包
if (!require("glmnet")) install.packages("glmnet")
library(glmnet)
# 加载必要的包
library(glmnet)
library(Boruta)
library(dplyr)


# 1. 加载必要包
if (!require("glmnet")) install.packages("glmnet")
library(glmnet)

# ==========================================
# 2. 数据预处理 (假设您的数据框名为 df)
# ==========================================
# 定义因变量和自变量
target <- "是否"
# 连续变量列表（请根据实际名称修改）
cont_vars <- c("碱性磷酸酶", "中淋", "尿酸肌酐", "空腹", "空腹1h","白细胞", 
               "糖化", "血红蛋白", "纤维蛋白原", "谷丙转氨酶", "尿素")
# 分类变量列表
cat_vars <- c("剖宫产史", "文化程度")

# A. 仅对连续变量进行标准化 (Z-score)
df_std <- df
df_std[cont_vars] <- lapply(df[cont_vars], scale)

# B. 将分类变量转为 factor
for(col in cat_vars) df_std[[col]] <- as.factor(df_std[[col]])

# C. 构建哑变量矩阵 (LASSO 必须输入矩阵)
# model.matrix 会自动把分类变量转为 0/1 哑变量，并保留已标准化的连续变量
f <- as.formula(paste(target, "~", paste(c(cont_vars, cat_vars), collapse = " + ")))
x <- model.matrix(f, data = df_std)[, -1] # 去掉截距项
y <- as.numeric(as.factor(df[[target]])) - 1 # 确保 y 是 0 和 1

# ==========================================
# 3. 运行 LASSO 交叉验证
# ==========================================
set.seed(2026) # 固定种子，确保结果可复现
# standardize = FALSE 因为我们前面已经手动处理过连续变量了
cv_lasso <- cv.glmnet(x, y, family = "binomial", alpha = 1, standardize = FALSE)

# ==========================================
# 4. 提取关键参数与变量数
# ==========================================
l_min <- cv_lasso$lambda.min
l_1se <- cv_lasso$lambda.1se

# 获取 lambda.min 对应的非零系数变量
coef_min <- coef(cv_lasso, s = l_min)
active_index <- which(as.numeric(coef_min) != 0)
# 排除第一个截距项
selected_indices <- active_index[active_index != 1]
num_min <- length(selected_indices)
final_vars <- row.names(coef_min)[selected_indices]

# ==========================================
# 1. 设置高质量绘图参数
# 如果你想直接保存为高清文件，取消下面 tiff 行前的注释
# tiff("LASSO_Combined_Plots.tiff", width = 24, height = 12, units = "cm", res = 300)

# 设置 1行2列 的布局，调整外边距(oma)和内边距(mar)
par(mfrow = c(1, 2), mar = c(5, 5, 6, 4), oma = c(1, 1, 2, 0))

# --- 图 A：美化版交叉验证误差图 ---
plot(cv_lasso, 
     xlab = expression(log(lambda)), 
     ylab = "Binomial Deviance", 
     main = "A: Parameter Selection (CV)", 
     col = "#D55E00",     # 经典的医学学术红
     pch = 20)           # 更精致的点样式

# 标注最优 Lambda.min 虚线
abline(v = log(cv_lasso$lambda.min), col = "#0072B2", lty = 2, lwd = 1.8) # 专业深蓝色
# 标注 1se 线（可选）
abline(v = log(cv_lasso$lambda.1se), col = "darkgrey", lty = 3, lwd = 1.5)



# --- 图 B：美化版系数路径图 ---
# 提取路径模型
fit_lasso <- cv_lasso$glmnet.fit

# 使用专业的配色方案（RColorBrewer 或内部渐变）
# 设置画布以便右侧标注变量名
plot(fit_lasso, xvar = "lambda", sign.lambda=1,label = FALSE, 
     main = "B: Coefficient Profiles", 
     xlab = expression(log(lambda)), 
     ylab = "Coefficients")

# 标注最优 lambda 位置
abline(v = log(cv_lasso$lambda.min), col = "#0072B2", lty = 2, lwd = 1.8)

# 动态标注关键变量名（可选，如果变量太多建议不加以免拥挤）
# 获取最终入选变量的系数和名称
vnames <- rownames(fit_lasso$beta)
final_coefs <- as.numeric(coef(cv_lasso, s = "lambda.min"))[-1]
active_vars <- which(final_coefs != 0)


# 添加总标题
mtext("LASSO Regression Feature Selection", outer = TRUE, cex = 1.2, font = 2)

# 如果开启了 tiff，记得关闭
dev.off()
# ==========================================
# 6. 输出最终清单
# ==========================================
cat("\n--- LASSO 筛选结果 ---\n")
cat("最佳 Lambda (min):", l_min, "\n")
cat("筛选出的变量总数:", num_min, "\n")
cat("变量清单:\n")
print(final_vars)



brouta算法：
# 1. 加载包
if (!require("Boruta")) install.packages("Boruta")
library(Boruta)

# 2. 准备 Boruta 专用数据集
# 注意：我们要使用 LASSO 筛选出来的变量 (final_lasso_vars)
# 确保 y (是否) 是因子类型
df_boruta <- as.data.frame(x[, final_vars]) # 从之前的 model.matrix 矩阵中提取
df_boruta$是否 <- as.factor(y) # 这里的 y 是 0/1 向量

# 3. 运行 Boruta 算法
set.seed(2026) # 保持随机种子一致
# maxRuns 建议设为 500 以确保收敛
boruta_train <- Boruta(是否 ~ ., data = df_boruta, doTrace = 2, maxRuns = 500)

# 4. 自动处理“待定(Tentative)”变量
# 如果迭代结束后仍有待定变量，通过对比中位数强制分类
final_boruta <- TentativeRoughFix(boruta_train)

# 5. 提取最终确定的变量清单
confirmed_vars <- getSelectedAttributes(final_boruta, withTentative = FALSE)

cat("\n--- Boruta 二次筛选结果 ---\n")
print(confirmed_vars)

# 1. 开启高质量设备
tiff("Boruta_Importance_Plot.tiff", width = 20, height = 15, units = "cm", res = 300)

# 2. 调整边距，下方留出更多空间给变量名
par(mar = c(8, 5, 4, 2)) 

# 3. 绘制基础箱线图 (不带默认坐标轴)
plot(final_boruta, 
     xlab = "", 
     xaxt = "n", 
     main = "Feature Importance Analysis (Boruta)",
     cex.main = 1.2)

# 4. 获取重要性历史并排序，确保图表从低到高排列
imp_history <- final_boruta$ImpHistory
# 过滤掉无穷大值
lz <- lapply(1:ncol(imp_history), function(i) imp_history[is.finite(imp_history[,i]), i])
names(lz) <- colnames(imp_history)
labels_sorted <- sort(sapply(lz, median))

# 5. 手动添加排序后的横坐标标签
axis(side = 1, 
     las = 2,                          # 标签垂直排列
     at = 1:ncol(imp_history), 
     labels = names(labels_sorted), 
     cex.axis = 0.7,                   # 缩放字体避免拥挤
     col.axis = "gray20")

# 6. 添加图例说明
# 绿色: Confirmed, 红色: Rejected, 蓝色: Shadow (影子变量)
legend("topleft", 
       legend = c("Confirmed", "Rejected", "Shadow"), 
       fill = c("#1a9641", "#d7191c", "#2b83ba"), 
       bty = "n", cex = 0.8)

dev.off()


多因素+wald
# 1. 加载 rms 包（医学建模最强工具）
if (!require("rms")) install.packages("rms")
library(rms)

# 2. 准备回归数据集
# 使用 Boruta 最终确定的变量 confirmed_vars
final_df <- df_boruta[, c("是否", confirmed_vars)]

# 3. 设定数据分布环境 (rms包必备步骤)
dd <- datadist(final_df)
options(datadist = 'dd')

# 4. 构建包含 RCS 的多因素模型
# 注意：我们将所有连续变量先放入 rcs() 进行非线性探索（设置3个节点）
# 假设 confirmed_vars 中既有连续变量也有分类变量，我们需要区别对待
# 这里手动构建公式，假设连续变量为 cont_vars_final
cont_vars_final <- intersect(confirmed_vars, cont_vars) # 提取确认名单中的连续变量
cat_vars_final <- setdiff(confirmed_vars, cont_vars_final) # 提取确认名单中的分类变量

# 自动拼接公式：连续变量加 rcs，分类变量直接加
formula_rcs <- as.formula(paste("是否 ~ ", 
                                paste(c(paste0("rcs(", cont_vars_final, ", 3)"), cat_vars_final), 
                                      collapse = " + ")))

# 拟合模型
fit_multi <- lrm(formula_rcs, data = final_df, x = TRUE, y = TRUE)

# 5. 执行 Wald 检验（查看 Nonlinear P 值）
wald_table <- anova(fit_multi)
cat("\n--- Wald 检验结果 (查看 Nonlinear 行以判定非线性) ---\n")
print(wald_table)


# 1. 彻底关闭当前的图形设备并重置所有参数
dev.off() 


森林图：
# 1. 提取回归系数、OR值及 95% CI
# 注意：rcs项的OR值解释较复杂，通常森林图展示的是线性部分的OR
summary_res <- summary(fit_multi)

# 2. 绘制森林图
# tiff("Forest_Plot.tiff", width = 18, height = 12, units = "cm", res = 300)
plot(summary_res, main = "Multivariate Logistic Regression (OR and 95% CI)",
     col.dot = "#0072B2", # 专业深蓝
     pch = 16)
dev.off()


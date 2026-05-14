# 1. 确保获取的是最终训练好的 GBM 模型对象
best_model_gbm <- ml_models[["GBM"]] 

# 2. 建议同时保存标准化参数 (preProc)，因为预测新数据时需要先进行同样的标准化
# preProc 是您之前使用 train_data 建立的 
saveRDS(preProc, file = "C:/Users/lenovo/Desktop/大论文文档/gbm_preProcess_params.rds")

# 3. 保存模型对象到本地（.rds 格式最适合 R 重新加载）
saveRDS(best_model_gbm, file = "C:/Users/lenovo/Desktop/大论文文档/best_gbm_model.rds")

# 4. 如果您想保存模型在验证集上的性能指标表，可以导出为 CSV
# 筛选出 GBM 的那一行结果 
gbm_performance <- subset(final_table, Model == "GBM") 
write.csv(gbm_performance, "C:/Users/lenovo/Desktop/大论文文档/GBM_Performance_Metrics.csv", row.names = FALSE)

cat("GBM 模型及其预处理参数已成功保存至您的桌面文件夹。")



# 定义需要保存的对象列表
save_list <- c(
  "models_full",      # 8个基模型的列表
  "ensemble_model",   # Stacking 集成模型
  "preProcParams",    # 标准化参数 (Mean/SD)，用于还原刻度
  "name_map_en",      # 变量中英文对照表
  "vars_full",        # 全量组变量名列表
  "train_data",       # 原始训练集 (用于 datadist 映射)
  "test_data"         # 原始测试集
)

# 保存为 RData 文件
save(list = save_list, file = "Preterm_Birth_Models_Full_2026.RData")

cat("✅ 所有模型及参数已成功保存至 Preterm_Birth_Models_Full_2026.RData\n")
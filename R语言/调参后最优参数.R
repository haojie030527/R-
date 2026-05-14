# 定义提取最优参数的函数
get_best_params <- function(model_list) {
  # 筛选出 caret 训练的模型（LR_RCS 除外）
  caret_models <- model_list[names(model_list) != "LR_RCS"]
  
  # 提取 bestTune
  params_list <- lapply(names(caret_models), function(name) {
    best_t <- caret_models[[name]]$bestTune
    # 将参数转为字符串格式：key1=val1, key2=val2
    param_str <- paste(names(best_t), unlist(best_t), sep = "=", collapse = ", ")
    data.frame(Model = name, Best_Parameters = param_str)
  })
  
  return(do.call(rbind, params_list))
}


# 提取并打印
best_params_full <- get_best_params(models_full)
print("--- Full Vars 组模型调优后最优参数 ---")
print(best_params_full)

write.csv(best_params_full, "Best_Model_Parameters.csv", row.names = FALSE)
# 提取并打印
best_params_full <- get_best_params(models_full)
print("--- Full Vars 组模型调优后最优参数 ---")
print(best_params_full)

# 可选：导出为 CSV
# write.csv(best_params_full, "Best_Model_Parameters.csv", row.names = FALSE)
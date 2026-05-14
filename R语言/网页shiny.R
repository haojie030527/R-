library(shiny)
library(caret)
library(rms)
library(gbm)

# ==========================================
# 1. 加载必要的包
# ==========================================
library(shiny)
library(shinythemes)
library(caret)   # 用于 preProcess 预测

# ==========================================
# 2. 加载模型与标准化参数（请根据实际路径修改）
# ==========================================
# 假设模型和 preProcParams 保存在 .rds 文件中
# 请修改为实际的文件路径
# models_full <- readRDS("models_full.rds")      # 包含 GBM 和 LR_RCS
# ensemble_model <- readRDS("ensemble_model.rds")
# preProcParams <- readRDS("preProcParams.rds")  # caret 生成的标准化对象

# 注意：preProcParams 应基于训练时的变量生成，变量名必须与下面 df_raw 的列名完全一致

# ==========================================
# 3. UI 界面
# ==========================================
ui <- fluidPage(
  theme = shinythemes::shinytheme("flatly"),
  titlePanel("基于 Stacking 集成模型的临床预测器"),
  
  sidebarLayout(
    sidebarPanel(
      tags$h4("请输入患者临床指标："),
      # 输入框（英文 ID，中文标签）
      numericInput("ALP", "碱性磷酸酶 (ALP):", value = 80),
      numericInput("NLR", "中性粒细胞/淋巴细胞比值 (NLR):", value = 1.5),
      numericInput("UA_Cr", "尿酸肌酐比:", value = 0.5),
      numericInput("FBG", "空腹血糖 (0h):", value = 5.0),
      numericInput("Post1h", "餐后血糖 (1h):", value = 7.5),
      numericInput("WBC", "白细胞计数 (WBC):", value = 6.0),
      numericInput("Hb", "血红蛋白 (Hb):", value = 130),
      numericInput("FIB", "纤维蛋白原 (FIB):", value = 3.0),
      numericInput("ALT", "谷丙转氨酶 (ALT):", value = 25),
      
      hr(),
      sliderInput("threshold", "风险分层阈值:", 
                  min = 0, max = 1, value = 0.1794, step = 0.01),
      actionButton("predict_btn", "开始预测", class = "btn-primary btn-lg", width = '100%')
    ),
    
    mainPanel(
      wellPanel(
        tags$h3("预测结果", style = "color: #2c3e50;"),
        hr(),
        uiOutput("result_ui")
      ),
      tags$footer("注：本预测器仅供科研参考，不作为临床诊断唯一依据。", 
                  style = "font-size: 12px; color: grey; margin-top: 20px;")
    )
  )
)

# ==========================================
# 4. Server 逻辑
# ==========================================
server <- function(input, output, session) {
  
  # 预测事件响应
  prediction_res <- eventReactive(input$predict_btn, {
    # 构建原始数据框（列名必须与训练时完全一致，这里是中文）
    df_raw <- data.frame(
      碱性磷酸酶 = input$ALP,
      中淋       = input$NLR,
      尿酸肌酐   = input$UA_Cr,
      空腹       = input$FBG,
      空腹1h     = input$Post1h,
      白细胞     = input$WBC,
      血红蛋白   = input$Hb,
      纤维蛋白原 = input$FIB,
      谷丙转氨酶 = input$ALT
    )
    
    cat("\n--- 新预测请求 ---\n")
    cat("原始输入:\n"); print(df_raw)
    
    # 应用标准化（使用训练集生成的 preProcParams）
    df_scaled <- tryCatch(
      predict(preProcParams, df_raw),
      error = function(e) {
        cat("标准化失败:", e$message, "\n")
        return(NULL)
      }
    )
    
    if (is.null(df_scaled)) {
      return(NA)
    }
    cat("标准化后:\n"); print(df_scaled)
    
    # 第一层预测：GBM
    p_gbm <- tryCatch(
      predict(models_full$GBM, df_scaled, type = "prob")[, "Yes"],
      error = function(e) {
        cat("GBM 预测失败:", e$message, "\n")
        return(NA)
      }
    )
    
    # 第一层预测：逻辑回归（假设 models_full$LR_RCS 是 rms::lrm 对象）
    p_lr_raw <- tryCatch(
      predict(models_full$LR_RCS, df_scaled, type = "fitted"),
      error = function(e) {
        cat("LR 预测失败:", e$message, "\n")
        return(NA)
      }
    )
    
    # 如果返回的是线性预测值（列表），转换为概率
    if (is.list(p_lr_raw) && !is.null(p_lr_raw$linear.predictors)) {
      p_lr <- 1 / (1 + exp(-p_lr_raw$linear.predictors))
    } else if (is.numeric(p_lr_raw) && length(p_lr_raw) == 1) {
      # 检查范围，若不在0~1则转换
      if (p_lr_raw < 0 || p_lr_raw > 1) {
        p_lr <- 1 / (1 + exp(-p_lr_raw))
      } else {
        p_lr <- p_lr_raw
      }
    } else {
      p_lr <- NA
    }
    
    cat("p_gbm =", p_gbm, "  p_lr =", p_lr, "\n")
    
    if (any(is.na(c(p_gbm, p_lr)))) {
      return(NA)
    }
    
    # 第二层预测（集成模型）
    stack_input <- data.frame(gbm_prob = p_gbm, lr_prob = p_lr)
    final_prob_linear <- tryCatch(
      predict(ensemble_model, stack_input, type = "fitted"),
      error = function(e) {
        cat("集成模型预测失败:", e$message, "\n")
        return(NA)
      }
    )
    
    # 转换为概率
    if (is.numeric(final_prob_linear) && !is.na(final_prob_linear)) {
      if (final_prob_linear >= 0 && final_prob_linear <= 1) {
        final_prob <- final_prob_linear
      } else {
        final_prob <- 1 / (1 + exp(-final_prob_linear))
        cat("已将集成模型线性预测值转换为概率:", final_prob, "\n")
      }
    } else {
      final_prob <- NA
    }
    
    cat("最终预测概率 =", final_prob, "\n")
    return(final_prob)
  })
  
  # 渲染结果界面
  output$result_ui <- renderUI({
    prob_val <- prediction_res()
    req(prob_val, !is.na(prob_val))
    
    threshold <- input$threshold
    is_high_risk <- prob_val >= threshold
    risk_color <- ifelse(is_high_risk, "#d9534f", "#5cb85c")
    
    advice_text <- if(is_high_risk) {
      tagList(
        h4("【高风险组临床建议】", style = "color:#d9534f; font-weight:bold;"),
        tags$ul(
          tags$li("加强产检频率：建议每 1-2 周进行一次围产期检查，密切监测宫颈长度变化。"),
          tags$li("血糖精细化管理：严格执行 GDM 饮食与运动方案，必要时增加胰岛素治疗强度以减少代谢紊乱引发的炎症反应。"),
          tags$li("症状预防教育：指导患者识别早期宫缩、阴道流血或流液等先兆早产症状，并建立快速就医通道。"),
          tags$li("医疗干预评估：临床医生应根据具体指标（如宫颈长度 < 25mm）评估是否需要使用孕酮类药物或实施宫颈环扎术。")
        )
      )
    } else {
      tagList(
        h4("【低风险组临床建议】", style = "color:#5cb85c; font-weight:bold;"),
        tags$ul(
          tags$li("常规孕期随访：维持正常的产检频率，持续监测 FBG (空腹血糖) 等代谢指标。"),
          tags$li("维持健康生活方式：继续保持合理的饮食结构与适量运动，防止血糖大幅波动。"),
          tags$li("自我健康监测：鼓励患者记录胎动情况，并定期自测血糖，确保存档数据稳定。"),
          tags$li("心理疏导：告知其早产风险处于较低水平，缓解焦虑情绪，保持积极的备孕心态。")
        )
      )
    }
    
    tagList(
      wellPanel(
        style = paste0("border-left: 5px solid ", risk_color, "; background-color: #f9f9f9;"),
        h3("预测概率:", span(style = paste0("color:", risk_color), paste0(round(prob_val * 100, 2), "%"))),
        h4("风险分层:", span(style = paste0("color:", risk_color), ifelse(is_high_risk, "高风险", "低风险"))),
        hr(),
        advice_text
      ),
      p(em("免责声明：本预测工具仅供科研参考，具体治疗方案请以临床医生面诊意见为准。"), style = "font-size: 12px; color: #777;")
    )
  })
}

# ==========================================
# 5. 运行应用
# ==========================================
shinyApp(ui = ui, server = server)




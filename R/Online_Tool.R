# ==========================================
# UI Configuration
# ==========================================
ui <- fluidPage(
  theme = shinythemes::shinytheme("flatly"),
  titlePanel("Clinical Predictor Based on Stacking Ensemble Model"),
  
  sidebarLayout(
    sidebarPanel(
      tags$h4("Please enter patient clinical indicators:"),
      
      # Input fields (English IDs, English Labels)
      numericInput("ALP", "Alkaline Phosphatase (ALP):", value = 80),
      numericInput("NLR", "Neutrophil-to-Lymphocyte Ratio (NLR):", value = 1.5),
      numericInput("UA_Cr", "Uric Acid to Creatinine Ratio (UA/Cr):", value = 0.5),
      numericInput("FBG", "Fasting Blood Glucose (FBG, 0h):", value = 5.0),
      numericInput("Post1h", "1-hour Postprandial Glucose (1h-PBG):", value = 7.5),
      numericInput("WBC", "White Blood Cell Count (WBC):", value = 6.0),
      numericInput("Hb", "Hemoglobin (Hb):", value = 130),
      numericInput("FIB", "Fibrinogen (FIB):", value = 3.0),
      numericInput("ALT", "Alanine Aminotransferase (ALT):", value = 25),
      
      hr(),
      sliderInput("threshold", "Risk Stratification Threshold:", 
                  min = 0, max = 1, value = 0.1794, step = 0.01),
      actionButton("predict_btn", "Start Prediction", class = "btn-primary btn-lg", width = '100%')
    ),
    
    mainPanel(
      wellPanel(
        tags$h3("Prediction Results", style = "color: #2c3e50;"),
        hr(),
        uiOutput("result_ui")
      ),
      tags$footer("Note: This predictor is for research reference only and should not be used as the sole basis for clinical diagnosis.", 
                  style = "font-size: 12px; color: grey; margin-top: 20px;")
    )
  )
)

# ==========================================
# 4. Server Logic
# ==========================================
server <- function(input, output, session) {
  
  # Prediction event response
  prediction_res <- eventReactive(input$predict_btn, {
    
    # Build raw dataframe 
    # (KEEP IN CHINESE: Column names must perfectly match the training data features)
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
    
    cat("\n--- New Prediction Request ---\n")
    cat("Raw Input:\n"); print(df_raw)
    
    # Apply standardization (using preProcParams generated from the training set)
    df_scaled <- tryCatch(
      predict(preProcParams, df_raw),
      error = function(e) {
        cat("Standardization failed:", e$message, "\n")
        return(NULL)
      }
    )
    
    if (is.null(df_scaled)) {
      return(NA)
    }
    cat("After standardization:\n"); print(df_scaled)
    
    # Level 1 Prediction: GBM
    p_gbm <- tryCatch(
      predict(models_full$GBM, df_scaled, type = "prob")[, "Yes"],
      error = function(e) {
        cat("GBM prediction failed:", e$message, "\n")
        return(NA)
      }
    )
    
    # Level 1 Prediction: Logistic Regression (assuming models_full$LR_RCS is an rms::lrm object)
    p_lr_raw <- tryCatch(
      predict(models_full$LR_RCS, df_scaled, type = "fitted"),
      error = function(e) {
        cat("LR prediction failed:", e$message, "\n")
        return(NA)
      }
    )
    
    # Convert to probability if returned as linear predictors (list)
    if (is.list(p_lr_raw) && !is.null(p_lr_raw$linear.predictors)) {
      p_lr <- 1 / (1 + exp(-p_lr_raw$linear.predictors))
    } else if (is.numeric(p_lr_raw) && length(p_lr_raw) == 1) {
      # Check range, convert if not between 0 and 1
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
    
    # Level 2 Prediction (Ensemble Model)
    stack_input <- data.frame(gbm_prob = p_gbm, lr_prob = p_lr)
    final_prob_linear <- tryCatch(
      predict(ensemble_model, stack_input, type = "fitted"),
      error = function(e) {
        cat("Ensemble model prediction failed:", e$message, "\n")
        return(NA)
      }
    )
    
    # Convert to probability
    if (is.numeric(final_prob_linear) && !is.na(final_prob_linear)) {
      if (final_prob_linear >= 0 && final_prob_linear <= 1) {
        final_prob <- final_prob_linear
      } else {
        final_prob <- 1 / (1 + exp(-final_prob_linear))
        cat("Converted linear predictor of ensemble model to probability:", final_prob, "\n")
      }
    } else {
      final_prob <- NA
    }
    
    cat("Final prediction probability =", final_prob, "\n")
    return(final_prob)
  })
  
  # Render result UI
  output$result_ui <- renderUI({
    prob_val <- prediction_res()
    req(prob_val, !is.na(prob_val))
    
    threshold <- input$threshold
    is_high_risk <- prob_val >= threshold
    risk_color <- ifelse(is_high_risk, "#d9534f", "#5cb85c")
    
    advice_text <- if(is_high_risk) {
      tagList(
        h4("【Clinical Recommendations for High-Risk Group】", style = "color:#d9534f; font-weight:bold;"),
        tags$ul(
          tags$li("Increase frequency of prenatal visits: Perinatal examinations are recommended every 1-2 weeks, with close monitoring of cervical length changes."),
          tags$li("Refined blood glucose management: Strictly implement GDM diet and exercise plans; if necessary, increase insulin therapy intensity to reduce inflammatory responses caused by metabolic disorders."),
          tags$li("Symptom prevention education: Guide patients to identify symptoms of threatened preterm labor, such as early contractions, vaginal bleeding, or fluid leakage, and establish a fast-track medical channel."),
          tags$li("Medical intervention assessment: Clinicians should evaluate the need for progesterone medications or cervical cerclage based on specific indicators (e.g., cervical length < 25mm).")
        )
      )
    } else {
      tagList(
        h4("【Clinical Recommendations for Low-Risk Group】", style = "color:#5cb85c; font-weight:bold;"),
        tags$ul(
          tags$li("Routine prenatal follow-up: Maintain regular frequency of prenatal visits and continuously monitor metabolic indicators such as FBG."),
          tags$li("Maintain a healthy lifestyle: Continue with a reasonable diet and moderate exercise to prevent significant blood glucose fluctuations."),
          tags$li("Self-health monitoring: Encourage patients to record fetal movements and regularly self-test blood glucose to ensure stable records."),
          tags$li("Psychological counseling: Inform the patient that the risk of preterm birth is low to relieve anxiety and maintain a positive mindset.")
        )
      )
    }
    
    tagList(
      wellPanel(
        style = paste0("border-left: 5px solid ", risk_color, "; background-color: #f9f9f9;"),
        h3("Prediction Probability:", span(style = paste0("color:", risk_color), paste0(round(prob_val * 100, 2), "%"))),
        h4("Risk Stratification:", span(style = paste0("color:", risk_color), ifelse(is_high_risk, "High Risk", "Low Risk"))),
        hr(),
        advice_text
      ),
      p(em("Disclaimer: This prediction tool is for research reference only. Please refer to clinical consultation for specific treatment plans."), style = "font-size: 12px; color: #777;")
    )
  })
}

# ==========================================
# 5. Run Application
# ==========================================
shinyApp(ui = ui, server = server)
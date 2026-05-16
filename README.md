# R-
Building Recognition Models and Online Classifiers
# GDM-sPTB Risk Assessment Model

This repository contains the R code and trained ensemble model for the manuscript:

**"Development of a Risk Assessment Model for Spontaneous Preterm Birth in Patients with Gestational Diabetes Mellitus Based on Ensemble Learning Algorithms"**  
(Hao J, et al., 2026)

## 🧬 Project overview

We developed a Stacking ensemble model (GBM + logistic regression) to predict the risk of spontaneous preterm birth (sPTB) in patients with gestational diabetes mellitus (GDM). The model uses nine clinical features and outputs a probability of sPTB. An online risk calculator is also available (see manuscript).

This repository provides:
- All R scripts to reproduce the analysis from raw data
- The final trained model bundle (`final_model_bundle.rds`)
- A standalone prediction function (`predict_sptb_risk.R`)
- Step‑by‑step instructions for replication and prediction

## 📁 Repository contents

| File / folder | Description |
|---------------|-------------|
| `final_model_bundle.rds` | Pre‑trained Stacking ensemble model (includes pre‑processing parameters, GBM and LR base models, and the meta‑learner) |
| `predict_sptb_risk.R` | R function to compute sPTB risk for new patients |
| `R/` | All R scripts used in the study (data cleaning, feature selection, model training, evaluation, SHAP, figures) |
| `README.md` | This file |

## ⚙️ Requirements

- R ≥ 4.5.1
- Required R packages (install with `install.packages(c("caret","gbm","randomForest","kernlab","nnet","rpart","kknn","naivebayes","rms","pROC","PRROC","ggplot2","shiny"))`)

## 📊 Reproduce the analysis

1. Clone this repository.
2. Open R and set the working directory to the repository root.
3. Place your raw data (CSV) in `data/` (see `data/sample_data.csv` for expected format).
4. Run the scripts in the following order (all located in `R/`):
   - `01_LASSO_Boruta_Multivariate_Nonlinear.R`
   - `02_Full_Reduced_Set_Modeling_Performance.R`
   - `03_ROC_Curves_Single_Full_Models.R.R` 
   - `04_DCA_and_Calibration_Curves_8Models.R`   
   - `05_PR_Curves_Validation_8Models.R` 
   - `06_Ensemble_DCA_Calibration.R`    
   - `07_Ensemble_PR_Curve.R`
   - `08_Shiny_WebApp.R`   
   - `09_Online_Tool.R`   

> **Note**: Running all scripts from scratch will take approximately 20‑30 minutes. The pre‑trained model bundle is provided so you can skip training and directly use the model for prediction.

## 🔮 Use the model for new patients

1. Load the model bundle:
  r
   source("predict_sptb_risk.R")
   model_bundle <- readRDS("final_model_bundle.rds")
2.Prepare a data frame for a new patient. The required variable names (and units) are:
  r
  new_patient <- data.frame(
    碱性磷酸酶 = 120,
    中淋 = 4.2,
    尿酸肌酐 = 7.0,
    空腹 = 5.3,
    空腹1h = 9.8,
    白细胞 = 8.5,
    血红蛋白 = 125,
    纤维蛋白原 = 4.2,
    谷丙转氨酶 = 15
   )
3.Calculate the sPTB risk:
  r
  risk <- predict_sptb_risk(new_patient, model_bundle)
  print(paste("sPTB risk (%):", round(risk * 100, 2)))

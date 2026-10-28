# HTG-AP SAP prediction

R code for a seven-variable nomogram predicting severe acute pancreatitis in patients with hypertriglyceridemia-associated acute pancreatitis and no organ failure at admission. The analysis uses a development cohort, a temporal validation cohort, and a multicentre validation cohort.

The predictors are temperature, neutrophil percentage, LDH, amylase, base excess, PaO2, and PaO2/FiO2. This repository contains code only.

## Figures

| Figure | Analysis | Script |
|---|---|---|
| 1 | Cohort selection | `figures/Figure_1.R` |
| 2 | Logistic regression, Boruta, LASSO, and their overlap | `figures/Figure_2.R` |
| 3 | Nomogram, ROC, calibration, decision curves, and clinical impact | `figures/Figure_3.R` |
| 4 | Simplified score, discrimination, clinical comparisons, and outcomes | `figures/Figure_4.R` |
| S1 | Missing values | `supplementary/Figure_S1.R` |
| S2 | Observed and imputed distributions | `supplementary/Figure_S2.R` |
| S3 | LASSO cross-validation | `supplementary/Figure_S3.R` |
| S4 | Restricted cubic splines and nonlinearity tests | `supplementary/Figure_S4.R` |
| S5 | Nomogram sensitivity analyses | `supplementary/Figure_S5.R` |
| S6 | Outcomes by organ failure at admission | `supplementary/Figure_S6.R` |
| S7 | Machine learning: training cross-validation | `supplementary/Figure_S7.R` |
| S8 | Machine learning: development cohort | `supplementary/Figure_S8.R` |
| S9 | Machine learning: temporal validation | `supplementary/Figure_S9.R` |
| S10 | Machine learning: multicentre validation | `supplementary/Figure_S10.R` |
| S11 | Machine-learning calibration | `supplementary/Figure_S11.R` |
| S12 | Machine-learning decision curves | `supplementary/Figure_S12.R` |
| S13 | Outcomes across nomogram risk groups | `supplementary/Figure_S13.R` |

## Run

Install the R packages with `Rscript R/install_packages.R`. The PDF composition scripts use the Python packages in `requirements.txt`.

Set `HTGAP_INPUT_DIR` to the folder containing the local inputs described in [inputs.md](inputs.md). Set `HTGAP_OUTPUT_DIR` to the output folder. The defaults are `private` and `results` within the repository. On Windows, these can be set in PowerShell:

```powershell
$env:HTGAP_INPUT_DIR = 'D:/study/private'
$env:HTGAP_OUTPUT_DIR = 'D:/study/results'
Rscript run.R impute
Rscript run.R selection
Rscript run.R analysis
Rscript run.R Figure_2
Rscript run.R Figure_3
Rscript run.R Figure_4
Rscript run.R Figure_S4
```

Use the same command with any figure number in the table. `HTGAP_FORMATS=pdf` selects PDF output; the default also produces SVG, PNG, and TIFF. Arial is used for statistical figures and Times New Roman for the flow diagram. Python can be selected with `HTGAP_PYTHON`. On other operating systems, set `HTGAP_FONT` and `HTGAP_FONT_BOLD` to the regular and bold Arial-compatible font files.

## Methods

Missing predictors are completed with five MICE imputations using predictive mean matching for continuous variables and logistic regression for binary variables. Imputation is performed within each cohort using predictors only. Regression coefficients and covariance estimates are pooled using Rubin's rules. Predicted probabilities are averaged over the five completed datasets.

Variable selection compares backward AIC, LASSO with five-fold AUC cross-validation, and Boruta within a 26-variable candidate set. A variable is retained by a method when selected in at least three imputations; Boruta includes confirmed and tentative decisions. The intersection is calculated from these sets.

Discrimination uses ROC AUC with DeLong confidence intervals. Calibration, Brier scores, threshold-specific sensitivity, specificity, PPV, and NPV are calculated from the predicted probabilities. Restricted cubic splines use three knots and report pooled tests of overall association and nonlinearity. Clinical-score comparisons are limited to the development and temporal validation cohorts.

Machine-learning code is in [machine_learning](machine_learning). `train_models.R` fits the classifiers and calculates five-fold predictions. Figures S7–S10 arrange the local ROC, precision–recall, and radar PDFs with their AUC labels. Figures S11–S12 use the saved probabilities. Run model training separately with `Rscript run.R machine_learning`.

The main R packages include [mice](https://github.com/amices/mice), [glmnet](https://github.com/cran/glmnet), [Boruta](https://github.com/mbq/Boruta), [rms](https://github.com/harrelfe/rms), [pROC](https://github.com/xrobin/pROC), [dcurves](https://github.com/ddsjoberg/dcurves), [mlr3learners](https://github.com/mlr-org/mlr3learners), and [mlr3extralearners](https://github.com/mlr-org/mlr3extralearners).

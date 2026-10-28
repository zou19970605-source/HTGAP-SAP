# Local inputs

Use numeric, harmonized baseline measurements. Binary outcomes and medical histories are coded 0 and 1. Missing values are blank or `NA`. No patient names or contact details are required.

`clean_training.csv`, `clean_temporal2025.csv`, and `clean_rct.csv` contain `sap` and the 40 predictor names listed in `config.R`. Each `clinical_<cohort>.csv` has the same row order and contains `sap`, `sex`, `onset_days`, `nof`, `pmof`, `ipn`, `ihm`, `icu_los`, and `hosp_los`. The local cohorts also contain the recorded `sirs`, `apache`, and `bisap` scores. Outcomes are not imputed.

The seven model columns are `temperature`, `neutrophil_pct`, `ldh`, `amylase`, `base_excess`, `pao2`, and `pf_ratio`. Use degrees Celsius, neutrophils as a percentage, LDH in U/L, amylase in U/dL, base excess in mmol/L, and oxygen pressures and ratios in mmHg.

For Figure 1, `cohort_flow_counts.csv` contains one row for each cohort, with columns `cohort`, `HTGAP_start`, `onset_over_72h`, `incomplete_admission_OF`, `admission_OF`, and `included`. Cohort names are `training`, `temporal2025`, and `rct`.

For Figure S6, `admission_comparison.csv` contains `cohort`, `admission_of`, `sap`, `pmof`, `ipn`, `ihm`, `icu_los`, and `hosp_los`. The figure uses the two local cohorts.

For variable selection, an optional `selection_folds.csv` contains `sap` and `fold` in development-cohort row order. Fold values are 1–5. If absent, the script creates one stratified five-fold assignment.

For the machine-learning figures, place the temporal analysis exports in `ml/temporal` and the regional analysis exports in `ml/regional`. Use their original filenames:

| Context | Prefix |
|---|---|
| Training cross-validation | `benchmark_` |
| Development | `multi_train_` |
| Validation | `multi_external.validation_` |

The probability files are `benchmark_prediction_data.csv`, `multi_train_prediction_data.csv`, and `multi_external.validation_prediction_data.csv`. They contain `truth` and one column per algorithm. The training summary is `benchmark_all_measures.csv`, with `Model` and `auc`. PDF filenames are listed in `machine_learning/compose_figures.py`.

The optional model-training script reads the first completed dataset for each cohort from `results/data_private/imputations.rds`. Classifiers use the seven model variables. Parameters can be edited in `machine_learning/train_models.R`. CatBoost and LightGBM require their R packages; AdaBoost and the Bayesian network require RWeka and Java. Installation instructions are available from [CatBoost](https://catboost.ai/docs/en/installation/r-installation-binary-installation) and [LightGBM](https://github.com/microsoft/LightGBM/tree/master/R-package).

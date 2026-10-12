# Home Credit Default Risk Project
This project analyzes Home Credit application and historical credit data to identify patterns associated with payment difficulty. Home Credit serves customers who may have limited access to traditional credit, making it important to assess repayment risk while avoiding unnecessary exclusion of applicants who may be able to repay.
The analysis uses the application training dataset, which includes the documented payment-difficulty outcome (TARGET), and the application test dataset, which has the same applicant characteristics but does not include the outcome. Supplementary tables provide historical information on bureau credit records, prior loan applications, installment payments, POS cash activity, and credit-card balances.
The project begins with exploratory data analysis to assess data quality, missingness, class imbalance, key structure, timing, and relationships between application characteristics and payment difficulty. The data preparation workflow then cleans the application data, engineers features, aggregates supplementary historical records to the applicant level, and joins those features to the application tables.
The final prepared training and test datasets use consistent feature definitions. Data-dependent values, such as missing-value imputation and binning thresholds, are learned from training data only and reused for test data to reduce leakage. The resulting datasets are designed for later predictive modeling, validation, and fairness-focused review.
Home credit default prediction project.
This project analyzes the Home Credit dataset to identify factors associated with loan repayment and default risk.

# Home Credit Data Preparation and Feature Engineering
The data_preparation.R script creates a consistent, reusable workflow for preparing the Home Credit data for modeling. Rather than placing all preprocessing steps in a single notebook chunk, the script organizes them into functions that can be called on the training and test datasets using the same rules. This improves consistency, reduces repeated code, and helps prevent data leakage.
The script is organized into five phases. Phase 1 cleans and transforms the application tables by addressing sentinel values, converting negative day fields into interpretable years, creating log-transformed monetary variables, and calculating basic loan-burden ratios. Phase 2 adds EDA-driven engineered features, including age and tenure bands, missing-data indicators, interaction variables, ownership combinations, and external-score summaries.
Phase 3 processes the supplementary historical tables—bureau records, previous applications, payment installments, POS cash balances, and credit-card balances. It filters to records occurring before the current application and aggregates each table to one row per applicant. Phase 4 joins those applicant-level historical features to the application training and test tables using SK_ID_CURR. Phase 5 fits all data-dependent values, including imputation values and bin thresholds, using training data only. It then applies the same stored values to test data and confirms that both outputs contain matching predictor columns.
To use the script, first load it with source("Scripts/data_preparation.R"). After loading the application and supplemental data, build the supplementary features, join them to train and test data, and run the final consistency workflow:

## Source Code and Input Data 
source("Scripts/data_preparation.R")

supplemental_tables <- read_supplemental_tables()
supplemental_features <- build_supplemental_features(supplemental_tables)

enriched <- join_supplementary_to_train_test(
  train_raw,
  test_raw,
  supplemental_features
)

prepared <- prepare_consistent_train_test_data(
  enriched$train,
  enriched$test
)

train_ready <- prepared$train
test_ready <- prepared$test

***The resulting train_ready dataset includes the final predictors and TARGET, while test_ready contains the same predictors without TARGET.***

**Modeling Introduction**
This notebook develops and compares several models for predicting whether an applicant will experience payment difficulty (TARGET = 1). The modeling data combines the primary application tables with applicant-level features aggregated from the bureau, previous-application, installment, POS cash, and credit-card history tables. All models are evaluated using the same five stratified cross-validation folds, with ROC AUC serving as the primary selection metric.

The analysis begins with a null model and then evaluates progressively more flexible algorithms. A pruned decision tree produced a mean cross-validated AUC of 0.6528, demonstrating that a small set of nonlinear decision rules provides some predictive discrimination but is insufficient for this complex outcome. A 100-tree random forest improved the AUC to 0.7504 by combining many nonlinear trees. Elastic-net logistic regression performed better, reaching 0.7629, showing that a regularized combination of application and historical predictors provides a strong benchmark.

Gradient-boosted trees produced the best results. Several XGBoost configurations were evaluated by varying tree depth, learning rate, child-size regularization, missing-data treatment, class weighting, and number of boosting rounds. Native missing-value handling outperformed median imputation, suggesting that the pattern of missing information contains useful predictive signal. Class weighting did not improve AUC and substantially worsened probability error, so it was not used in the final model. Extending the leading model from 350 to 525 rounds also failed to improve validation performance.

The selected model is an unweighted XGBoost model with a maximum tree depth of 7, learning rate of 0.05, minimum child weight of 20, native missing-value handling, and 350 boosting rounds. It achieved a mean five-fold ROC AUC of 0.7825, with a fold standard deviation of 0.0018. It was selected because it provided the strongest and most stable out-of-sample ranking performance while maintaining reasonable runtime. The selected workflow was subsequently refitted using all labeled training applications to generate payment-difficulty probabilities for the competition test set.




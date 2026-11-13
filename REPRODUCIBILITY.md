# Reproducing the Home Credit modeling results

The modeling source is [`Home Credit Modeling.qmd`](Home%20Credit%20Modeling.qmd).
Its helper functions are in [`Scripts/data_preparation.R`](Scripts/data_preparation.R)
and [`Scripts/modeling_pipeline.R`](Scripts/modeling_pipeline.R). The notebook
sets seed **6812** for the five stratified folds and for every XGBoost fit.

## Software

- R **4.6.0**
- Quarto **1.9.36**
- R package versions and transitive dependencies in [`renv.lock`](renv.lock)

From the project root, install `renv` if needed, then run:

```r
renv::activate()
renv::restore(prompt = FALSE)
```

Run those commands in R 4.6.0. The lockfile is the record of the packages used
for the clean reproduction run. The version of Quarto is recorded above because
it is installed separately from R packages.

## Data

Download the Home Credit Default Risk competition data from
[Kaggle](https://www.kaggle.com/competitions/home-credit-default-risk/data) and
place these eight CSV files in the repository root:

```text
application_train.csv
application_test.csv
bureau.csv
bureau_balance.csv
previous_application.csv
installments_payments.csv
POS_CASH_balance.csv
credit_card_balance.csv
```

The files are excluded from Git because they are large. Compare their SHA-256
hashes against [`data_sha256.txt`](data_sha256.txt) before running the notebook.
If the hashes differ, the resulting scores are not directly comparable with
this run.

On systems with `sha256sum` (including Git Bash), run
`sha256sum -c data_sha256.txt` from the repository root. On Windows PowerShell,
`Get-FileHash application_train.csv -Algorithm SHA256` shows a hash to compare
with the corresponding line of the manifest; repeat for the other files.

## Clean run

Start from a fresh clone with no model output cache. On Windows PowerShell, from
the repository root:

```powershell
$env:HOME_CREDIT_OUTPUT_DIR = "Model Outputs Reproduction"
quarto render "Home Credit Modeling.qmd"
```

The environment variable sends every cache, model object, and submission file
created by the notebook to a separate folder. The notebook's default remains
`Model Outputs` when the variable is unset. Do not reuse an existing output
folder for an independent reproduction check: the notebook loads cached
results whenever it finds them.

The run generates a rendered `Home Credit Modeling.html` report and writes these
main artifacts under `Model Outputs Reproduction`:

- `best_model_score_record.csv`: selected configuration and cross-validation
  metrics.
- `selected_xgboost_oof_predictions_refined.rds`: predictions for training
  applicants from folds that did not train on those applicants.
- `final_xgboost_fit.rds`: model refitted on all labeled applications.
- `home_credit_submission.csv`: probabilities for the 48,744 unlabeled test
  applicants.

The historical Kaggle public/private AUCs printed in the notebook came from an
earlier submission. Rendering a newly seeded model does **not** recompute those
scores; its new submission file must be evaluated on Kaggle before a new private
AUC can be reported.

## Observed clean run (November 12, 2026)

With the locked R packages, seed 6812, and an empty output folder, the run
selected **unweighted XGBoost with native missing-value handling and 525
rounds**. Its mean five-fold ROC AUC was **0.782646** (fold standard deviation
**0.001686**). Mean PR AUC was **0.274306** and mean Brier score was **0.066266**.
The output checker below passed for 307,511 out-of-fold predictions and 48,744
test predictions.

The earlier 350-round fit had mean cross-validation AUC 0.782481 and Kaggle
private AUC 0.77982. The 525-round fit is a newly seeded model and **has not
been scored on Kaggle**. Its small cross-validation advantage does not establish
that it will have a higher private leaderboard score.

## Verification

Check that all five fold scores are present, that the selected configuration
and mean AUC are recorded in `best_model_score_record.csv`, and that
`home_credit_submission.csv` has 48,744 unique `SK_ID_CURR` values and no
missing or out-of-range probabilities. The notebook checks the submission
structure automatically. Differences across R, XGBoost, or Quarto versions may
change the exact fitted model, so use the locked versions when comparing scores.

Run the independent output check after the render completes:

```powershell
Rscript "Scripts/verify_modeling_outputs.R" "Model Outputs Reproduction"
```

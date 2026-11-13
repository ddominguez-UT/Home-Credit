# Check the main artifacts from a completed clean modeling run.
args <- commandArgs(trailingOnly = TRUE)
output_dir <- if (length(args)) args[[1]] else
  Sys.getenv("HOME_CREDIT_OUTPUT_DIR", unset = "Model Outputs Reproduction")

score_path <- file.path(output_dir, "best_model_score_record.csv")
oof_path <- file.path(output_dir, "selected_xgboost_oof_predictions_refined.rds")
submission_path <- file.path(output_dir, "home_credit_submission.csv")
fit_path <- file.path(output_dir, "final_xgboost_fit.rds")
required <- c(score_path, oof_path, submission_path, fit_path)
if (!all(file.exists(required))) {
  stop("Missing output files: ", paste(required[!file.exists(required)], collapse = ", "))
}

score <- read.csv(score_path, check.names = FALSE)
if (nrow(score) != 1L || score$model[[1]] != "XGBoost" ||
    score$folds[[1]] != 5L || !is.finite(score$roc_auc_mean[[1]]) ||
    score$roc_auc_mean[[1]] <= 0.5 || score$roc_auc_mean[[1]] > 1) {
  stop("Selected-model score record failed validation.")
}

oof <- readRDS(oof_path)
if (length(oof$probability) != 307511L ||
    length(unique(oof$SK_ID_CURR)) != 307511L ||
    !identical(sort(unique(oof$fold)), 1:5) ||
    anyNA(oof$probability) ||
    any(oof$probability < 0 | oof$probability > 1)) {
  stop("Out-of-fold predictions failed validation.")
}

submission <- read.csv(submission_path, check.names = FALSE)
if (nrow(submission) != 48744L ||
    length(unique(submission$SK_ID_CURR)) != 48744L ||
    anyNA(submission$TARGET) ||
    any(submission$TARGET < 0 | submission$TARGET > 1)) {
  stop("Kaggle submission failed validation.")
}

fit <- readRDS(fit_path)
if (fit$nrounds != as.integer(round(score$best_iteration_mean[[1]])) ||
    !isTRUE(all.equal(fit$submission$SK_ID_CURR,
                      as.numeric(submission$SK_ID_CURR))) ||
    !isTRUE(all.equal(fit$submission$TARGET, submission$TARGET,
                      tolerance = 1e-12))) {
  stop("Saved fit and submission disagree with the selected score record.")
}

cat(sprintf("Selected model: %s\n", score$model[[1]]))
cat(sprintf("Five-fold mean ROC AUC: %.5f\n", score$roc_auc_mean[[1]]))
cat(sprintf("Out-of-fold predictions: %d\n", length(oof$probability)))
cat(sprintf("Test predictions: %d\n", nrow(submission)))
cat("Output validation passed.\n")

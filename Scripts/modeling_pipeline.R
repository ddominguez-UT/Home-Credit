# Reusable modeling functions for the Home Credit default-risk project.
#
# Design principles:
#   * TARGET is treated as the positive class when TARGET = 1.
#   * One fixed set of stratified folds is reused for every candidate model.
#   * Learned preprocessing is fit on each analysis fold, never on assessment data.
#   * ROC AUC is the primary selection metric; PR AUC and Brier score are reported
#     to make class-imbalance and probability calibration visible.

required_modeling_packages <- function() {
  c("Matrix", "glmnet", "rpart", "xgboost", "dplyr", "readr")
}

check_modeling_packages <- function() {
  missing <- required_modeling_packages()[
    !vapply(required_modeling_packages(), requireNamespace, logical(1), quietly = TRUE)
  ]
  if (length(missing)) {
    stop("Install these required packages before modeling: ", paste(missing, collapse = ", "))
  }
  invisible(TRUE)
}

assert_binary_target <- function(y) {
  if (anyNA(y) || !all(y %in% c(0, 1))) {
    stop("The outcome must contain only non-missing 0 and 1 values.")
  }
  invisible(TRUE)
}

# Assign each outcome class evenly across folds. The returned integer vector is
# stored and reused so differences between models cannot be caused by resampling.
make_stratified_folds <- function(y, v = 5L, seed = 6812L) {
  assert_binary_target(y)
  if (v < 2L) stop("`v` must be at least 2.")
  set.seed(seed)
  fold_id <- integer(length(y))
  for (class_value in c(0, 1)) {
    indices <- sample(which(y == class_value))
    fold_id[indices] <- rep(seq_len(v), length.out = length(indices))
  }
  fold_id
}

fold_balance_table <- function(y, fold_id) {
  assert_binary_target(y)
  do.call(rbind, lapply(sort(unique(fold_id)), function(fold) {
    assessment <- fold_id == fold
    data.frame(
      fold = fold,
      analysis_rows = sum(!assessment),
      assessment_rows = sum(assessment),
      assessment_events = sum(y[assessment] == 1),
      assessment_event_rate = mean(y[assessment] == 1)
    )
  }))
}

# Rank-based ROC AUC is equivalent to the Mann-Whitney statistic and handles ties.
roc_auc_binary <- function(truth, probability) {
  keep <- is.finite(probability) & !is.na(truth)
  truth <- truth[keep]
  probability <- probability[keep]
  # Store counts as doubles because n_positive * n_negative exceeds the
  # 32-bit integer limit for the full 307,511-row out-of-fold prediction set.
  n_positive <- as.double(sum(truth == 1))
  n_negative <- as.double(sum(truth == 0))
  if (!n_positive || !n_negative) return(NA_real_)
  ranks <- rank(probability, ties.method = "average")
  (sum(ranks[truth == 1]) - n_positive * (n_positive + 1) / 2) /
    (n_positive * n_negative)
}

# Average precision is used as the PR-AUC summary. It is more informative than
# accuracy when only about eight percent of applications have TARGET = 1.
average_precision_binary <- function(truth, probability) {
  keep <- is.finite(probability) & !is.na(truth)
  truth <- truth[keep]
  probability <- probability[keep]
  if (!sum(truth == 1)) return(NA_real_)
  ordering <- order(probability, decreasing = TRUE)
  ordered_truth <- truth[ordering]
  precision <- cumsum(ordered_truth == 1) / seq_along(ordered_truth)
  mean(precision[ordered_truth == 1])
}

score_probabilities <- function(truth, probability) {
  probability <- pmin(pmax(as.numeric(probability), 0), 1)
  data.frame(
    roc_auc = roc_auc_binary(truth, probability),
    pr_auc = average_precision_binary(truth, probability),
    brier = mean((probability - truth)^2),
    mean_prediction = mean(probability),
    event_rate = mean(truth)
  )
}

# Static feature engineering may be applied before resampling because it learns
# no distributional quantities and never uses TARGET. Quantile cut points and
# imputation values are deliberately excluded here and learned within folds.
make_modeling_frame <- function(data, target_col = "TARGET", id_col = "SK_ID_CURR") {
  if (!exists("engineer_application_features", mode = "function")) {
    stop("Source Scripts/data_preparation.R before calling make_modeling_frame().")
  }
  output <- engineer_application_features(data, target_col)
  if (!id_col %in% names(output)) stop("Modeling data is missing `", id_col, "`.")
  if (target_col %in% names(output)) assert_binary_target(output[[target_col]])
  output
}

finite_or_missing <- function(x) {
  x <- as.numeric(x)
  x[!is.finite(x)] <- NA_real_
  x
}

# Learn a sparse design-matrix specification from one analysis fold. Rare levels
# are collapsed to control dimensionality; unseen assessment/test levels map to
# __OTHER__. Numeric imputation can be median or native (NA retained for XGBoost).
fit_sparse_encoder <- function(data, target_col = "TARGET", id_col = "SK_ID_CURR",
                               numeric_imputation = c("median", "native"),
                               rare_proportion = 0.001,
                               add_numeric_missing_indicators = TRUE) {
  numeric_imputation <- match.arg(numeric_imputation)
  predictors <- setdiff(names(data), c(target_col, id_col))
  numeric_columns <- predictors[vapply(data[predictors], is.numeric, logical(1))]
  categorical_columns <- setdiff(predictors, numeric_columns)

  medians <- vapply(numeric_columns, function(column) {
    values <- finite_or_missing(data[[column]])
    answer <- stats::median(values, na.rm = TRUE)
    if (is.finite(answer)) answer else 0
  }, numeric(1))

  missing_indicator_columns <- if (add_numeric_missing_indicators) {
    numeric_columns[vapply(numeric_columns, function(column) {
      anyNA(finite_or_missing(data[[column]]))
    }, logical(1))]
  } else character()

  categorical_levels <- lapply(categorical_columns, function(column) {
    values <- as.character(data[[column]])
    values[is.na(values) | !nzchar(values)] <- "__MISSING__"
    counts <- table(values)
    minimum_count <- max(2L, ceiling(nrow(data) * rare_proportion))
    kept <- names(counts)[counts >= minimum_count]
    unique(c(sort(setdiff(kept, c("__MISSING__", "__OTHER__"))),
             "__MISSING__", "__OTHER__"))
  })
  names(categorical_levels) <- categorical_columns

  categorical_feature_names <- unlist(Map(function(column, levels) {
    paste0(column, "__", make.names(levels))
  }, categorical_columns, categorical_levels), use.names = FALSE)
  feature_names <- make.unique(c(
    numeric_columns,
    paste0("MISSING_NUMERIC__", missing_indicator_columns),
    categorical_feature_names
  ))

  structure(list(
    target_col = target_col,
    id_col = id_col,
    numeric_imputation = numeric_imputation,
    numeric_columns = numeric_columns,
    categorical_columns = categorical_columns,
    medians = medians,
    missing_indicator_columns = missing_indicator_columns,
    categorical_levels = categorical_levels,
    feature_names = feature_names
  ), class = "home_credit_sparse_encoder")
}

apply_sparse_encoder <- function(data, encoder) {
  if (!inherits(encoder, "home_credit_sparse_encoder")) {
    stop("`encoder` must be created by fit_sparse_encoder().")
  }
  required <- c(encoder$numeric_columns, encoder$categorical_columns)
  missing_columns <- setdiff(required, names(data))
  if (length(missing_columns)) {
    stop("Data is missing encoded predictors: ", paste(missing_columns, collapse = ", "))
  }
  row_count <- nrow(data)

  numeric_values <- lapply(encoder$numeric_columns, function(column) {
    values <- finite_or_missing(data[[column]])
    if (encoder$numeric_imputation == "median") {
      values[is.na(values)] <- encoder$medians[[column]]
    }
    values
  })
  numeric_matrix <- if (length(numeric_values)) {
    Matrix::Matrix(do.call(cbind, numeric_values), sparse = TRUE)
  } else {
    Matrix::Matrix(0, nrow = row_count, ncol = 0, sparse = TRUE)
  }

  indicator_matrix <- if (length(encoder$missing_indicator_columns)) {
    indicators <- lapply(encoder$missing_indicator_columns, function(column) {
      as.numeric(is.na(finite_or_missing(data[[column]])))
    })
    Matrix::Matrix(do.call(cbind, indicators), sparse = TRUE)
  } else {
    Matrix::Matrix(0, nrow = row_count, ncol = 0, sparse = TRUE)
  }

  categorical_matrices <- Map(function(column, levels) {
    values <- as.character(data[[column]])
    values[is.na(values) | !nzchar(values)] <- "__MISSING__"
    values[!values %in% levels] <- "__OTHER__"
    codes <- match(values, levels)
    Matrix::sparseMatrix(
      i = seq_len(row_count), j = codes, x = 1,
      dims = c(row_count, length(levels))
    )
  }, encoder$categorical_columns, encoder$categorical_levels)
  categorical_matrix <- if (length(categorical_matrices)) {
    do.call(cbind, categorical_matrices)
  } else {
    Matrix::Matrix(0, nrow = row_count, ncol = 0, sparse = TRUE)
  }

  output <- cbind(numeric_matrix, indicator_matrix, categorical_matrix)
  colnames(output) <- encoder$feature_names
  methods::as(output, "dgCMatrix")
}

summarize_cv_results <- function(results) {
  if (!nrow(results)) return(results)
  metric_columns <- intersect(c("roc_auc", "pr_auc", "brier", "best_iteration"), names(results))
  group_columns <- setdiff(names(results), c("fold", metric_columns,
                                             "mean_prediction", "event_rate"))
  key <- interaction(results[group_columns], drop = TRUE, lex.order = TRUE)
  summaries <- lapply(split(results, key), function(part) {
    identifying <- part[1, group_columns, drop = FALSE]
    metrics <- do.call(cbind, lapply(metric_columns, function(column) {
      values <- part[[column]]
      c(mean = mean(values, na.rm = TRUE), sd = stats::sd(values, na.rm = TRUE))
    }))
    names(metrics) <- as.vector(rbind(paste0(metric_columns, "_mean"),
                                      paste0(metric_columns, "_sd")))
    cbind(identifying, as.data.frame(as.list(metrics)), folds = nrow(part))
  })
  output <- do.call(rbind, summaries)
  rownames(output) <- NULL
  output[order(output$roc_auc_mean, decreasing = TRUE), , drop = FALSE]
}

cross_validate_null <- function(data, fold_id, target_col = "TARGET") {
  y <- data[[target_col]]
  do.call(rbind, lapply(sort(unique(fold_id)), function(fold) {
    analysis <- fold_id != fold
    assessment <- !analysis
    probability <- rep(mean(y[analysis]), sum(assessment))
    cbind(data.frame(model = "Null model", config = "training event rate", fold = fold),
          score_probabilities(y[assessment], probability))
  }))
}

default_glmnet_grid <- function() {
  expand.grid(
    # Pure ridge was excluded after a full-data timing check showed that its
    # dense coefficient path was disproportionate for this notebook's runtime.
    alpha = c(0.5, 1),
    lambda = c(0.0003, 0.001, 0.003),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
}

cross_validate_glmnet <- function(data, fold_id, grid = default_glmnet_grid(),
                                  target_col = "TARGET", id_col = "SK_ID_CURR",
                                  rare_proportion = 0.001) {
  check_modeling_packages()
  y <- data[[target_col]]
  results <- list()
  result_index <- 1L
  for (fold in sort(unique(fold_id))) {
    analysis <- fold_id != fold
    assessment <- !analysis
    encoder <- fit_sparse_encoder(
      data[analysis, , drop = FALSE], target_col, id_col,
      numeric_imputation = "median", rare_proportion = rare_proportion
    )
    x_analysis <- apply_sparse_encoder(data[analysis, , drop = FALSE], encoder)
    x_assessment <- apply_sparse_encoder(data[assessment, , drop = FALSE], encoder)

    for (alpha_value in unique(grid$alpha)) {
      alpha_grid <- grid[grid$alpha == alpha_value, , drop = FALSE]
      lambda_values <- sort(unique(alpha_grid$lambda), decreasing = TRUE)
      fit <- glmnet::glmnet(
        x = x_analysis, y = y[analysis], family = "binomial",
        alpha = alpha_value, lambda = lambda_values, standardize = TRUE
      )
      predictions <- as.matrix(stats::predict(
        fit, newx = x_assessment, s = lambda_values, type = "response"
      ))
      for (lambda_index in seq_along(lambda_values)) {
        config <- sprintf("alpha=%.1f, lambda=%g", alpha_value, lambda_values[lambda_index])
        results[[result_index]] <- cbind(
          data.frame(model = "Elastic-net logistic regression", config = config,
                     alpha = alpha_value, lambda = lambda_values[lambda_index], fold = fold),
          score_probabilities(y[assessment], predictions[, lambda_index])
        )
        result_index <- result_index + 1L
      }
    }
    rm(x_analysis, x_assessment)
    invisible(gc())
  }
  do.call(rbind, results)
}

default_tree_grid <- function() {
  data.frame(
    cp = c(0.0005, 0.0001),
    maxdepth = c(5L, 8L),
    minsplit = c(500L, 300L)
  )
}

# A single tree is included as a transparent nonlinear benchmark. To keep it
# interpretable and fast, it uses up to 70 EDA-relevant encoded columns.
cross_validate_decision_tree <- function(data, fold_id, grid = default_tree_grid(),
                                         target_col = "TARGET", id_col = "SK_ID_CURR",
                                         max_features = 70L) {
  y <- data[[target_col]]
  results <- list()
  result_index <- 1L
  for (fold in sort(unique(fold_id))) {
    analysis <- fold_id != fold
    assessment <- !analysis
    encoder <- fit_sparse_encoder(data[analysis, , drop = FALSE], target_col, id_col,
                                  numeric_imputation = "median")
    x_analysis <- apply_sparse_encoder(data[analysis, , drop = FALSE], encoder)
    x_assessment <- apply_sparse_encoder(data[assessment, , drop = FALSE], encoder)
    preferred <- grep(
      "EXT_SOURCE|AGE|EMPLOY|ANNUITY|CREDIT_TO|INCOME|REGION_RATING|EDUCATION|OCCUPATION|ORGANIZATION|BUREAU|INSTALLMENT",
      colnames(x_analysis), ignore.case = TRUE
    )
    selected <- head(unique(c(preferred, seq_len(ncol(x_analysis)))), max_features)
    analysis_frame <- as.data.frame(as.matrix(x_analysis[, selected, drop = FALSE]))
    assessment_frame <- as.data.frame(as.matrix(x_assessment[, selected, drop = FALSE]))
    names(analysis_frame) <- make.names(names(analysis_frame), unique = TRUE)
    names(assessment_frame) <- names(analysis_frame)
    analysis_frame$.outcome <- factor(y[analysis], levels = c(0, 1))

    for (grid_row in seq_len(nrow(grid))) {
      fit <- rpart::rpart(
        .outcome ~ ., data = analysis_frame, method = "class",
        control = rpart::rpart.control(
          cp = grid$cp[grid_row], maxdepth = grid$maxdepth[grid_row],
          minsplit = grid$minsplit[grid_row], xval = 0
        )
      )
      probability <- stats::predict(fit, assessment_frame, type = "prob")[, "1"]
      config <- sprintf("cp=%g, depth=%d, minsplit=%d", grid$cp[grid_row],
                        grid$maxdepth[grid_row], grid$minsplit[grid_row])
      results[[result_index]] <- cbind(
        data.frame(model = "Pruned decision tree", config = config, fold = fold),
        score_probabilities(y[assessment], probability)
      )
      result_index <- result_index + 1L
    }
    rm(x_analysis, x_assessment, analysis_frame, assessment_frame)
    invisible(gc())
  }
  do.call(rbind, results)
}

default_xgboost_grid <- function() {
  data.frame(
    max_depth = c(3L, 5L, 7L, 5L),
    eta = c(0.08, 0.05, 0.05, 0.05),
    min_child_weight = c(10, 10, 20, 10),
    subsample = c(0.85, 0.85, 0.85, 0.85),
    colsample_bytree = c(0.85, 0.85, 0.85, 0.85),
    numeric_imputation = c("native", "native", "native", "median"),
    class_weight = c("none", "none", "none", "none"),
    nrounds = c(250L, 350L, 350L, 350L),
    stringsAsFactors = FALSE
  )
}

imbalance_multiplier <- function(y, strategy = c("none", "sqrt", "balanced")) {
  strategy <- match.arg(strategy)
  ratio <- sum(y == 0) / sum(y == 1)
  switch(strategy, none = 1, sqrt = sqrt(ratio), balanced = ratio)
}

predict_xgboost_best <- function(fit, newdata, best_iteration) {
  tryCatch(
    stats::predict(fit, newdata, iterationrange = c(0, as.integer(best_iteration))),
    error = function(e) stats::predict(fit, newdata)
  )
}

cross_validate_xgboost <- function(data, fold_id, grid = default_xgboost_grid(),
                                   target_col = "TARGET", id_col = "SK_ID_CURR",
                                   rare_proportion = 0.001, nthread = 2L,
                                   early_stopping_rounds = 30L) {
  check_modeling_packages()
  y <- data[[target_col]]
  results <- list()
  result_index <- 1L
  for (fold in sort(unique(fold_id))) {
    analysis <- fold_id != fold
    assessment <- !analysis
    for (imputation in unique(grid$numeric_imputation)) {
      encoder <- fit_sparse_encoder(
        data[analysis, , drop = FALSE], target_col, id_col,
        numeric_imputation = imputation, rare_proportion = rare_proportion
      )
      x_analysis <- apply_sparse_encoder(data[analysis, , drop = FALSE], encoder)
      x_assessment <- apply_sparse_encoder(data[assessment, , drop = FALSE], encoder)
      d_analysis <- xgboost::xgb.DMatrix(x_analysis, label = y[analysis], missing = NA)
      d_assessment <- xgboost::xgb.DMatrix(x_assessment, label = y[assessment], missing = NA)
      relevant_rows <- which(grid$numeric_imputation == imputation)

      for (grid_row in relevant_rows) {
        weight <- imbalance_multiplier(y[analysis], grid$class_weight[grid_row])
        parameters <- list(
          objective = "binary:logistic", eval_metric = "auc", tree_method = "hist",
          max_depth = grid$max_depth[grid_row], eta = grid$eta[grid_row],
          min_child_weight = grid$min_child_weight[grid_row],
          subsample = grid$subsample[grid_row],
          colsample_bytree = grid$colsample_bytree[grid_row],
          scale_pos_weight = weight, nthread = nthread
        )
        fit <- xgboost::xgb.train(
          params = parameters, data = d_analysis,
          nrounds = grid$nrounds[grid_row],
          evals = list(validation = d_assessment),
          early_stopping_rounds = early_stopping_rounds,
          verbose = 0
        )
        best_iteration <- if (!is.null(fit$best_iteration)) fit$best_iteration else grid$nrounds[grid_row]
        probability <- predict_xgboost_best(fit, d_assessment, best_iteration)
        config <- sprintf(
          "depth=%d, eta=%.2f, child=%g, impute=%s, weight=%s, rounds=%d",
          grid$max_depth[grid_row], grid$eta[grid_row], grid$min_child_weight[grid_row],
          imputation, grid$class_weight[grid_row], grid$nrounds[grid_row]
        )
        results[[result_index]] <- cbind(
          data.frame(model = "XGBoost", config = config, fold = fold,
                     max_depth = grid$max_depth[grid_row], eta = grid$eta[grid_row],
                     min_child_weight = grid$min_child_weight[grid_row],
                     subsample = grid$subsample[grid_row],
                     colsample_bytree = grid$colsample_bytree[grid_row],
                     numeric_imputation = imputation,
                     class_weight = grid$class_weight[grid_row],
                     nrounds = grid$nrounds[grid_row],
                     best_iteration = best_iteration),
          score_probabilities(y[assessment], probability)
        )
        result_index <- result_index + 1L
      }
      rm(x_analysis, x_assessment, d_analysis, d_assessment)
      invisible(gc())
    }
  }
  do.call(rbind, results)
}

# Generate out-of-fold probabilities for the selected XGBoost configuration.
# These predictions support calibration diagnostics without scoring rows that
# were used to fit their model.
cross_validated_xgboost_predictions <- function(data, fold_id, configuration,
                                                target_col = "TARGET",
                                                id_col = "SK_ID_CURR",
                                                rare_proportion = 0.001,
                                                nthread = 2L,
                                                early_stopping_rounds = 30L) {
  y <- data[[target_col]]
  probability <- rep(NA_real_, nrow(data))
  iterations <- integer(length(unique(fold_id)))
  fold_values <- sort(unique(fold_id))
  for (fold_index in seq_along(fold_values)) {
    fold <- fold_values[fold_index]
    analysis <- fold_id != fold
    assessment <- !analysis
    imputation <- as.character(configuration$numeric_imputation[1])
    encoder <- fit_sparse_encoder(
      data[analysis, , drop = FALSE], target_col, id_col,
      numeric_imputation = imputation, rare_proportion = rare_proportion
    )
    x_analysis <- apply_sparse_encoder(data[analysis, , drop = FALSE], encoder)
    x_assessment <- apply_sparse_encoder(data[assessment, , drop = FALSE], encoder)
    d_analysis <- xgboost::xgb.DMatrix(x_analysis, label = y[analysis], missing = NA)
    d_assessment <- xgboost::xgb.DMatrix(x_assessment, label = y[assessment], missing = NA)
    maximum_rounds <- if ("nrounds" %in% names(configuration)) {
      as.integer(configuration$nrounds[1])
    } else if ("best_iteration_mean" %in% names(configuration)) {
      max(100L, as.integer(round(configuration$best_iteration_mean[1] * 1.25)))
    } else 500L
    fit <- xgboost::xgb.train(
      params = list(
        objective = "binary:logistic", eval_metric = "auc", tree_method = "hist",
        max_depth = as.integer(configuration$max_depth[1]),
        eta = as.numeric(configuration$eta[1]),
        min_child_weight = as.numeric(configuration$min_child_weight[1]),
        subsample = as.numeric(configuration$subsample[1]),
        colsample_bytree = as.numeric(configuration$colsample_bytree[1]),
        scale_pos_weight = imbalance_multiplier(y[analysis], as.character(configuration$class_weight[1])),
        nthread = nthread
      ),
      data = d_analysis, nrounds = maximum_rounds,
      evals = list(validation = d_assessment),
      early_stopping_rounds = early_stopping_rounds, verbose = 0
    )
    iterations[fold_index] <- if (!is.null(fit$best_iteration)) fit$best_iteration else maximum_rounds
    probability[assessment] <- predict_xgboost_best(fit, d_assessment, iterations[fold_index])
    rm(x_analysis, x_assessment, d_analysis, d_assessment)
    invisible(gc())
  }
  data.frame(
    row_id = seq_len(nrow(data)),
    fold = fold_id,
    truth = y,
    probability = probability,
    SK_ID_CURR = data[[id_col]],
    best_iteration = iterations[match(fold_id, fold_values)]
  )
}

calibration_table <- function(truth, probability, groups = 10L) {
  breaks <- unique(stats::quantile(probability, probs = seq(0, 1, length.out = groups + 1),
                                   na.rm = TRUE, names = FALSE))
  band <- cut(probability, breaks = breaks, include.lowest = TRUE,
              labels = FALSE)
  output <- stats::aggregate(
    cbind(predicted = probability, observed = truth) ~ band,
    FUN = mean
  )
  output$n <- as.integer(table(band)[as.character(output$band)])
  output
}

# XGBoost can also grow many independent trees in one boosting round. This is a
# substantially faster full-data random-forest benchmark than the legacy
# randomForest package for this dataset.
cross_validate_xgboost_forest <- function(data, fold_id, target_col = "TARGET",
                                          id_col = "SK_ID_CURR", trees = 100L,
                                          max_depth = 8L, nthread = 2L) {
  y <- data[[target_col]]
  results <- list()
  for (fold in sort(unique(fold_id))) {
    analysis <- fold_id != fold
    assessment <- !analysis
    encoder <- fit_sparse_encoder(data[analysis, , drop = FALSE], target_col, id_col,
                                  numeric_imputation = "native")
    x_analysis <- apply_sparse_encoder(data[analysis, , drop = FALSE], encoder)
    x_assessment <- apply_sparse_encoder(data[assessment, , drop = FALSE], encoder)
    d_analysis <- xgboost::xgb.DMatrix(x_analysis, label = y[analysis], missing = NA)
    fit <- xgboost::xgb.train(
      params = list(
        objective = "binary:logistic", eval_metric = "auc", tree_method = "hist",
        num_parallel_tree = trees, max_depth = max_depth, eta = 1,
        subsample = 0.70, colsample_bynode = 0.70, nthread = nthread
      ),
      data = d_analysis, nrounds = 1, verbose = 0
    )
    probability <- stats::predict(fit, x_assessment)
    results[[fold]] <- cbind(
      data.frame(model = "Random forest (XGBoost engine)",
                 config = sprintf("trees=%d, depth=%d", trees, max_depth), fold = fold),
      score_probabilities(y[assessment], probability)
    )
    rm(x_analysis, x_assessment, d_analysis)
    invisible(gc())
  }
  do.call(rbind, results)
}

make_imbalance_grid <- function(best_row, strategies = c("none", "sqrt", "balanced")) {
  output <- best_row[rep(1, length(strategies)), , drop = FALSE]
  output$class_weight <- strategies
  rownames(output) <- NULL
  output
}

select_best_configuration <- function(cv_results) {
  summary <- summarize_cv_results(cv_results)
  summary[which.max(summary$roc_auc_mean), , drop = FALSE]
}

# Refit a selected XGBoost configuration on every labeled row, then create test
# probabilities in the exact original application_test row order.
fit_final_xgboost <- function(train_data, test_data, configuration,
                              target_col = "TARGET", id_col = "SK_ID_CURR",
                              final_nrounds = NULL, nthread = 2L) {
  y <- train_data[[target_col]]
  imputation <- as.character(configuration$numeric_imputation[1])
  encoder <- fit_sparse_encoder(train_data, target_col, id_col,
                                numeric_imputation = imputation)
  x_train <- apply_sparse_encoder(train_data, encoder)
  x_test <- apply_sparse_encoder(test_data, encoder)
  d_train <- xgboost::xgb.DMatrix(x_train, label = y, missing = NA)
  rounds <- if (!is.null(final_nrounds)) {
    as.integer(final_nrounds)
  } else if ("best_iteration_mean" %in% names(configuration)) {
    max(1L, as.integer(round(configuration$best_iteration_mean[1])))
  } else if ("nrounds" %in% names(configuration)) {
    as.integer(configuration$nrounds[1])
  } else 350L
  fit <- xgboost::xgb.train(
    params = list(
      objective = "binary:logistic", eval_metric = "auc", tree_method = "hist",
      max_depth = as.integer(configuration$max_depth[1]),
      eta = as.numeric(configuration$eta[1]),
      min_child_weight = as.numeric(configuration$min_child_weight[1]),
      subsample = as.numeric(configuration$subsample[1]),
      colsample_bytree = as.numeric(configuration$colsample_bytree[1]),
      scale_pos_weight = imbalance_multiplier(y, as.character(configuration$class_weight[1])),
      nthread = nthread
    ),
    data = d_train, nrounds = rounds, verbose = 0
  )
  probability <- as.numeric(stats::predict(fit, x_test))
  submission <- data.frame(
    SK_ID_CURR = test_data[[id_col]],
    TARGET = probability
  )
  if (nrow(submission) != nrow(test_data) || anyNA(submission) ||
      any(submission$TARGET < 0 | submission$TARGET > 1)) {
    stop("Final prediction validation failed.")
  }
  list(model = fit, encoder = encoder, nrounds = rounds, submission = submission)
}

save_modeling_result <- function(object, path) {
  directory <- dirname(path)
  if (!dir.exists(directory)) dir.create(directory, recursive = TRUE)
  saveRDS(object, path)
  invisible(path)
}

# Build supplemental aggregates one source at a time to limit peak memory. The
# enriched application tables are cached because the source CSVs exceed 2 GB.
load_or_build_enriched_applications <- function(train_raw, test_raw, data_dir = ".",
                                                cache_path = file.path("Model Outputs", "enriched_applications.rds")) {
  if (file.exists(cache_path)) return(readRDS(cache_path))
  if (!exists("aggregate_bureau_features", mode = "function")) {
    stop("Source Scripts/data_preparation.R before building supplemental features.")
  }
  read_csv <- function(filename) {
    readr::read_csv(file.path(data_dir, filename), show_col_types = FALSE, progress = FALSE)
  }

  bureau <- read_csv("bureau.csv")
  bureau_features <- aggregate_bureau_features(bureau)
  bureau_balance <- read_csv("bureau_balance.csv")
  bureau_balance_features <- aggregate_bureau_balance_features(bureau, bureau_balance)
  rm(bureau_balance, bureau)
  invisible(gc())

  previous_features <- aggregate_previous_application_features(read_csv("previous_application.csv"))
  invisible(gc())
  installment_features <- aggregate_installment_features(read_csv("installments_payments.csv"))
  invisible(gc())
  pos_features <- aggregate_monthly_balance_features(
    read_csv("POS_CASH_balance.csv"), "POS", FALSE
  )
  invisible(gc())
  card_features <- aggregate_monthly_balance_features(
    read_csv("credit_card_balance.csv"), "CREDIT_CARD", TRUE
  )
  invisible(gc())

  features <- list(bureau_features, bureau_balance_features, previous_features,
                   installment_features, pos_features, card_features)
  enriched <- join_supplementary_to_train_test(train_raw, test_raw, features)
  save_modeling_result(enriched, cache_path)
  enriched
}

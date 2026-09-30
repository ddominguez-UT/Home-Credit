# Reusable preprocessing for the Home Credit application tables
#
# Usage:
#   train_raw <- read.csv("application_train.csv", stringsAsFactors = FALSE)
#   test_raw  <- read.csv("application_test.csv", stringsAsFactors = FALSE)
#   prep <- fit_application_preprocessor(train_raw)
#   train_ready <- apply_application_preprocessor(train_raw, prep)
#   test_ready  <- apply_application_preprocessor(test_raw, prep)
#
# -----------------------------------------------------------------------------
# Phase 1: Clean and transform the application data
# -----------------------------------------------------------------------------

# Return the most frequent non-missing value; resolve ties alphabetically.
most_common_value <- function(x) {
  x <- as.character(x)
  x <- x[!is.na(x) & nzchar(x)]
  if (!length(x)) return(NA_character_)

  counts <- sort(table(x), decreasing = TRUE)
  candidates <- names(counts)[counts == counts[1]]
  sort(candidates)[1]
}

# Return NA for missing, zero, or negative denominators.
safe_ratio <- function(numerator, denominator) {
  answer <- rep(NA_real_, length(numerator))
  valid <- !is.na(numerator) & !is.na(denominator) & denominator > 0
  answer[valid] <- numerator[valid] / denominator[valid]
  answer
}

# Add a ratio only when its source columns are available.
add_ratio_if_available <- function(data, output_name, numerator, denominator) {
  if (all(c(numerator, denominator) %in% names(data))) {
    data[[output_name]] <- safe_ratio(data[[numerator]], data[[denominator]])
  }
  data
}

# Clean and engineer features consistently for train and test data.
clean_application_data <- function(data, target_col = "TARGET") {
  if (!is.data.frame(data)) stop("`data` must be a data frame.")
  if (!"SK_ID_CURR" %in% names(data)) stop("`data` must contain `SK_ID_CURR`.")

  data <- as.data.frame(data, stringsAsFactors = FALSE)
  data[] <- lapply(data, function(x) if (is.factor(x)) as.character(x) else x)

  if (anyDuplicated(data$SK_ID_CURR)) {
    stop("`SK_ID_CURR` must uniquely identify each application.")
  }

  # EDA decision: 365243 is an employment-duration sentinel; retain an indicator.
  if ("DAYS_EMPLOYED" %in% names(data)) {
    data$DAYS_EMPLOYED_ANOMALY <- as.integer(data$DAYS_EMPLOYED == 365243)
    data$DAYS_EMPLOYED[data$DAYS_EMPLOYED == 365243] <- NA_real_
    data$YEARS_EMPLOYED <- abs(data$DAYS_EMPLOYED) / 365.25
  }

  # EDA decision: convert negative day offsets to positive, interpretable years.
  day_year_map <- c(
    DAYS_BIRTH = "AGE_YEARS",
    DAYS_REGISTRATION = "YEARS_SINCE_REGISTRATION",
    DAYS_ID_PUBLISH = "YEARS_SINCE_ID_PUBLISH",
    DAYS_LAST_PHONE_CHANGE = "YEARS_SINCE_PHONE_CHANGE"
  )
  for (source_name in names(day_year_map)) {
    if (source_name %in% names(data)) {
      data[[day_year_map[[source_name]]]] <- abs(data[[source_name]]) / 365.25
    }
  }

  # EDA decision: ratings outside the documented 1--3 range are invalid.
  if ("REGION_RATING_CLIENT_W_CITY" %in% names(data)) {
    invalid_rating <- !is.na(data$REGION_RATING_CLIENT_W_CITY) &
      !(data$REGION_RATING_CLIENT_W_CITY %in% 1:3)
    data$REGION_RATING_CLIENT_W_CITY[invalid_rating] <- NA_real_
  }

  # EDA decision: recode `XNA` and `Unknown` as missing before imputation.
  if ("CODE_GENDER" %in% names(data)) {
    data$CODE_GENDER[data$CODE_GENDER == "XNA"] <- NA_character_
  }
  if ("NAME_FAMILY_STATUS" %in% names(data)) {
    data$NAME_FAMILY_STATUS[data$NAME_FAMILY_STATUS == "Unknown"] <- NA_character_
  }

  # EDA decision: retain raw monetary values and add log1p versions for skew.
  money_columns <- intersect(
    c("AMT_INCOME_TOTAL", "AMT_CREDIT", "AMT_ANNUITY", "AMT_GOODS_PRICE"),
    names(data)
  )
  for (column in money_columns) {
    data[[column]][!is.na(data[[column]]) & data[[column]] < 0] <- NA_real_
    data[[paste0("LOG_", column)]] <- log1p(data[[column]])
  }

  # EDA decision: add scale-free application-time loan-burden ratios.
  data <- add_ratio_if_available(data, "CREDIT_TO_INCOME", "AMT_CREDIT", "AMT_INCOME_TOTAL")
  data <- add_ratio_if_available(data, "ANNUITY_TO_INCOME", "AMT_ANNUITY", "AMT_INCOME_TOTAL")
  data <- add_ratio_if_available(data, "CREDIT_TO_GOODS_PRICE", "AMT_CREDIT", "AMT_GOODS_PRICE")
  data <- add_ratio_if_available(data, "ANNUITY_TO_CREDIT", "AMT_ANNUITY", "AMT_CREDIT")

  if (target_col %in% names(data) && !is.numeric(data[[target_col]])) {
    stop("`", target_col, "` must be numeric when it is present.")
  }
  data
}

# Fit training-only imputation values and predictor schema.
fit_application_preprocessor <- function(train_data,
                                         target_col = "TARGET",
                                         id_col = "SK_ID_CURR") {
  if (!target_col %in% names(train_data)) {
    stop("Training data must contain `", target_col, "`.")
  }
  cleaned <- clean_application_data(train_data, target_col)
  if (anyNA(cleaned[[target_col]])) stop("Training target contains missing values.")

  predictors <- setdiff(names(cleaned), c(id_col, target_col))
  numeric_columns <- predictors[vapply(cleaned[predictors], is.numeric, logical(1))]
  categorical_columns <- setdiff(predictors, numeric_columns)

  numeric_fill <- vapply(cleaned[numeric_columns], function(x) {
    value <- stats::median(x, na.rm = TRUE)
    if (is.nan(value)) 0 else value
  }, numeric(1))
  categorical_fill <- vapply(cleaned[categorical_columns], most_common_value, character(1))
  categorical_fill[is.na(categorical_fill)] <- "Missing"

  structure(list(
    target_col = target_col,
    id_col = id_col,
    predictor_columns = predictors,
    numeric_columns = numeric_columns,
    categorical_columns = categorical_columns,
    numeric_fill = numeric_fill,
    categorical_fill = categorical_fill
  ), class = "home_credit_preprocessor")
}

# Apply the fitted preprocessing to train, validation, or test data.
apply_application_preprocessor <- function(data, preprocessor) {
  if (!inherits(preprocessor, "home_credit_preprocessor")) {
    stop("`preprocessor` must be created by `fit_application_preprocessor()`.")
  }
  cleaned <- clean_application_data(data, preprocessor$target_col)
  missing_predictors <- setdiff(preprocessor$predictor_columns, names(cleaned))
  if (length(missing_predictors)) {
    stop("Input is missing predictor columns: ", paste(missing_predictors, collapse = ", "))
  }

  # Keep training predictor order and exclude extra columns.
  result <- cleaned[, preprocessor$predictor_columns, drop = FALSE]
  for (column in preprocessor$numeric_columns) {
    result[[column]][is.na(result[[column]])] <- preprocessor$numeric_fill[[column]]
  }
  for (column in preprocessor$categorical_columns) {
    values <- as.character(result[[column]])
    values[is.na(values) | !nzchar(values)] <- preprocessor$categorical_fill[[column]]
    result[[column]] <- values
  }

  # Keep the identifier and, when present, the training target outside predictors.
  output <- data.frame(cleaned[[preprocessor$id_col]], result, check.names = FALSE)
  names(output)[1] <- preprocessor$id_col
  if (preprocessor$target_col %in% names(cleaned)) {
    output[[preprocessor$target_col]] <- cleaned[[preprocessor$target_col]]
  }
  output
}

# Convenience wrapper for the standard Home Credit train/test workflow.
prepare_train_test_data <- function(train_data, test_data,
                                    target_col = "TARGET", id_col = "SK_ID_CURR") {
  preprocessor <- fit_application_preprocessor(train_data, target_col, id_col)
  list(
    preprocessor = preprocessor,
    train = apply_application_preprocessor(train_data, preprocessor),
    test = apply_application_preprocessor(test_data, preprocessor)
  )
}


# -----------------------------------------------------------------------------
# Additional EDA-driven feature engineering
# These functions extend, rather than replace, the preprocessing workflow above.
# -----------------------------------------------------------------------------

# Create stable categorical bins so train and test use identical definitions.
make_feature_band <- function(x, breaks, labels) {
  as.character(cut(x, breaks = breaks, labels = labels, include.lowest = TRUE))
}

# Add missingness features for the fields whose completeness was informative in
# the EDA. Property coverage is represented by one count to avoid redundant
# indicators for closely related building fields.
add_eda_missing_indicators <- function(data) {
  indicator_columns <- intersect(
    c(
      "EXT_SOURCE_1", "EXT_SOURCE_2", "EXT_SOURCE_3", "OCCUPATION_TYPE",
      "OWN_CAR_AGE", "AMT_ANNUITY", "AMT_GOODS_PRICE",
      "OBS_30_CNT_SOCIAL_CIRCLE", "DEF_30_CNT_SOCIAL_CIRCLE",
      "OBS_60_CNT_SOCIAL_CIRCLE", "DEF_60_CNT_SOCIAL_CIRCLE"
    ),
    names(data)
  )

  for (column in indicator_columns) {
    data[[paste0("MISSING_", column)]] <- as.integer(is.na(data[[column]]))
  }

  property_columns <- grep("(_AVG|_MODE|_MEDI)$|^TOTALAREA_MODE$", names(data), value = TRUE)
  if (length(property_columns)) {
    data$PROPERTY_MISSING_COUNT <- rowSums(is.na(data[property_columns]))
  }
  data
}

# Create EDA-driven features from either application table. This function first
# applies the original cleaning rules, then adds only application-time features.
engineer_application_features <- function(data, target_col = "TARGET") {
  data <- clean_application_data(data, target_col)

  # EDA decision: age, employment tenure, payment burden, and yearly inquiries
  # show nonlinear patterns, so retain both their numeric and binned forms.
  if ("AGE_YEARS" %in% names(data)) {
    data$AGE_BAND <- make_feature_band(
      data$AGE_YEARS, c(0, 25, 35, 45, 55, 65, Inf),
      c("under_25", "25_34", "35_44", "45_54", "55_64", "65_plus")
    )
  }
  if ("YEARS_EMPLOYED" %in% names(data)) {
    data$EMPLOYMENT_TENURE_BAND <- make_feature_band(
      data$YEARS_EMPLOYED, c(-Inf, 1, 3, 5, 10, 20, Inf),
      c("up_to_1", "1_to_3", "3_to_5", "5_to_10", "10_to_20", "over_20")
    )
  }
  if ("AMT_REQ_CREDIT_BUREAU_YEAR" %in% names(data)) {
    data$BUREAU_INQUIRIES_YEAR_BAND <- make_feature_band(
      data$AMT_REQ_CREDIT_BUREAU_YEAR, c(-Inf, 0, 1, 2, 5, Inf),
      c("0", "1", "2", "3_to_5", "over_5")
    )
  }
  if ("ANNUITY_TO_INCOME" %in% names(data)) {
    data$ANNUITY_TO_INCOME_BAND <- make_feature_band(
      data$ANNUITY_TO_INCOME, c(-Inf, 0.10, 0.20, 0.30, 0.50, Inf),
      c("up_to_10pct", "10_to_20pct", "20_to_30pct", "30_to_50pct", "over_50pct")
    )
  }

  # EDA decision: combine related capacity and ownership signals.
  if (all(c("LOG_AMT_INCOME_TOTAL", "ANNUITY_TO_INCOME") %in% names(data))) {
    data$LOG_INCOME_X_ANNUITY_TO_INCOME <-
      data$LOG_AMT_INCOME_TOTAL * data$ANNUITY_TO_INCOME
  }
  if (all(c("YEARS_EMPLOYED", "AGE_YEARS") %in% names(data))) {
    data$EMPLOYMENT_TO_AGE_RATIO <- safe_ratio(data$YEARS_EMPLOYED, data$AGE_YEARS)
  }
  if (all(c("FLAG_OWN_CAR", "FLAG_OWN_REALTY") %in% names(data))) {
    data$OWN_CAR_REALTY_COMBINATION <- paste(data$FLAG_OWN_CAR, data$FLAG_OWN_REALTY, sep = "_")
  }

  # EDA decision: score availability contains information. External scores
  # remain subject to the governance restriction recorded in the EDA.
  external_scores <- intersect(c("EXT_SOURCE_1", "EXT_SOURCE_2", "EXT_SOURCE_3"), names(data))
  if (length(external_scores)) {
    score_data <- data[external_scores]
    data$EXT_SOURCE_AVAILABLE_COUNT <- rowSums(!is.na(score_data))
    data$EXT_SOURCE_MISSING_COUNT <- rowSums(is.na(score_data))
    data$EXT_SOURCE_MEAN <- rowMeans(score_data, na.rm = TRUE)
    data$EXT_SOURCE_MEAN[data$EXT_SOURCE_AVAILABLE_COUNT == 0] <- NA_real_
  }

  add_eda_missing_indicators(data)
}

# Prepare engineered training and test data with training-only imputation.
prepare_engineered_train_test_data <- function(train_data, test_data,
                                               target_col = "TARGET",
                                               id_col = "SK_ID_CURR") {
  train_engineered <- engineer_application_features(train_data, target_col)
  test_engineered <- engineer_application_features(test_data, target_col)
  preprocessor <- fit_application_preprocessor(train_engineered, target_col, id_col)

  list(
    preprocessor = preprocessor,
    train = apply_application_preprocessor(train_engineered, preprocessor),
    test = apply_application_preprocessor(test_engineered, preprocessor)
  )
}


# -----------------------------------------------------------------------------
# Phase 3: Process supplementary application-history tables
# -----------------------------------------------------------------------------

# Read selected supplemental CSV files.  Functions below also accept data frames
# directly, which makes them reusable after data has already been loaded.
read_supplemental_tables <- function(data_dir = ".") {
  files <- c(
    bureau = "bureau.csv",
    bureau_balance = "bureau_balance.csv",
    credit_card_balance = "credit_card_balance.csv",
    installments_payments = "installments_payments.csv",
    POS_CASH_balance = "POS_CASH_balance.csv",
    previous_application = "previous_application.csv"
  )
  paths <- file.path(data_dir, files)
  missing_files <- paths[!file.exists(paths)]
  if (length(missing_files)) {
    stop("Missing supplemental files: ", paste(basename(missing_files), collapse = ", "))
  }
  lapply(paths, readr::read_csv, show_col_types = FALSE, progress = FALSE)
}

# Check that an aggregate table can be safely joined to application data.
assert_unique_curr_key <- function(data, table_name) {
  if (!"SK_ID_CURR" %in% names(data)) {
    stop("`", table_name, "` must contain `SK_ID_CURR`.")
  }
  if (anyNA(data$SK_ID_CURR) || anyDuplicated(data$SK_ID_CURR)) {
    stop("`", table_name, "` must have one non-missing row per `SK_ID_CURR`.")
  }
  invisible(data)
}

# Convert bureau monthly status codes to a delinquency flag. C, X, and 0 are not
# counted as delinquent; numeric codes 1--5 represent increasing delinquency.
bureau_status_is_delinquent <- function(status) {
  as.integer(as.character(status) %in% c("1", "2", "3", "4", "5"))
}

# Aggregate bureau credit records that predate the current application.
aggregate_bureau_features <- function(bureau) {
  required <- c("SK_ID_CURR", "SK_ID_BUREAU", "DAYS_CREDIT", "CREDIT_ACTIVE",
                "AMT_CREDIT_SUM", "AMT_CREDIT_SUM_DEBT", "AMT_CREDIT_SUM_OVERDUE")
  if (!all(required %in% names(bureau))) stop("`bureau` is missing required columns.")

  bureau |>
    dplyr::filter(DAYS_CREDIT < 0) |>
    dplyr::mutate(
      bureau_active = as.integer(CREDIT_ACTIVE == "Active"),
      bureau_debt_to_credit = ifelse(AMT_CREDIT_SUM > 0, AMT_CREDIT_SUM_DEBT / AMT_CREDIT_SUM, NA_real_)
    ) |>
    dplyr::group_by(SK_ID_CURR) |>
    dplyr::summarise(
      BUREAU_RECORD_COUNT = dplyr::n(),
      BUREAU_ACTIVE_COUNT = sum(bureau_active, na.rm = TRUE),
      BUREAU_CREDIT_SUM = sum(AMT_CREDIT_SUM, na.rm = TRUE),
      BUREAU_DEBT_SUM = sum(AMT_CREDIT_SUM_DEBT, na.rm = TRUE),
      BUREAU_OVERDUE_SUM = sum(AMT_CREDIT_SUM_OVERDUE, na.rm = TRUE),
      BUREAU_MEAN_DEBT_TO_CREDIT = mean(bureau_debt_to_credit, na.rm = TRUE),
      BUREAU_MOST_RECENT_DAYS_CREDIT = max(DAYS_CREDIT, na.rm = TRUE),
      .groups = "drop"
    )
}

# Aggregate monthly bureau history after linking it to prior bureau records.
aggregate_bureau_balance_features <- function(bureau, bureau_balance) {
  required_bureau <- c("SK_ID_CURR", "SK_ID_BUREAU", "DAYS_CREDIT")
  required_balance <- c("SK_ID_BUREAU", "MONTHS_BALANCE", "STATUS")
  if (!all(required_bureau %in% names(bureau)) || !all(required_balance %in% names(bureau_balance))) {
    stop("`bureau` or `bureau_balance` is missing required columns.")
  }

  bureau |>
    dplyr::filter(DAYS_CREDIT < 0) |>
    dplyr::select(SK_ID_CURR, SK_ID_BUREAU) |>
    dplyr::inner_join(bureau_balance, by = "SK_ID_BUREAU") |>
    dplyr::filter(MONTHS_BALANCE < 0) |>
    dplyr::mutate(bureau_balance_dpd = bureau_status_is_delinquent(STATUS)) |>
    dplyr::group_by(SK_ID_CURR) |>
    dplyr::summarise(
      BUREAU_BALANCE_MONTH_COUNT = dplyr::n(),
      BUREAU_BALANCE_ACCOUNT_COUNT = dplyr::n_distinct(SK_ID_BUREAU),
      BUREAU_BALANCE_DPD_MONTH_COUNT = sum(bureau_balance_dpd, na.rm = TRUE),
      BUREAU_BALANCE_MOST_RECENT_MONTH = max(MONTHS_BALANCE, na.rm = TRUE),
      .groups = "drop"
    )
}

# Aggregate prior applications decided before the current application.
aggregate_previous_application_features <- function(previous_application) {
  required <- c("SK_ID_CURR", "SK_ID_PREV", "DAYS_DECISION", "NAME_CONTRACT_STATUS",
                "AMT_APPLICATION", "AMT_CREDIT")
  if (!all(required %in% names(previous_application))) {
    stop("`previous_application` is missing required columns.")
  }

  previous_application |>
    dplyr::filter(DAYS_DECISION < 0) |>
    dplyr::mutate(
      previous_approved = as.integer(NAME_CONTRACT_STATUS == "Approved"),
      previous_refused = as.integer(NAME_CONTRACT_STATUS == "Refused"),
      previous_application_to_credit = ifelse(AMT_CREDIT > 0, AMT_APPLICATION / AMT_CREDIT, NA_real_)
    ) |>
    dplyr::group_by(SK_ID_CURR) |>
    dplyr::summarise(
      PREVIOUS_APPLICATION_COUNT = dplyr::n(),
      PREVIOUS_APPROVED_COUNT = sum(previous_approved, na.rm = TRUE),
      PREVIOUS_REFUSED_COUNT = sum(previous_refused, na.rm = TRUE),
      PREVIOUS_MEAN_CREDIT = mean(AMT_CREDIT, na.rm = TRUE),
      PREVIOUS_MEAN_APPLICATION_TO_CREDIT = mean(previous_application_to_credit, na.rm = TRUE),
      PREVIOUS_MOST_RECENT_DAYS_DECISION = max(DAYS_DECISION, na.rm = TRUE),
      .groups = "drop"
    )
}

# Aggregate scheduled installments known before the current application.
aggregate_installment_features <- function(installments_payments) {
  required <- c("SK_ID_CURR", "DAYS_INSTALMENT", "DAYS_ENTRY_PAYMENT",
                "AMT_INSTALMENT", "AMT_PAYMENT")
  if (!all(required %in% names(installments_payments))) {
    stop("`installments_payments` is missing required columns.")
  }

  installments_payments |>
    dplyr::filter(DAYS_INSTALMENT < 0) |>
    dplyr::mutate(
      days_late = DAYS_ENTRY_PAYMENT - DAYS_INSTALMENT,
      late_payment = as.integer(!is.na(days_late) & days_late > 0),
      payment_ratio = ifelse(AMT_INSTALMENT > 0, AMT_PAYMENT / AMT_INSTALMENT, NA_real_)
    ) |>
    dplyr::group_by(SK_ID_CURR) |>
    dplyr::summarise(
      INSTALLMENT_COUNT = dplyr::n(),
      INSTALLMENT_LATE_PAYMENT_COUNT = sum(late_payment, na.rm = TRUE),
      INSTALLMENT_MEAN_DAYS_LATE = mean(days_late, na.rm = TRUE),
      INSTALLMENT_MEAN_PAYMENT_RATIO = mean(payment_ratio, na.rm = TRUE),
      .groups = "drop"
    )
}

# Aggregate monthly POS cash or credit-card history. A function parameter keeps
# the shared timing and delinquency rules consistent across both table types.
aggregate_monthly_balance_features <- function(data, prefix, include_utilization = FALSE) {
  required <- c("SK_ID_CURR", "MONTHS_BALANCE", "SK_DPD", "SK_DPD_DEF")
  if (!all(required %in% names(data))) stop("Monthly balance table is missing required columns.")

  data <- data |>
    dplyr::filter(MONTHS_BALANCE < 0) |>
    dplyr::mutate(monthly_dpd = as.integer(SK_DPD > 0 | SK_DPD_DEF > 0))

  if (include_utilization) {
    if (!all(c("AMT_BALANCE", "AMT_CREDIT_LIMIT_ACTUAL") %in% names(data))) {
      stop("Credit-card balance table is missing utilization columns.")
    }
    data <- dplyr::mutate(
      data,
      credit_utilization = ifelse(AMT_CREDIT_LIMIT_ACTUAL > 0, AMT_BALANCE / AMT_CREDIT_LIMIT_ACTUAL, NA_real_)
    )
  }

  summary <- data |>
    dplyr::group_by(SK_ID_CURR) |>
    dplyr::summarise(
      month_count = dplyr::n(),
      dpd_month_count = sum(monthly_dpd, na.rm = TRUE),
      max_dpd = max(SK_DPD, na.rm = TRUE),
      most_recent_month = max(MONTHS_BALANCE, na.rm = TRUE),
      .groups = "drop"
    )

  if (include_utilization) {
    utilization <- data |>
      dplyr::group_by(SK_ID_CURR) |>
      dplyr::summarise(mean_utilization = mean(credit_utilization, na.rm = TRUE), .groups = "drop")
    summary <- dplyr::left_join(summary, utilization, by = "SK_ID_CURR")
  }

  names(summary)[names(summary) != "SK_ID_CURR"] <- paste0(prefix, "_", toupper(names(summary)[names(summary) != "SK_ID_CURR"]))
  summary
}

# Join one-row-per-applicant supplemental features without changing application
# row order. Missing feature values indicate no observable prior history.
join_supplemental_features <- function(application_data, feature_tables) {
  if (!is.data.frame(application_data)) stop("`application_data` must be a data frame.")
  if (!"SK_ID_CURR" %in% names(application_data)) stop("Application data must contain `SK_ID_CURR`.")

  output <- application_data
  for (feature_table in feature_tables) {
    assert_unique_curr_key(feature_table, "supplemental feature table")
    feature_names <- setdiff(names(feature_table), "SK_ID_CURR")
    if (any(feature_names %in% names(output))) {
      stop("Feature-name collision: ", paste(intersect(feature_names, names(output)), collapse = ", "))
    }
    match_index <- match(output$SK_ID_CURR, feature_table$SK_ID_CURR)
    output[feature_names] <- feature_table[match_index, feature_names, drop = FALSE]
  }
  output
}

# Build all supplemental feature tables. Input is a named list returned by
# `read_supplemental_tables()` or an equivalent list of data frames.
build_supplemental_features <- function(supplemental_tables) {
  required_tables <- c("bureau", "bureau_balance", "credit_card_balance",
                       "installments_payments", "POS_CASH_balance", "previous_application")
  missing_tables <- setdiff(required_tables, names(supplemental_tables))
  if (length(missing_tables)) {
    stop("Missing supplemental tables: ", paste(missing_tables, collapse = ", "))
  }

  list(
    aggregate_bureau_features(supplemental_tables$bureau),
    aggregate_bureau_balance_features(supplemental_tables$bureau, supplemental_tables$bureau_balance),
    aggregate_previous_application_features(supplemental_tables$previous_application),
    aggregate_installment_features(supplemental_tables$installments_payments),
    aggregate_monthly_balance_features(supplemental_tables$POS_CASH_balance, "POS", FALSE),
    aggregate_monthly_balance_features(supplemental_tables$credit_card_balance, "CREDIT_CARD", TRUE)
  )
}


# -----------------------------------------------------------------------------
# Phase 4: Join aggregated supplementary tables to application tables
# -----------------------------------------------------------------------------

# Join pre-aggregated supplemental features to one application table. The
# aggregate tables must have one row per `SK_ID_CURR`; this preserves the
# application-table grain and keeps applicants without history as missing.
join_aggregated_supplementary_tables <- function(application_data,
                                                  aggregated_feature_tables) {
  join_supplemental_features(application_data, aggregated_feature_tables)
}

# Apply the same supplementary feature tables to train and test application
# data. No target values are used during the join, so the function is safe for
# both datasets and maintains their original row order.
join_supplementary_to_train_test <- function(train_data, test_data,
                                              aggregated_feature_tables) {
  list(
    train = join_aggregated_supplementary_tables(train_data, aggregated_feature_tables),
    test = join_aggregated_supplementary_tables(test_data, aggregated_feature_tables)
  )
}


# -----------------------------------------------------------------------------
# Phase 5: Ensure train/test consistency with training-fitted transformations
# -----------------------------------------------------------------------------

# Learn quantile-bin cut points from training data only. Duplicate quantiles are
# removed so variables with repeated values do not create invalid intervals.
fit_quantile_bin_specification <- function(data, columns, bins = 5) {
  available_columns <- intersect(columns, names(data))
  specifications <- list()

  for (column in available_columns) {
    values <- data[[column]]
    values <- values[is.finite(values)]
    if (length(values) < 2 || length(unique(values)) < 2) next

    quantiles <- unique(stats::quantile(
      values,
      probs = seq(0, 1, length.out = bins + 1),
      na.rm = TRUE,
      names = FALSE
    ))
    if (length(quantiles) < 3) next

    breaks <- c(-Inf, quantiles[-c(1, length(quantiles))], Inf)
    specifications[[column]] <- list(
      output_name = paste0("TRAIN_Q_BAND_", column),
      breaks = breaks,
      labels = paste0("band_", seq_len(length(breaks) - 1))
    )
  }
  specifications
}

# Apply previously learned training cut points without recalculating them from
# validation or test data.
apply_quantile_bin_specification <- function(data, bin_specification) {
  for (specification in bin_specification) {
    source_column <- sub("^TRAIN_Q_BAND_", "", specification$output_name)
    if (source_column %in% names(data)) {
      data[[specification$output_name]] <- as.character(cut(
        data[[source_column]],
        breaks = specification$breaks,
        labels = specification$labels,
        include.lowest = TRUE
      ))
    }
  }
  data
}

# Fit all Phase 5 values using training data only. Median/mode imputation is
# stored in `application_preprocessor`; bin thresholds are stored separately.
fit_consistent_preprocessor <- function(train_data, target_col = "TARGET",
                                        id_col = "SK_ID_CURR") {
  engineered_train <- engineer_application_features(train_data, target_col)
  bin_specification <- fit_quantile_bin_specification(
    engineered_train,
    c("AMT_INCOME_TOTAL", "ANNUITY_TO_INCOME", "CREDIT_TO_INCOME")
  )
  engineered_train <- apply_quantile_bin_specification(engineered_train, bin_specification)
  application_preprocessor <- fit_application_preprocessor(
    engineered_train, target_col, id_col
  )

  structure(list(
    target_col = target_col,
    id_col = id_col,
    bin_specification = bin_specification,
    application_preprocessor = application_preprocessor
  ), class = "home_credit_consistent_preprocessor")
}

# Apply the complete training-fitted workflow to any application data. The same
# fitted medians, category modes, and bin thresholds are used for train/test.
apply_consistent_preprocessor <- function(data, preprocessor) {
  if (!inherits(preprocessor, "home_credit_consistent_preprocessor")) {
    stop("`preprocessor` must be created by `fit_consistent_preprocessor()`.")
  }
  engineered_data <- engineer_application_features(data, preprocessor$target_col)
  engineered_data <- apply_quantile_bin_specification(
    engineered_data, preprocessor$bin_specification
  )
  apply_application_preprocessor(engineered_data, preprocessor$application_preprocessor)
}

# Confirm that train and test contain the same predictors in the same order.
validate_train_test_consistency <- function(train_data, test_data,
                                            target_col = "TARGET", id_col = "SK_ID_CURR") {
  train_predictors <- setdiff(names(train_data), c(id_col, target_col))
  test_predictors <- setdiff(names(test_data), id_col)
  identical_schema <- identical(train_predictors, test_predictors)
  if (!identical_schema) {
    stop("Train/test predictor schemas differ after preprocessing.")
  }
  if (anyNA(train_data) || anyNA(test_data)) {
    stop("Missing values remain after preprocessing.")
  }
  list(
    train_rows = nrow(train_data),
    test_rows = nrow(test_data),
    predictor_count = length(train_predictors),
    identical_predictor_schema = identical_schema
  )
}

# One-step final workflow for raw or Phase-4-enriched application tables.
prepare_consistent_train_test_data <- function(train_data, test_data,
                                               target_col = "TARGET",
                                               id_col = "SK_ID_CURR") {
  preprocessor <- fit_consistent_preprocessor(train_data, target_col, id_col)
  train <- apply_consistent_preprocessor(train_data, preprocessor)
  test <- apply_consistent_preprocessor(test_data, preprocessor)
  validation <- validate_train_test_consistency(train, test, target_col, id_col)

  list(
    preprocessor = preprocessor,
    train = train,
    test = test,
    validation = validation
  )
}

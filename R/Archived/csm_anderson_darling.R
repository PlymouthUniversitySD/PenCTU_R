#Title: Anderson-Darling Function
#Author: Paigan Aspinall
#Version & Date: V1.1.0 12AUG2026
#R version: 4.4.3

#' Run Anderson-Darling distribution tests by site
#'
#' This function compares each site's distribution with the rest of the study
#' population for required continuous and derived numeric variables using the
#' Anderson-Darling test.
#'
#' Non-numeric variables, including Date variables, are excluded automatically.
#'
#' @param data A REDCap export dataset.
#' @param metadata A critical data item metadata dataframe.
#' @param variable_type_column Column name in metadata containing variable type.
#' @param required_column Column name in metadata indicating whether the field is required.
#' @param field_name_column Column name in metadata containing REDCap field names.
#' @param site_column Column name in dataset representing site/group.
#' @param min_site_n Minimum number of records required at the site.
#' @param min_rest_n Minimum number of records required in the rest of study.
#' @param min_unique_values Minimum number of unique values required in each comparison group.
#' @param p_threshold P-value threshold used to flag results.
#'
#' @return A list containing:
#' \itemize{
#'   \item \code{ad_results}: all Anderson-Darling test results
#'   \item \code{ad_flags}: results with p-value below threshold
#'   \item \code{skipped_tests}: tests that could not be run and the reason
#'   \item \code{excluded_variables}: metadata variables excluded because they
#'   were absent from the dataset or were not numeric
#' }
#'
#' @importFrom dplyr "%>%"
#'
#' @examples
#' ad_outputs <- ad_distribution_analysis(data, metadata)
#' ad_results <- ad_outputs$ad_results
#' ad_flags <- ad_outputs$ad_flags
#'
#' @export

ad_distribution_analysis <- function(
    data,
    metadata,
    variable_type_column = "variable_type",
    required_column = "required_yn",
    field_name_column = "field_name",
    site_column = "redcap_data_access_group",
    min_site_n = 5,
    min_rest_n = 5,
    min_unique_values = 4,
    p_threshold = 0.05) {
  
  # Check site column exists
  if (!site_column %in% names(data)) {
    stop(
      paste0(
        "Site column '",
        site_column,
        "' was not found in the dataset."
      )
    )
  }
  
  # Identify variables requested by metadata
  requested_vars <- metadata %>%
    dplyr::filter(
      .data[[variable_type_column]] %in% c("continuous", "derived"),
      tolower(.data[[required_column]]) == "y"
    ) %>%
    dplyr::pull(.data[[field_name_column]]) %>%
    unique()
  
  # Identify variables present in data
  present_vars <- requested_vars[
    requested_vars %in% names(data)
  ]
  
  # Retain numeric variables only
  vars_for_summary <- present_vars[
    vapply(
      data[present_vars],
      is.numeric,
      logical(1)
    )
  ]
  
  # Record variables excluded before analysis
  excluded_variables <- data.frame(
    variable = requested_vars,
    reason = dplyr::case_when(
      !requested_vars %in% names(data) ~
        "not_present_in_dataset",
      
      requested_vars %in% present_vars &
        !requested_vars %in% vars_for_summary ~
        "not_numeric",
      
      TRUE ~ NA_character_
    ),
    stringsAsFactors = FALSE
  ) %>%
    dplyr::filter(!is.na(reason))
  
  # Prepare result containers
  ad_results <- list()
  skipped_tests <- list()
  
  # Run analysis for each eligible variable
  for (var in vars_for_summary) {
    
    df <- data %>%
      dplyr::select(
        dplyr::all_of(site_column),
        dplyr::all_of(var)
      ) %>%
      dplyr::filter(
        !is.na(.data[[site_column]]),
        !is.na(.data[[var]])
      ) %>%
      dplyr::mutate(
        value = as.numeric(.data[[var]])
      ) %>%
      dplyr::filter(
        is.finite(value)
      )
    
    # Skip variable if no usable data
    if (nrow(df) == 0) {
      skipped_tests[[var]] <- "no_numeric_data"
      next
    }
    
    sites <- unique(df[[site_column]])
    
    for (s in sites) {
      
      site_data <- df %>%
        dplyr::filter(
          .data[[site_column]] == s
        ) %>%
        dplyr::pull(value)
      
      rest_data <- df %>%
        dplyr::filter(
          .data[[site_column]] != s
        ) %>%
        dplyr::pull(value)
      
      test_id <- paste(var, s, sep = "_")
      
      # Check sample size
      if (length(site_data) < min_site_n) {
        skipped_tests[[test_id]] <- "too_few_site_records"
        next
      }
      
      if (length(rest_data) < min_rest_n) {
        skipped_tests[[test_id]] <- "too_few_rest_records"
        next
      }
      
      # Check number of unique values
      if (
        length(unique(site_data)) < min_unique_values ||
        length(unique(rest_data)) < min_unique_values
      ) {
        skipped_tests[[test_id]] <- "too_few_unique_values"
        next
      }
      
      # Check variance
      site_sd <- stats::sd(site_data)
      rest_sd <- stats::sd(rest_data)
      
      if (
        is.na(site_sd) ||
        is.na(rest_sd) ||
        site_sd == 0 ||
        rest_sd == 0
      ) {
        skipped_tests[[test_id]] <- "zero_or_invalid_variance"
        next
      }
      
      # Run Anderson-Darling test
      test <- tryCatch(
        kSamples::ad.test(
          site_data,
          rest_data
        ),
        error = function(e) e
      )
      
      # Record test errors
      if (inherits(test, "error")) {
        skipped_tests[[test_id]] <- paste0(
          "test_error: ",
          conditionMessage(test)
        )
        next
      }
      
      # Extract p-value
      p_value <- tryCatch(
        test$ad[1, " asympt. P-value"],
        error = function(e) NA_real_
      )
      
      if (is.na(p_value)) {
        skipped_tests[[test_id]] <- "p_value_not_available"
        next
      }
      
      # Save successful result
      ad_results[[test_id]] <- data.frame(
        variable = var,
        site = as.character(s),
        n_site = length(site_data),
        n_rest = length(rest_data),
        p_value = as.numeric(p_value),
        row.names = NULL,
        stringsAsFactors = FALSE
      )
    }
  }
  
  # Combine successful results
  if (length(ad_results) > 0) {
    
    ad_results <- dplyr::bind_rows(ad_results) %>%
      dplyr::arrange(p_value)
    
  } else {
    
    ad_results <- data.frame(
      variable = character(),
      site = character(),
      n_site = integer(),
      n_rest = integer(),
      p_value = numeric(),
      stringsAsFactors = FALSE
    )
  }
  
  # Create flagged results
  ad_flags <- ad_results %>%
    dplyr::filter(
      p_value < p_threshold
    ) %>%
    dplyr::arrange(
      p_value
    )
  
  # Convert skipped tests to dataframe
  if (length(skipped_tests) > 0) {
    
    skipped_tests <- data.frame(
      test = names(skipped_tests),
      reason = unlist(
        skipped_tests,
        use.names = FALSE
      ),
      row.names = NULL,
      stringsAsFactors = FALSE
    )
    
  } else {
    
    skipped_tests <- data.frame(
      test = character(),
      reason = character(),
      stringsAsFactors = FALSE
    )
  }
  
  return(
    list(
      ad_results = ad_results,
      ad_flags = ad_flags,
      skipped_tests = skipped_tests,
      excluded_variables = excluded_variables
    )
  )
}
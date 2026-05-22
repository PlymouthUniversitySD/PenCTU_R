#Title: Anderson-Darling Function
#Author: Paigan Aspinall
#Version & Date: V1.0.0 24APR2026
#R version: 4.4.3

#' Run Anderson-Darling distribution tests by site
#'
#' This function compares each site's distribution with the rest of the study
#' population for required continuous and derived variables using the
#' Anderson-Darling test.
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
#'
ad_distribution_analysis <- function(data,
                                     metadata,
                                     variable_type_column = "variable_type",
                                     required_column = "required_yn",
                                     field_name_column = "field_name",
                                     site_column = "redcap_data_access_group",
                                     min_site_n = 5,
                                     min_rest_n = 5,
                                     min_unique_values = 4,
                                     p_threshold = 0.05) {
  
  vars_for_summary <- metadata %>%
    dplyr::filter(
      .data[[variable_type_column]] %in% c("continuous", "derived"),
      tolower(.data[[required_column]]) == "y"
    ) %>%
    dplyr::pull(.data[[field_name_column]]) %>%
    unique()
  
  vars_for_summary <- vars_for_summary[vars_for_summary %in% names(data)]
  
  ad_results <- list()
  skipped_tests <- list()
  
  for (var in vars_for_summary) {
    
    df <- data %>%
      dplyr::select(dplyr::all_of(site_column), dplyr::all_of(var)) %>%
      dplyr::filter(!is.na(.data[[var]])) %>%
      dplyr::mutate(value = suppressWarnings(as.numeric(.data[[var]]))) %>%
      dplyr::filter(is.finite(value))
    
    if (nrow(df) == 0) {
      skipped_tests[[var]] <- "no_numeric_data"
      next
    }
    
    sites <- unique(df[[site_column]])
    
    for (s in sites) {
      
      site_data <- df %>%
        dplyr::filter(.data[[site_column]] == s) %>%
        dplyr::pull(value)
      
      rest_data <- df %>%
        dplyr::filter(.data[[site_column]] != s) %>%
        dplyr::pull(value)
      
      test_id <- paste(var, s, sep = "_")
      
      if (length(site_data) < min_site_n) {
        skipped_tests[[test_id]] <- "too_few_site_records"
        next
      }
      
      if (length(rest_data) < min_rest_n) {
        skipped_tests[[test_id]] <- "too_few_rest_records"
        next
      }
      
      if (stats::sd(site_data, na.rm = TRUE) == 0 ||
          stats::sd(rest_data, na.rm = TRUE) == 0) {
        skipped_tests[[test_id]] <- "zero_variance"
        next
      }
      
      if (length(unique(site_data)) < min_unique_values ||
          length(unique(rest_data)) < min_unique_values) {
        skipped_tests[[test_id]] <- "too_few_unique_values"
        next
      }
      
      test <- tryCatch(
        kSamples::ad.test(site_data, rest_data),
        error = function(e) NULL
      )
      
      if (is.null(test)) {
        skipped_tests[[test_id]] <- "test_error"
        next
      }
      
      ad_results[[test_id]] <- data.frame(
        variable = var,
        site = s,
        n_site = length(site_data),
        n_rest = length(rest_data),
        p_value = test$ad[1, " asympt. P-value"],
        row.names = NULL
      )
    }
  }
  
  ad_results <- dplyr::bind_rows(ad_results) %>%
    dplyr::arrange(p_value)
  
  ad_flags <- ad_results %>%
    dplyr::filter(p_value < p_threshold) %>%
    dplyr::arrange(p_value)
  
  skipped_tests <- data.frame(
    test = names(skipped_tests),
    reason = unlist(skipped_tests),
    row.names = NULL
  )
  
  return(list(
    ad_results = ad_results,
    ad_flags = ad_flags,
    skipped_tests = skipped_tests
  ))
}
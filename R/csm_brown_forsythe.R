#Title: Brown-Forsythe Test Function
#Author: Paigan Aspinall
#Version & Date: V1.0.1 10AUG2026
#R version: 4.4.3

#' Run Brown-Forsythe variance tests by site
#'
#' This function runs Brown-Forsythe tests for required continuous and derived
#' variables to identify variables where variability differs between sites.
#' Small sites and variables with insufficient data are excluded.
#'
#' Variables selected for analysis are coerced to numeric before testing.
#'
#' @param data A REDCap export dataset.
#' @param metadata A critical data item metadata dataframe.
#' @param variable_type_column Column name in metadata containing variable type.
#' @param required_column Column name in metadata indicating whether the field is required.
#' @param field_name_column Column name in metadata containing REDCap field names.
#' @param site_column Column name in dataset representing site/group.
#' @param min_total_n Minimum total number of records required for testing.
#' @param min_site_n Minimum number of records per site required for inclusion.
#' @param p_threshold P-value threshold used to flag variables.
#'
#' @return A list containing:
#' \itemize{
#'   \item \code{variance_results}: all Brown-Forsythe test results
#'   \item \code{variance_flags}: variables with p-value below threshold
#'   \item \code{skipped_vars}: variables excluded from testing and the reason
#' }
#'
#' @importFrom dplyr "%>%"
#'
#' @examples
#' variance_outputs <- variance_analysis(data, metadata)
#' variance_results <- variance_outputs$variance_results
#' variance_flags <- variance_outputs$variance_flags
#'
#' @export

variance_analysis <- function(
    data,
    metadata,
    variable_type_column = "variable_type",
    required_column = "required_yn",
    field_name_column = "field_name",
    site_column = "redcap_data_access_group",
    min_total_n = 10,
    min_site_n = 5,
    p_threshold = 0.05) {
  
  vars_for_summary <- metadata %>%
    dplyr::filter(
      .data[[variable_type_column]] %in% c("continuous", "derived"),
      tolower(.data[[required_column]]) == "y"
    ) %>%
    dplyr::pull(.data[[field_name_column]]) %>%
    unique()
  
  vars_for_summary <- vars_for_summary[
    vars_for_summary %in% names(data)
  ]
  
  variance_results <- list()
  skipped_vars <- list()
  
  for (var in vars_for_summary) {
    
    df <- data %>%
      dplyr::transmute(
        site = as.factor(.data[[site_column]]),
        value = as.numeric(.data[[var]])
      ) %>%
      dplyr::filter(
        !is.na(site),
        !is.na(value)
      )
    
    if (nrow(df) < min_total_n) {
      skipped_vars[[var]] <- "too_few_records"
      next
    }
    
    df <- df %>%
      dplyr::group_by(site) %>%
      dplyr::filter(dplyr::n() >= min_site_n) %>%
      dplyr::ungroup() %>%
      droplevels()
    
    if (dplyr::n_distinct(df$site) < 2) {
      skipped_vars[[var]] <- "fewer_than_two_sites_after_filtering"
      next
    }
    
    if (stats::sd(df$value, na.rm = TRUE) == 0) {
      skipped_vars[[var]] <- "zero_variance"
      next
    }
    
    test <- car::leveneTest(
      value ~ site,
      data = df,
      center = stats::median
    )
    
    variance_results[[var]] <- data.frame(
      variable = var,
      p_value = test$`Pr(>F)`[1],
      method = "Brown-Forsythe",
      row.names = NULL
    )
  }
  
  variance_results <- dplyr::bind_rows(variance_results)
  
  if (nrow(variance_results) > 0) {
    variance_results <- variance_results %>%
      dplyr::arrange(p_value)
    
    variance_flags <- variance_results %>%
      dplyr::filter(p_value < p_threshold)
  } else {
    variance_flags <- variance_results
  }
  
  skipped_vars <- data.frame(
    variable = names(skipped_vars),
    reason = unlist(skipped_vars),
    row.names = NULL
  )
  
  return(list(
    variance_results = variance_results,
    variance_flags = variance_flags,
    skipped_vars = skipped_vars
  ))
}
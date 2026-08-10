#Title: Generate Site Distribution Summary Function
#Author: Paigan Aspinall
#Version & Date: V1.0.1 10AUG2026
#R version: 4.4.3

#' Generate site-level distribution summaries for required numeric variables
#'
#' This function identifies required continuous and derived variables from the
#' metadata and calculates descriptive statistics by site (REDCap data access group).
#'
#' Variables selected for summary are coerced to numeric before descriptive
#' statistics are calculated. Missing values are retained as NA.
#'
#' @param data A REDCap export dataset.
#' @param metadata A critical data item metadata dataframe.
#' @param variable_type_column Column name in metadata containing variable type.
#' @param required_column Column name in metadata indicating whether the field is required.
#' @param field_name_column Column name in metadata containing REDCap field names.
#' @param site_column Column name in dataset representing site/group
#'   (default = redcap_data_access_group).
#'
#' @return A dataframe containing summary statistics by site and variable.
#'
#' @importFrom dplyr "%>%"
#'
#' @examples
#' site_summary <- site_distribution_summary(data, metadata)
#'
#' @export

site_distribution_summary <- function(
    data,
    metadata,
    variable_type_column = "variable_type",
    required_column = "required_yn",
    field_name_column = "field_name",
    site_column = "redcap_data_access_group") {
  
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
  
  site_summary <- lapply(vars_for_summary, function(var) {
    
    temp_data <- data %>%
      dplyr::mutate(
        .summary_value = as.numeric(.data[[var]])
      )
    
    temp_data %>%
      dplyr::group_by(.data[[site_column]]) %>%
      dplyr::summarise(
        n = sum(!is.na(.summary_value)),
        mean = mean(.summary_value, na.rm = TRUE),
        sd = stats::sd(.summary_value, na.rm = TRUE),
        median = stats::median(.summary_value, na.rm = TRUE),
        IQR = stats::IQR(.summary_value, na.rm = TRUE),
        min = if (all(is.na(.summary_value))) {
          NA_real_
        } else {
          min(.summary_value, na.rm = TRUE)
        },
        max = if (all(is.na(.summary_value))) {
          NA_real_
        } else {
          max(.summary_value, na.rm = TRUE)
        },
        .groups = "drop"
      ) %>%
      dplyr::mutate(variable = var)
    
  }) %>%
    dplyr::bind_rows()
  
  return(site_summary)
}
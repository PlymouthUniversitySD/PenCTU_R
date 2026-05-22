#Title: Generate Distribution Summary Function
#Author: Paigan Aspinall
#Version & Date: V1.0.0 24APR2026
#R version: 4.4.3

#' Generate overall distribution summaries for required numeric variables
#'
#' This function identifies required continuous and derived variables from the
#' critical data item metadata and calculates overall descriptive statistics
#' for each variable present in the REDCap export dataset.
#'
#' @param data A REDCap export dataset.
#' @param metadata A critical data item metadata dataframe.
#' @param variable_type_column Column name in metadata containing variable type.
#' @param required_column Column name in metadata indicating whether the field is required.
#' @param field_name_column Column name in metadata containing REDCap field names.
#'
#' @return A dataframe containing one row per variable with summary statistics.
#'
#' @importFrom dplyr "%>%"
#'
#' @examples
#' overall_summary <- overall_distribution_summary(
#'   data = data,
#'   metadata = metadata,
#'   variable_type_column = "variable_type",
#'   required_column = "required_yn",
#'   field_name_column = "field_name"
#' )
#'
#' @export

overall_distribution_summary <- function(data,
                                         metadata,
                                         variable_type_column = "variable_type",
                                         required_column = "required_yn",
                                         field_name_column = "field_name") {
  
  vars_for_summary <- metadata %>%
    dplyr::filter(
      .data[[variable_type_column]] %in% c("continuous", "derived"),
      tolower(.data[[required_column]]) == "y"
    ) %>%
    dplyr::pull(.data[[field_name_column]]) %>%
    unique()
  
  vars_for_summary <- vars_for_summary[vars_for_summary %in% names(data)]
  
  overall_summary <- lapply(vars_for_summary, function(var) {
    
    stats <- summary_stats(data[[var]])
    
    data.frame(
      variable = var,
      t(stats),
      row.names = NULL
    )
    
  }) %>%
    dplyr::bind_rows()
  
  return(overall_summary)
}
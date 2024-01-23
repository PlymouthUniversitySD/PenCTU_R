# Author: Paigan Aspinall
# Date: 23JAN2024
# R version: 4.2.2
#' Calculates the completeness of a timepoint based on the *timepoint_name*_compelte column values.
#'
#' Outputs "Complete", "Partially complete", "Not started or "Undefined" as the completeness value for a row in a defined dataset.Used in conjundtion with plot_crf_completeness_data_preparation function.
#'
#' @param row A row defined from a dataset.
#'
#' @return A value indicated the completeness of the timepoint.
#'
#'
#' @importFrom dplyr "%>%"
#' 
#' @examples
#' 
#' completeness_values <- apply(subset_table[, completeness_columns], 1, calculate_completeness)
#'
#' @export
#'

calculate_completeness <- function(row) {
  if (any(!is.na(row)) && ((any(row %in% c(1, 2)) && any(row == 0)) || (any(row == 1) && any(row == 2)))) {
    return("Partially complete")
  } else if (all(row == 2, na.rm = TRUE) || all(is.na(row) | row == 2)) {
    return("Complete")
  } else if (all(is.na(row)) || all(row == 0) || all(is.na(row) | row == 0)) {
    return("Not started")
  } else {
    return("Undefined")
  }
}

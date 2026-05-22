#Title: Z-scores Function
#Author: Paigan Aspinall
#Version & Date: V1.0.0 24APR2026
#R version: 4.4.3

#' Calculate site-level Z-scores for required numeric variables
#'
#' This function calculates site-level Z-scores for required continuous and
#' derived variables. Z-scores compare each site's mean with the overall study
#' mean, standardised by the overall standard deviation.
#'
#' @param data A REDCap export dataset.
#' @param metadata A critical data item metadata dataframe.
#' @param variable_type_column Column name in metadata containing variable type.
#' @param required_column Column name in metadata indicating whether the field is required.
#' @param field_name_column Column name in metadata containing REDCap field names.
#' @param site_column Column name in dataset representing site/group.
#' @param small_n_threshold Minimum site-level n before results are considered meaningful.
#' @param moderate_threshold Absolute Z-score threshold for moderate flags.
#' @param large_threshold Absolute Z-score threshold for large flags.
#'
#' @return A list containing:
#' \itemize{
#'   \item \code{z_scores}: all calculated Z-scores
#'   \item \code{z_flags}: meaningful moderate/large Z-score flags
#' }
#'
#' @importFrom dplyr "%>%"
#'
#' @examples
#' z_score_outputs <- z_score_analysis(data, metadata)
#' z_scores <- z_score_outputs$z_scores
#' z_flags <- z_score_outputs$z_flags
#'
#' @export
#'
z_score_analysis <- function(data,
                             metadata,
                             variable_type_column = "variable_type",
                             required_column = "required_yn",
                             field_name_column = "field_name",
                             site_column = "redcap_data_access_group",
                             small_n_threshold = 5,
                             moderate_threshold = 2,
                             large_threshold = 3) {
  
  vars_for_summary <- metadata %>%
    dplyr::filter(
      .data[[variable_type_column]] %in% c("continuous", "derived"),
      tolower(.data[[required_column]]) == "y"
    ) %>%
    dplyr::pull(.data[[field_name_column]]) %>%
    unique()
  
  vars_for_summary <- vars_for_summary[vars_for_summary %in% names(data)]
  
  overall_stats <- data %>%
    dplyr::summarise(dplyr::across(
      dplyr::all_of(vars_for_summary),
      list(
        mean = ~mean(.x, na.rm = TRUE),
        sd = ~sd(.x, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    ))
  
  site_means <- data %>%
    dplyr::group_by(.data[[site_column]]) %>%
    dplyr::summarise(dplyr::across(
      dplyr::all_of(vars_for_summary),
      ~mean(.x, na.rm = TRUE),
      .names = "{.col}"
    ), .groups = "drop")
  
  site_long <- site_means %>%
    tidyr::pivot_longer(
      -dplyr::all_of(site_column),
      names_to = "variable",
      values_to = "site_mean"
    )
  
  overall_long <- overall_stats %>%
    tidyr::pivot_longer(
      dplyr::everything(),
      names_to = c("variable", ".value"),
      names_pattern = "(.+)_(mean|sd)$"
    )
  
  z_scores <- site_long %>%
    dplyr::left_join(overall_long, by = "variable") %>%
    dplyr::mutate(
      z_score = (site_mean - mean) / sd
    )
  
  site_n <- data %>%
    dplyr::group_by(.data[[site_column]]) %>%
    dplyr::summarise(dplyr::across(
      dplyr::all_of(vars_for_summary),
      ~sum(!is.na(.x)),
      .names = "{.col}"
    ), .groups = "drop") %>%
    tidyr::pivot_longer(
      -dplyr::all_of(site_column),
      names_to = "variable",
      values_to = "n"
    )
  
  z_scores <- z_scores %>%
    dplyr::left_join(site_n, by = c(site_column, "variable")) %>%
    dplyr::mutate(
      flag_large = abs(z_score) > large_threshold,
      flag_moderate = abs(z_score) > moderate_threshold,
      small_n_flag = n < small_n_threshold,
      invalid_flag = is.na(sd) | sd == 0
    )
  
  z_flags <- z_scores %>%
    dplyr::filter(
      !small_n_flag,
      !invalid_flag,
      flag_moderate
    )
  
  return(list(
    z_scores = z_scores,
    z_flags = z_flags
  ))
}
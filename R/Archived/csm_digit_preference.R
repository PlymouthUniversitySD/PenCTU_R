#Title: Digit Preference Function
#Author: Paigan Aspinall
#Version & Date: V1.0.0 27APR2026
#R version: 4.4.3

#' Check decimal precision and rounding patterns by site
#'
#' This function checks required continuous variables for possible rounding
#' patterns by site. Derived/calculated fields are excluded because rounding
#' patterns may be expected from scoring algorithms.
#'
#' @param data A REDCap export dataset.
#' @param metadata A critical data item metadata dataframe.
#' @param expected_precision_column Optional metadata column giving expected decimal places.
#' @param whole_number_threshold Proportion threshold used to flag whole-number patterns.
#' @param min_n Minimum number of records required per site/variable.
#'
#' @return A dataframe summarising rounding patterns by site and variable.
#'
#' @export
#'
decimal_preference_analysis <- function(data,
                                        metadata,
                                        expected_precision_column = NULL,
                                        whole_number_threshold = 0.8,
                                        min_n = 5) {
  
  vars_meta <- metadata %>%
    dplyr::filter(
      variable_type == "continuous",
      tolower(required_yn) == "y"
    ) %>%
    dplyr::select(field_name, dplyr::everything()) %>%
    dplyr::distinct(field_name, .keep_all = TRUE)
  
  vars <- vars_meta$field_name
  vars <- vars[vars %in% names(data)]
  
  results <- list()
  
  for (var in vars) {
    
    expected_precision <- NA_integer_
    
    if (!is.null(expected_precision_column) &&
        expected_precision_column %in% names(vars_meta)) {
      expected_precision <- vars_meta %>%
        dplyr::filter(field_name == var) %>%
        dplyr::pull(.data[[expected_precision_column]]) %>%
        suppressWarnings(as.integer(.)) %>%
        dplyr::first()
    }
    
    df <- data %>%
      dplyr::select(redcap_data_access_group, dplyr::all_of(var)) %>%
      dplyr::filter(!is.na(.data[[var]])) %>%
      dplyr::mutate(
        value_raw = as.character(.data[[var]]),
        value_num = suppressWarnings(as.numeric(.data[[var]]))
      ) %>%
      dplyr::filter(is.finite(value_num))
    
    if (nrow(df) == 0) next
    
    site_summary <- df %>%
      dplyr::group_by(redcap_data_access_group) %>%
      dplyr::summarise(
        n = dplyr::n(),
        
        whole_number_n = sum(value_num == round(value_num), na.rm = TRUE),
        whole_number_prop = whole_number_n / n,
        
        decimal_recorded_n = sum(grepl("\\.", value_raw)),
        decimal_recorded_prop = decimal_recorded_n / n,
        
        trailing_00_n = sum(grepl("\\.00$", value_raw)),
        trailing_00_prop = trailing_00_n / n,
        
        unique_values = dplyr::n_distinct(value_num),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        variable = var,
        expected_precision = expected_precision,
        
        flag_whole_number_pattern = dplyr::case_when(
          n < min_n ~ FALSE,
          !is.na(expected_precision) & expected_precision == 0 ~ FALSE,
          whole_number_prop >= whole_number_threshold ~ TRUE,
          TRUE ~ FALSE
        ),
        
        flag_trailing_00_pattern = dplyr::case_when(
          n < min_n ~ FALSE,
          trailing_00_prop >= whole_number_threshold ~ TRUE,
          TRUE ~ FALSE
        ),
        
        flag_decimal_preference =
          flag_whole_number_pattern | flag_trailing_00_pattern
      )
    
    results[[var]] <- site_summary
  }
  
  dplyr::bind_rows(results) %>%
    dplyr::arrange(
      dplyr::desc(flag_decimal_preference),
      dplyr::desc(whole_number_prop),
      dplyr::desc(trailing_00_prop)
    )
}
#Title: Mahalanobis Distribution Function
#Author: Paigan Aspinall
#Version & Date: V1.0.0 24APR2026
#R version: 4.4.3

#' Run Mahalanobis distance analysis for multivariate groups
#'
#' This function uses the multivariate group definitions in the metadata to
#' calculate Mahalanobis distances for continuous variables within each group.
#' Derived and non-continuous variables are excluded from the analysis.
#'
#' @param data A REDCap export dataset.
#' @param metadata A critical data item metadata dataframe.
#' @param multivariate_group_column Column name containing multivariate group names.
#' @param multivariate_yn_column Column name indicating whether a variable is multivariate.
#' @param variable_type_column Column name containing variable type.
#' @param field_name_column Column name containing REDCap field names.
#' @param record_id_column Column name containing participant/record ID.
#' @param site_column Column name containing site/group.
#' @param event_column Column name containing REDCap event name.
#' @param min_n Minimum number of complete records required to run the analysis.
#' @param threshold_prob Chi-square percentile used as the Mahalanobis threshold.
#'
#' @return A dataframe containing record identifiers, group values, Mahalanobis
#' distance, threshold, and flag.
#'
#' @importFrom dplyr "%>%"
#'
#' @examples
#' mahalanobis_results <- mahalanobis_analysis(data, metadata)
#'
#' @export
#'
mahalanobis_analysis <- function(data,
                                 metadata,
                                 multivariate_group_column = "multivariate_group",
                                 multivariate_yn_column = "multivariate_yn",
                                 variable_type_column = "variable_type",
                                 field_name_column = "field_name",
                                 record_id_column = "record_id",
                                 site_column = "redcap_data_access_group",
                                 event_column = "redcap_event_name",
                                 min_n = 10,
                                 threshold_prob = 0.99) {
  
  metadata_long <- metadata %>%
    tidyr::separate_rows(.data[[multivariate_group_column]], sep = ";") %>%
    dplyr::mutate(
      multivariate_group = stringr::str_trim(.data[[multivariate_group_column]])
    )
  
  valid_vars <- metadata_long %>%
    dplyr::filter(
      tolower(.data[[multivariate_yn_column]]) == "y",
      .data[[variable_type_column]] == "continuous",
      !is.na(multivariate_group),
      multivariate_group != ""
    ) %>%
    dplyr::group_by(multivariate_group) %>%
    dplyr::summarise(
      vars = list(unique(.data[[field_name_column]])),
      .groups = "drop"
    )
  
  results <- list()
  skipped_groups <- list()
  
  for (i in seq_len(nrow(valid_vars))) {
    
    group_name <- valid_vars$multivariate_group[i]
    vars <- unlist(valid_vars$vars[i])
    vars <- vars[vars %in% names(data)]
    
    if (length(vars) < 2) {
      skipped_groups[[group_name]] <- "fewer_than_two_variables"
      next
    }
    
    df <- data %>%
      dplyr::select(
        dplyr::all_of(c(record_id_column, site_column, event_column)),
        dplyr::all_of(vars)
      ) %>%
      tidyr::drop_na()
    
    x <- df %>%
      dplyr::select(dplyr::all_of(vars)) %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), as.numeric))
    
    if (nrow(x) < min_n) {
      skipped_groups[[group_name]] <- "too_few_complete_records"
      next
    }
    
    sds <- sapply(x, sd, na.rm = TRUE)
    
    if (any(is.na(sds) | sds == 0)) {
      skipped_groups[[group_name]] <- "zero_or_missing_sd"
      next
    }
    
    cov_matrix <- stats::cov(x, use = "complete.obs")
    
    if (det(cov_matrix) == 0) {
      skipped_groups[[group_name]] <- "singular_covariance_matrix"
      next
    }
    
    center <- colMeans(x, na.rm = TRUE)
    
    md <- stats::mahalanobis(x, center, cov_matrix)
    
    threshold <- stats::qchisq(threshold_prob, df = length(vars))
    
    results[[group_name]] <- df %>%
      dplyr::mutate(
        multivariate_group = group_name,
        mahalanobis = md,
        threshold = threshold,
        flag = mahalanobis > threshold
      )
  }
  
  mahalanobis_results <- dplyr::bind_rows(results)
  
  skipped_groups <- data.frame(
    multivariate_group = names(skipped_groups),
    reason = unlist(skipped_groups),
    row.names = NULL
  )
  
  return(list(
    mahalanobis_results = mahalanobis_results,
    skipped_groups = skipped_groups
  ))
}
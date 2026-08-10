#Title: k-means Function
#Author: Paigan Aspinall
#Version & Date: V1.0.1 10AUG2026
#R version: 4.4.3

#' Run k-means clustering for multivariate groups
#'
#' This function runs k-means clustering for continuous variables within each
#' multivariate group defined in the metadata. It is intended as an exploratory
#' analysis to identify unusual clustering patterns or site-dominated clusters.
#'
#' Variables selected for clustering are coerced to numeric before complete-case
#' filtering. Records containing missing or non-finite values in any clustering
#' variable are excluded from the analysis.
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
#' @param min_n Minimum number of complete records required to run clustering.
#' @param k Number of clusters to use.
#' @param nstart Number of random starts for k-means.
#' @param site_prop_threshold Proportion threshold used to flag site-dominated clusters.
#'
#' @return A list containing cluster-level outputs.
#'
#' @importFrom dplyr "%>%"
#'
#' @examples
#' cluster_outputs <- clustering_analysis(data, metadata)
#'
#' @export
#'
clustering_analysis <- function(
    data,
    metadata,
    multivariate_group_column = "multivariate_group",
    multivariate_yn_column = "multivariate_yn",
    variable_type_column = "variable_type",
    field_name_column = "field_name",
    record_id_column = "record_id",
    site_column = "redcap_data_access_group",
    event_column = "redcap_event_name",
    min_n = 10,
    k = 2,
    nstart = 25,
    site_prop_threshold = 0.8) {
  
  metadata_long <- metadata %>%
    tidyr::separate_rows(
      dplyr::all_of(multivariate_group_column),
      sep = ";"
    ) %>%
    dplyr::mutate(
      multivariate_group =
        stringr::str_trim(.data[[multivariate_group_column]])
    )
  
  valid_groups <- metadata_long %>%
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
  
  cluster_results <- list()
  skipped_groups <- list()
  
  for (i in seq_len(nrow(valid_groups))) {
    
    group_name <- valid_groups$multivariate_group[i]
    vars <- unlist(valid_groups$vars[i])
    vars <- vars[vars %in% names(data)]
    
    if (length(vars) < 2) {
      skipped_groups[[group_name]] <- "fewer_than_two_variables"
      next
    }
    
    # Select required identifying and clustering variables
    df <- data %>%
      dplyr::select(
        dplyr::all_of(c(
          record_id_column,
          site_column,
          event_column,
          vars
        ))
      )
    
    # Convert clustering variables to numeric before checking missingness
    df <- df %>%
      dplyr::mutate(
        dplyr::across(
          dplyr::all_of(vars),
          as.numeric
        )
      )
    
    # Remove records containing NA, NaN or Inf in clustering variables
    df <- df %>%
      dplyr::filter(
        dplyr::if_all(
          dplyr::all_of(vars),
          ~ !is.na(.) & is.finite(.)
        )
      )
    
    if (nrow(df) < min_n) {
      skipped_groups[[group_name]] <- "too_few_complete_records"
      next
    }
    
    x <- df %>%
      dplyr::select(dplyr::all_of(vars))
    
    # Check for variables with no variation
    sds <- vapply(
      x,
      stats::sd,
      numeric(1),
      na.rm = TRUE
    )
    
    if (any(is.na(sds) | !is.finite(sds) | sds == 0)) {
      skipped_groups[[group_name]] <- "zero_or_missing_sd"
      next
    }
    
    # k-means requires at least k distinct observations
    n_unique <- nrow(unique(x))
    
    if (n_unique < k) {
      skipped_groups[[group_name]] <- "fewer_unique_observations_than_clusters"
      next
    }
    
    # Standardise variables
    x_scaled <- scale(x)
    
    # Final safeguard against non-finite scaled values
    if (any(!is.finite(x_scaled))) {
      skipped_groups[[group_name]] <- "non_finite_values_after_scaling"
      next
    }
    
    set.seed(123)
    
    km <- stats::kmeans(
      x_scaled,
      centers = k,
      nstart = nstart
    )
    
    cluster_results[[group_name]] <- df %>%
      dplyr::mutate(
        multivariate_group = group_name,
        cluster = km$cluster
      )
  }
  
  cluster_results <- dplyr::bind_rows(cluster_results)
  
  if (nrow(cluster_results) > 0) {
    
    cluster_summary <- cluster_results %>%
      dplyr::group_by(
        multivariate_group,
        cluster
      ) %>%
      dplyr::summarise(
        n = dplyr::n(),
        .groups = "drop"
      )
    
    site_cluster_summary <- cluster_results %>%
      dplyr::group_by(
        multivariate_group,
        .data[[site_column]],
        cluster
      ) %>%
      dplyr::summarise(
        n = dplyr::n(),
        .groups = "drop"
      ) %>%
      dplyr::group_by(
        multivariate_group,
        .data[[site_column]]
      ) %>%
      dplyr::mutate(
        prop = n / sum(n)
      ) %>%
      dplyr::ungroup()
    
    cluster_flags <- site_cluster_summary %>%
      dplyr::filter(
        prop > site_prop_threshold
      )
    
  } else {
    
    cluster_summary <- data.frame()
    site_cluster_summary <- data.frame()
    cluster_flags <- data.frame()
  }
  
  skipped_groups <- data.frame(
    multivariate_group = names(skipped_groups),
    reason = unlist(skipped_groups),
    row.names = NULL
  )
  
  return(list(
    cluster_results = cluster_results,
    cluster_summary = cluster_summary,
    site_cluster_summary = site_cluster_summary,
    cluster_flags = cluster_flags,
    skipped_groups = skipped_groups
  ))
}

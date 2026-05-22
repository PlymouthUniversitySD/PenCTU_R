#Title: Duplicate detection function
#Author: Paigan Aspinall
#Version & Date: V1.0.0 27APR2026
#R version: 4.4.3

#' Detect duplicate or highly similar records
#'
#' This function compares non-repeating REDCap records across required critical
#' data items and identifies pairs of records with high similarity.
#'
#' @param data A REDCap export dataset.
#' @param metadata A critical data item metadata dataframe.
#' @param min_comparable_fields Minimum number of shared non-missing fields required for comparison.
#' @param similarity_threshold Similarity threshold used to flag record pairs.
#'
#' @return A dataframe containing pairs of records with high similarity.
#'
#' @importFrom dplyr "%>%"
#'
#' @examples
#' similarity_results <- similarity_detection(data, metadata)
#'
#' @export
#'
similarity_detection <- function(data,
                                 metadata,
                                 min_comparable_fields = 3,
                                 similarity_threshold = 0.8) {
  
  metadata_events <- metadata %>%
    tidyr::separate_rows(event_names, sep = ";") %>%
    dplyr::mutate(
      event_names = stringr::str_trim(event_names),
      required_yn = tolower(required_yn)
    ) %>%
    dplyr::filter(required_yn == "y")
  
  vars_similarity <- metadata_events %>%
    dplyr::pull(field_name) %>%
    unique()
  
  vars_similarity <- vars_similarity[vars_similarity %in% names(data)]
  
  df <- data %>%
    dplyr::filter(is.na(redcap_repeat_instance)) %>%
    dplyr::select(
      record_id,
      redcap_data_access_group,
      redcap_event_name,
      dplyr::all_of(vars_similarity)
    )
  
  df_compare <- df %>%
    dplyr::mutate(dplyr::across(
      dplyr::all_of(vars_similarity),
      ~ {
        x <- .x
        
        if (is.numeric(x)) {
          x <- round(x, 1)
        }
        
        as.character(x)
      }
    ))
  
  similarity_results <- list()
  
  if (nrow(df_compare) < 2) {
    return(dplyr::bind_rows(similarity_results))
  }
  
  for (i in 1:(nrow(df_compare) - 1)) {
    for (j in (i + 1):nrow(df_compare)) {
      
      row1 <- df_compare[i, vars_similarity]
      row2 <- df_compare[j, vars_similarity]
      
      comparable <- !is.na(row1) & !is.na(row2)
      total <- sum(comparable)
      
      if (total < min_comparable_fields) next
      
      matches <- sum(row1[comparable] == row2[comparable])
      similarity <- matches / total
      
      if (similarity >= similarity_threshold) {
        similarity_results[[length(similarity_results) + 1]] <- data.frame(
          record_id_1 = df_compare$record_id[i],
          record_id_2 = df_compare$record_id[j],
          site_1 = df_compare$redcap_data_access_group[i],
          site_2 = df_compare$redcap_data_access_group[j],
          event_1 = df_compare$redcap_event_name[i],
          event_2 = df_compare$redcap_event_name[j],
          comparable_fields = total,
          matching_fields = matches,
          similarity = similarity,
          row.names = NULL
        )
      }
    }
  }
  
  dplyr::bind_rows(similarity_results)
}
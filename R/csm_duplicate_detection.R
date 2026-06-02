#Title: Duplicate detection function
#Author: Paigan Aspinall
#Version & Date: V1.0.0 27APR2026
#R version: 4.4.3

#' Detect duplicate or highly similar records
#'
#' This function compares non-repeating REDCap records across required critical
#' data items and identifies pairs of records with a high degree of similarity.
#'
#' The function is intended to support central statistical monitoring by
#' identifying potential duplicate participants, duplicate data entry,
#' fabricated records, or records that are unexpectedly similar.
#'
#' Comparisons are restricted to required fields defined within the metadata.
#' Numeric values are rounded to one decimal place prior to comparison to reduce
#' the impact of trivial numerical differences.
#'
#' For each pair of records, the function:
#' \itemize{
#'   \item Identifies fields where both records contain non-missing values.
#'   \item Calculates the number of matching values across comparable fields.
#'   \item Calculates a similarity score:
#'   \deqn{
#'   Similarity = Matching\ Fields / Comparable\ Fields
#'   }
#'   \item Returns record pairs exceeding the specified similarity threshold.
#' }
#'
#' The output additionally reports the names of matching and non-matching
#' fields to facilitate review of potentially duplicated records.
#'
#' @param data A REDCap export dataset.
#'
#' @param metadata A critical data item metadata dataframe. Required fields are
#'   identified using the \code{required_yn} column.
#'
#' @param min_comparable_fields Minimum number of shared non-missing fields
#'   required before a comparison is performed.
#'
#' @param similarity_threshold Minimum similarity score required for a record
#'   pair to be returned. Values range from 0 to 1.
#'
#' @return A dataframe containing:
#'   \describe{
#'     \item{record_id_1}{First record identifier}
#'     \item{record_id_2}{Second record identifier}
#'     \item{site_1}{Site associated with the first record}
#'     \item{site_2}{Site associated with the second record}
#'     \item{event_1}{Event associated with the first record}
#'     \item{event_2}{Event associated with the second record}
#'     \item{comparable_fields}{Number of fields with non-missing values in both records}
#'     \item{matching_fields}{Number of matching values across comparable fields}
#'     \item{similarity}{Proportion of matching fields}
#'     \item{matching_field_names}{Semi-colon separated list of fields with identical values}
#'     \item{non_matching_field_names}{Semi-colon separated list of fields with differing values}
#'   }
#'
#' Results are ordered according to the order in which record pairs are
#' identified. Higher similarity values indicate greater similarity between
#' records.
#'
#' @examples
#' similarity_results <- similarity_detection(
#'   data = data,
#'   metadata = metadata
#' )
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
      
      row1 <- df_compare[i, vars_similarity, drop = FALSE]
      row2 <- df_compare[j, vars_similarity, drop = FALSE]
      
      comparable <- !is.na(row1) & !is.na(row2)
      comparable <- as.logical(comparable[1, ])
      
      total <- sum(comparable)
      
      if (total < min_comparable_fields) next
      
      matching <- comparable & as.character(row1[1, ]) == as.character(row2[1, ])
      non_matching <- comparable & as.character(row1[1, ]) != as.character(row2[1, ])
      
      matches <- sum(matching)
      similarity <- matches / total
      
      if (similarity >= similarity_threshold) {
        
        matching_field_names <- paste(
          vars_similarity[matching],
          collapse = "; "
        )
        
        non_matching_field_names <- paste(
          vars_similarity[non_matching],
          collapse = "; "
        )
        
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
          matching_field_names = matching_field_names,
          non_matching_field_names = non_matching_field_names,
          row.names = NULL
        )
      }
    }
  }
  
  dplyr::bind_rows(similarity_results)
}

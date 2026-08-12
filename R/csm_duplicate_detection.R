#Title: Duplicate detection function
#Author: Paigan Aspinall
#Version & Date: V1.1.0 12AUG2026
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
#' The first column of the dataset is assumed to contain the REDCap record or
#' participant identifier.
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
#' @param data A REDCap export dataset. The first column is assumed to contain
#'   the record or participant identifier.
#'
#' @param metadata A critical data item metadata dataframe. Required fields are
#'   identified using the \code{required_yn} column.
#'
#' @param site_column Column name representing site/group.
#'
#' @param event_column Column name representing the REDCap event.
#'
#' @param repeat_instance_column Column containing the REDCap repeat instance.
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
#' @examples
#' similarity_results <- similarity_detection(
#'   data = data,
#'   metadata = metadata
#' )
#'
#' @export
#'
similarity_detection <- function(
    data,
    metadata,
    site_column = "redcap_data_access_group",
    event_column = "redcap_event_name",
    repeat_instance_column = "redcap_repeat_instance",
    min_comparable_fields = 3,
    similarity_threshold = 0.8) {
  
  # Use the first column of the dataset as the record/participant identifier
  record_id_column <- names(data)[1]
  
  # Check required structural columns exist
  required_columns <- c(
    record_id_column,
    site_column,
    event_column
  )
  
  missing_columns <- required_columns[
    !required_columns %in% names(data)
  ]
  
  if (length(missing_columns) > 0) {
    stop(
      paste0(
        "The following required columns were not found in the dataset: ",
        paste(missing_columns, collapse = ", ")
      )
    )
  }
  
  # Identify required variables from metadata
  metadata_events <- metadata %>%
    tidyr::separate_rows(
      event_names,
      sep = ";"
    ) %>%
    dplyr::mutate(
      event_names = stringr::str_trim(event_names),
      required_yn = tolower(required_yn)
    ) %>%
    dplyr::filter(
      required_yn == "y"
    )
  
  # Identify fields for similarity comparison
  vars_similarity <- metadata_events %>%
    dplyr::pull(field_name) %>%
    unique()
  
  # Retain only fields that exist in the dataset
  vars_similarity <- vars_similarity[
    vars_similarity %in% names(data)
  ]
  
  # Remove structural fields from comparison if they occur in metadata
  vars_similarity <- setdiff(
    vars_similarity,
    c(
      record_id_column,
      site_column,
      event_column,
      repeat_instance_column
    )
  )
  
  # Stop if there are no eligible fields for comparison
  if (length(vars_similarity) == 0) {
    stop(
      "No required metadata fields were found in the dataset for similarity comparison."
    )
  }
  
  # Retain non-repeating rows only
  if (repeat_instance_column %in% names(data)) {
    
    df <- data %>%
      dplyr::filter(
        is.na(.data[[repeat_instance_column]])
      )
    
  } else {
    
    df <- data
  }
  
  # Select record identifier, structural fields and comparison variables
  df <- df %>%
    dplyr::select(
      dplyr::all_of(record_id_column),
      dplyr::all_of(site_column),
      dplyr::all_of(event_column),
      dplyr::all_of(vars_similarity)
    )
  
  # Standardise values for comparison
  #
  # Numeric variables are rounded to one decimal place.
  # All variables are then converted to character so different
  # underlying R classes can be compared consistently.
  df_compare <- df %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(vars_similarity),
        ~ {
          x <- .x
          
          if (is.numeric(x)) {
            x <- round(x, 1)
          }
          
          as.character(x)
        }
      )
    )
  
  # Create empty result structure
  empty_results <- data.frame(
    record_id_1 = character(),
    record_id_2 = character(),
    site_1 = character(),
    site_2 = character(),
    event_1 = character(),
    event_2 = character(),
    comparable_fields = integer(),
    matching_fields = integer(),
    similarity = numeric(),
    matching_field_names = character(),
    non_matching_field_names = character(),
    stringsAsFactors = FALSE
  )
  
  # At least two records are required for comparison
  if (nrow(df_compare) < 2) {
    return(empty_results)
  }
  
  similarity_results <- list()
  
  # Compare every possible pair of records
  for (i in seq_len(nrow(df_compare) - 1)) {
    
    for (j in (i + 1):nrow(df_compare)) {
      
      # Extract comparison values
      row1 <- df_compare[
        i,
        vars_similarity,
        drop = FALSE
      ]
      
      row2 <- df_compare[
        j,
        vars_similarity,
        drop = FALSE
      ]
      
      values1 <- unlist(
        row1,
        use.names = FALSE
      )
      
      values2 <- unlist(
        row2,
        use.names = FALSE
      )
      
      # Fields are comparable only when both records contain data
      #
      # Blank strings are treated as missing so that two blank responses
      # do not artificially increase the similarity score.
      comparable <- !is.na(values1) &
        !is.na(values2) &
        values1 != "" &
        values2 != ""
      
      total <- sum(comparable)
      
      # Skip comparisons with insufficient shared data
      if (total < min_comparable_fields) {
        next
      }
      
      # Identify matching and non-matching comparable fields
      matching <- comparable &
        values1 == values2
      
      non_matching <- comparable &
        values1 != values2
      
      matches <- sum(matching)
      
      # Calculate similarity score
      similarity <- matches / total
      
      # Retain pairs meeting the similarity threshold
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
          
          record_id_1 = as.character(
            df_compare[[record_id_column]][i]
          ),
          
          record_id_2 = as.character(
            df_compare[[record_id_column]][j]
          ),
          
          site_1 = as.character(
            df_compare[[site_column]][i]
          ),
          
          site_2 = as.character(
            df_compare[[site_column]][j]
          ),
          
          event_1 = as.character(
            df_compare[[event_column]][i]
          ),
          
          event_2 = as.character(
            df_compare[[event_column]][j]
          ),
          
          comparable_fields = total,
          matching_fields = matches,
          similarity = similarity,
          matching_field_names = matching_field_names,
          non_matching_field_names = non_matching_field_names,
          
          stringsAsFactors = FALSE,
          row.names = NULL
        )
      }
    }
  }
  
  # Return empty structured dataframe if no potential duplicates were identified
  if (length(similarity_results) == 0) {
    return(empty_results)
  }
  
  # Combine results and place strongest similarities first
  similarity_results <- dplyr::bind_rows(
    similarity_results
  ) %>%
    dplyr::arrange(
      dplyr::desc(similarity),
      dplyr::desc(comparable_fields)
    )
  
  return(similarity_results)
}
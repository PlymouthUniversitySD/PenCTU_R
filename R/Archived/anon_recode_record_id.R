#Title: ANON - Recode record identifiers
#Author: Paigan Aspinall
#Version & Date: V1.0.0 01JUN2026
#R version: 4.4.3

#' Recode Record Identifiers
#'
#' Replaces participant record identifiers with randomly generated anonymised
#' identifiers.
#'
#' This function is designed for REDCap datasets where the participant
#' identifier is usually stored in \code{record_id}. The function can also use
#' an existing linkage file so that identifiers are recoded consistently across
#' multiple datasets.
#'
#' If a participant ID already exists in the linkage file, the existing
#' anonymised ID is reused. If a participant ID is not present in the linkage
#' file, a new random anonymised ID is generated.
#'
#' @param dataset A data frame containing the REDCap dataset.
#' @param record_id_field Character string specifying the record ID field in the
#'   dataset. Default is \code{"record_id"}.
#' @param prefix Character string used at the start of each anonymised ID.
#'   Default is \code{"P"}.
#' @param id_length Integer specifying the length of the random part of the
#'   anonymised ID. Default is \code{8}.
#' @param linkage_file Optional data frame containing an existing record ID
#'   linkage file. Expected columns are \code{original_record_id} and
#'   \code{anonymised_record_id}.
#' @param overwrite_original Logical. If \code{TRUE}, the original record ID
#'   field is overwritten. If \code{FALSE}, a new field is created. Default is
#'   \code{TRUE}.
#' @param new_record_id_field Character string specifying the new record ID field
#'   when \code{overwrite_original = FALSE}. Default is
#'   \code{"anonymised_record_id"}.
#'
#' @return A list containing:
#'   \describe{
#'     \item{dataset}{Dataset with recoded record identifiers}
#'     \item{linkage_file}{Record ID linkage file}
#'     \item{change_log}{Summary of record ID recoding}
#'   }
#'
#' @examples
#' result <- recode_record_id(
#'   dataset = erase_data,
#'   record_id_field = "record_id",
#'   prefix = "P",
#'   id_length = 8
#' )
#'
#' anonymised_data <- result$dataset
#' record_id_linkage <- result$linkage_file
#'
#' result_2 <- recode_record_id(
#'   dataset = second_dataset,
#'   record_id_field = "participant_id",
#'   linkage_file = record_id_linkage
#' )
#'
#' @export
recode_record_id <- function(dataset,
                             record_id_field = "record_id",
                             prefix = "P",
                             id_length = 8,
                             linkage_file = NULL,
                             overwrite_original = TRUE,
                             new_record_id_field = "anonymised_record_id") {
  
  generate_random_ids <- function(n, prefix, id_length, existing_ids = character()) {
    
    allowed_characters <- c(LETTERS, 0:9)
    generated_ids <- character()
    
    while (length(generated_ids) < n) {
      
      candidate <- paste0(
        prefix,
        paste0(
          sample(
            allowed_characters,
            size = id_length,
            replace = TRUE
          ),
          collapse = ""
        )
      )
      
      if (!candidate %in% c(existing_ids, generated_ids)) {
        generated_ids <- c(generated_ids, candidate)
      }
    }
    
    generated_ids
  }
  
  if (!is.data.frame(dataset)) {
    stop("dataset must be a data frame.", call. = FALSE)
  }
  
  if (!record_id_field %in% names(dataset)) {
    stop(
      paste0("record_id_field not found in dataset: ", record_id_field),
      call. = FALSE
    )
  }
  
  if (!is.numeric(id_length) || length(id_length) != 1 || id_length < 1) {
    stop("id_length must be a positive number.", call. = FALSE)
  }
  
  original_ids <- sort(unique(stats::na.omit(as.character(dataset[[record_id_field]]))))
  
  if (is.null(linkage_file)) {
    
    linkage_file <- tibble::tibble(
      original_record_id = character(),
      anonymised_record_id = character()
    )
    
  } else {
    
    if (!is.data.frame(linkage_file)) {
      stop("linkage_file must be a data frame if supplied.", call. = FALSE)
    }
    
    required_linkage_columns <- c(
      "original_record_id",
      "anonymised_record_id"
    )
    
    missing_linkage_columns <- setdiff(
      required_linkage_columns,
      names(linkage_file)
    )
    
    if (length(missing_linkage_columns) > 0) {
      stop(
        paste0(
          "linkage_file is missing required columns: ",
          paste(missing_linkage_columns, collapse = ", ")
        ),
        call. = FALSE
      )
    }
    
    linkage_file <- linkage_file |>
      dplyr::mutate(
        original_record_id = as.character(.data$original_record_id),
        anonymised_record_id = as.character(.data$anonymised_record_id)
      )
  }
  
  existing_original_ids <- linkage_file$original_record_id
  new_original_ids <- setdiff(original_ids, existing_original_ids)
  
  if (length(new_original_ids) > 0) {
    
    new_anonymised_ids <- generate_random_ids(
      n = length(new_original_ids),
      prefix = prefix,
      id_length = id_length,
      existing_ids = linkage_file$anonymised_record_id
    )
    
    new_linkage_rows <- tibble::tibble(
      original_record_id = new_original_ids,
      anonymised_record_id = new_anonymised_ids
    )
    
    linkage_file <- dplyr::bind_rows(
      linkage_file,
      new_linkage_rows
    )
  }
  
  id_lookup <- stats::setNames(
    linkage_file$anonymised_record_id,
    linkage_file$original_record_id
  )
  
  updated_dataset <- dataset
  
  recoded_ids <- unname(
    id_lookup[as.character(updated_dataset[[record_id_field]])]
  )
  
  if (overwrite_original) {
    updated_dataset[[record_id_field]] <- recoded_ids
    output_field <- record_id_field
  } else {
    updated_dataset[[new_record_id_field]] <- recoded_ids
    output_field <- new_record_id_field
  }
  
  change_log <- tibble::tibble(
    action = "recode_record_id",
    record_id_field = record_id_field,
    output_field = output_field,
    participants_in_dataset = length(original_ids),
    existing_linkages_reused = sum(original_ids %in% existing_original_ids),
    new_linkages_created = length(new_original_ids),
    linkage_rows_total = nrow(linkage_file)
  )
  
  list(
    dataset = updated_dataset,
    linkage_file = linkage_file,
    change_log = change_log
  )
}

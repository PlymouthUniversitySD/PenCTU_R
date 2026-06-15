#Title: ANON - Rename fields marked for anonymisation
#Author: Paigan Aspinall
#Version & Date: V1.0.0 01JUN2026
#R version: 4.4.3

#' Rename Fields Marked for Anonymisation
#'
#' Renames fields in a REDCap dataset where the annotated data dictionary
#' specifies \code{action == "rename"}.
#'
#' The new field name is taken from the \code{new_field_name} column in the
#' annotated data dictionary.
#'
#' REDCap checkbox fields are handled automatically. If a checkbox parent field
#' is renamed, all exported checkbox option columns are renamed while preserving
#' the REDCap checkbox suffix.
#'
#' For example:
#' \itemize{
#'   \item \code{symptoms___1} becomes \code{baseline_symptoms___1}
#'   \item \code{symptoms___2} becomes \code{baseline_symptoms___2}
#' }
#'
#' @param dataset A data frame containing the REDCap dataset.
#' @param data_dictionary A data frame containing the annotated REDCap data
#'   dictionary.
#' @param field_name_column Character string specifying the field name column in
#'   the data dictionary. Default is \code{"field_name"}.
#' @param data_type_column Character string specifying the REDCap field type
#'   column. Default is \code{"data_type"}.
#' @param action_column Character string specifying the anonymisation action
#'   column. Default is \code{"action"}.
#' @param new_field_name_column Character string specifying the new field name
#'   column. Default is \code{"new_field_name"}.
#'
#' @return A list containing:
#'   \describe{
#'     \item{dataset}{Dataset with fields renamed}
#'     \item{data_dictionary}{Data dictionary with field names updated}
#'     \item{change_log}{Summary of fields renamed}
#'   }
#'
#' @examples
#' result <- rename_anonymisation_fields(
#'   dataset = erase_data,
#'   data_dictionary = erase_data_dictionary
#' )
#'
#' updated_data <- result$dataset
#' updated_dictionary <- result$data_dictionary
#' change_log <- result$change_log
#'
#' @export
rename_anonymisation_fields <- function(dataset,
                                        data_dictionary,
                                        field_name_column = "field_name",
                                        data_type_column = "data_type",
                                        action_column = "action",
                                        new_field_name_column = "new_field_name") {
  
  if (!is.data.frame(dataset)) {
    stop("dataset must be a data frame.", call. = FALSE)
  }
  
  if (!is.data.frame(data_dictionary)) {
    stop("data_dictionary must be a data frame.", call. = FALSE)
  }
  
  required_columns <- c(
    field_name_column,
    data_type_column,
    action_column,
    new_field_name_column
  )
  
  missing_columns <- setdiff(required_columns, names(data_dictionary))
  
  if (length(missing_columns) > 0) {
    stop(
      paste0(
        "data_dictionary is missing required columns: ",
        paste(missing_columns, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  
  rename_rows <- data_dictionary |>
    dplyr::filter(
      tolower(trimws(as.character(.data[[action_column]]))) == "rename"
    )
  
  updated_dataset <- dataset
  updated_data_dictionary <- data_dictionary
  
  change_log <- tibble::tibble(
    action = character(),
    original_field_name = character(),
    new_field_name = character(),
    dataset_field_renamed = character(),
    status = character()
  )
  
  if (nrow(rename_rows) == 0) {
    return(
      list(
        dataset = updated_dataset,
        data_dictionary = updated_data_dictionary,
        change_log = change_log
      )
    )
  }
  
  for (i in seq_len(nrow(rename_rows))) {
    
    original_field_name <- as.character(rename_rows[[field_name_column]][[i]])
    data_type <- tolower(trimws(as.character(rename_rows[[data_type_column]][[i]])))
    new_field_name <- as.character(rename_rows[[new_field_name_column]][[i]])
    
    if (is.na(new_field_name) || trimws(new_field_name) == "") {
      stop(
        paste0(
          "new_field_name is missing for field marked rename: ",
          original_field_name
        ),
        call. = FALSE
      )
    }
    
    if (data_type == "checkbox") {
      
      checkbox_fields <- names(updated_dataset)[
        grepl(paste0("^", original_field_name, "___"), names(updated_dataset))
      ]
      
      if (length(checkbox_fields) == 0) {
        change_log <- dplyr::bind_rows(
          change_log,
          tibble::tibble(
            action = "rename",
            original_field_name = original_field_name,
            new_field_name = new_field_name,
            dataset_field_renamed = NA_character_,
            status = "checkbox parent not found in dataset"
          )
        )
      }
      
      for (checkbox_field in checkbox_fields) {
        
        checkbox_suffix <- sub(
          paste0("^", original_field_name),
          "",
          checkbox_field
        )
        
        renamed_checkbox_field <- paste0(new_field_name, checkbox_suffix)
        
        names(updated_dataset)[names(updated_dataset) == checkbox_field] <-
          renamed_checkbox_field
        
        change_log <- dplyr::bind_rows(
          change_log,
          tibble::tibble(
            action = "rename",
            original_field_name = original_field_name,
            new_field_name = new_field_name,
            dataset_field_renamed = paste0(
              checkbox_field,
              " -> ",
              renamed_checkbox_field
            ),
            status = "renamed"
          )
        )
      }
      
    } else {
      
      if (!original_field_name %in% names(updated_dataset)) {
        change_log <- dplyr::bind_rows(
          change_log,
          tibble::tibble(
            action = "rename",
            original_field_name = original_field_name,
            new_field_name = new_field_name,
            dataset_field_renamed = NA_character_,
            status = "field not found in dataset"
          )
        )
      } else {
        names(updated_dataset)[names(updated_dataset) == original_field_name] <-
          new_field_name
        
        change_log <- dplyr::bind_rows(
          change_log,
          tibble::tibble(
            action = "rename",
            original_field_name = original_field_name,
            new_field_name = new_field_name,
            dataset_field_renamed = paste0(
              original_field_name,
              " -> ",
              new_field_name
            ),
            status = "renamed"
          )
        )
      }
    }
    
    updated_data_dictionary[[field_name_column]][
      updated_data_dictionary[[field_name_column]] == original_field_name
    ] <- new_field_name
  }
  
  list(
    dataset = updated_dataset,
    data_dictionary = updated_data_dictionary,
    change_log = change_log
  )
}

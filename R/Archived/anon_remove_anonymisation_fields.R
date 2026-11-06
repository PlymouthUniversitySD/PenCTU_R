#Title: ANON - Remove fields marked for anonymisation
#Author: Paigan Aspinall
#Version & Date: V1.0.0 01JUN2026
#R version: 4.4.3

#' Remove Fields Marked for Anonymisation
#'
#' Removes fields from a REDCap dataset where the annotated data dictionary
#' specifies \code{action == "remove"}.
#'
#' REDCap checkbox fields are handled automatically. If a checkbox parent field
#' is marked for removal, all exported checkbox option columns are removed.
#' For example, if \code{field_name == "symptoms"} and the dataset contains
#' \code{symptoms___1}, \code{symptoms___2}, and \code{symptoms___3}, all three
#' exported columns will be removed.
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
#'
#' @return A list containing:
#'   \describe{
#'     \item{dataset}{Dataset with remove fields deleted}
#'     \item{data_dictionary}{Data dictionary with remove fields deleted}
#'     \item{change_log}{Summary of fields removed}
#'   }
#'
#' @examples
#' result <- remove_anonymisation_fields(
#'   dataset = erase_data,
#'   data_dictionary = erase_data_dictionary
#' )
#'
#' anonymised_data <- result$dataset
#' anonymised_dictionary <- result$data_dictionary
#' change_log <- result$change_log
#'
#' @export
remove_anonymisation_fields <- function(dataset,
                                        data_dictionary,
                                        field_name_column = "field_name",
                                        data_type_column = "data_type",
                                        action_column = "action") {
  
  if (!is.data.frame(dataset)) {
    stop("dataset must be a data frame.", call. = FALSE)
  }
  
  if (!is.data.frame(data_dictionary)) {
    stop("data_dictionary must be a data frame.", call. = FALSE)
  }
  
  required_columns <- c(field_name_column, data_type_column, action_column)
  
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
  
  remove_rows <- data_dictionary |>
    dplyr::filter(
      tolower(trimws(as.character(.data[[action_column]]))) == "remove"
    )
  
  fields_to_remove <- character()
  
  for (i in seq_len(nrow(remove_rows))) {
    
    field_name <- as.character(remove_rows[[field_name_column]][[i]])
    data_type <- tolower(trimws(as.character(remove_rows[[data_type_column]][[i]])))
    
    if (data_type == "checkbox") {
      checkbox_fields <- names(dataset)[
        grepl(paste0("^", field_name, "___"), names(dataset))
      ]
      
      fields_to_remove <- c(fields_to_remove, checkbox_fields)
      
    } else {
      fields_to_remove <- c(fields_to_remove, field_name)
    }
  }
  
  fields_to_remove <- unique(fields_to_remove)
  fields_found <- fields_to_remove[fields_to_remove %in% names(dataset)]
  fields_missing <- fields_to_remove[!fields_to_remove %in% names(dataset)]
  
  updated_dataset <- dataset |>
    dplyr::select(-dplyr::any_of(fields_found))
  
  removed_parent_fields <- remove_rows[[field_name_column]]
  
  updated_data_dictionary <- data_dictionary |>
    dplyr::filter(
      !.data[[field_name_column]] %in% removed_parent_fields
    )
  
  change_log <- tibble::tibble(
    action = "remove",
    field_name = fields_to_remove,
    status = dplyr::case_when(
      field_name %in% fields_found ~ "removed",
      field_name %in% fields_missing ~ "not found in dataset",
      TRUE ~ "unknown"
    )
  )
  
  list(
    dataset = updated_dataset,
    data_dictionary = updated_data_dictionary,
    change_log = change_log
  )
}

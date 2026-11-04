#Title: ATR - Number of changes per user
#Author: Paigan Aspinall
#Version & Date: V1.0.0 22MAY2026
#R version: 4.4.3

#' Identify User Data Changes
#'
#' Identifies unique users within the audit trail dataset and summarises the
#' number of qualifying data changes made by each user.
#'
#' A qualifying change is identified where:
#' \itemize{
#'   \item \code{action == "Update record"}
#'   \item \code{old_value} is not missing
#'   \item \code{old_value != "NA"}
#'   \item \code{old_value != new_value}
#' }
#'
#' The following fields are excluded:
#' \itemize{
#'   \item Fields annotated within the metadata dataset using:
#'   \itemize{
#'     \item \code{#DataManagement}
#'     \item \code{#SystemFunctionality}
#'     \item \code{#TrialManagement}
#'   }
#'   \item Fields with names ending in \code{"_complete"}
#' }
#' 
#' #' The following fields are excluded:
#' \itemize{
#'   \item Fields annotated within the metadata dataset using:
#'   \itemize{
#'     \item \code{#DataManagement}
#'     \item \code{#SystemFunctionality}
#'     \item \code{#TrialManagement}
#'   }
#'   \item Fields with names ending in \code{"_complete"}
#' }
#'
#' @param atr_data A data frame containing REDCap audit trail data. Expected
#'   columns include:
#'   \describe{
#'     \item{username}{Username associated with the audit action}
#'     \item{action}{Audit action performed}
#'     \item{old_value}{Previous value}
#'     \item{new_value}{Updated value}
#'   }
#'   
#'   #' @param metadata A data frame containing REDCap metadata. Expected columns
#'   include:
#'   \describe{
#'     \item{field_name}{Field/variable name}
#'     \item{field_annotation}{Field annotation text}
#'   }
#'
#'#' @param metadata A data frame containing REDCap metadata. Expected columns
#'   include:
#'   \describe{
#'     \item{field_name}{Field/variable name}
#'     \item{field_annotation}{Field annotation text}
#'   }
#'   
#' @return A data frame containing:
#'   \describe{
#'     \item{username}{Username associated with qualifying changes}
#'     \item{n_changes}{Total number of qualifying changes made by the user}
#'   }
#'
#' @examples
#' user_data_changes <- identify_user_data_changes(
#'   atr_data = atr_data,
#'   metadata = metadata
#' )
#' 
#' @export

identify_user_data_changes <- function(
    atr_data,
    metadata
) {
  
  excluded_fields <- metadata %>%
    filter(
      grepl(
        "#DataManagement|#SystemFunctionality|#TrialManagement",
        field_annotation
      )
    ) %>%
    pull(field_name)
  
  atr_data %>%
    filter(
      !is.na(username),
      username != "SYSTEM",
      username != "[survey respondent]",
      action == "Update record",
      !is.na(old_value),
      old_value != "NA",
      old_value != new_value,
      !field %in% excluded_fields,
      !grepl("_complete$", field)
    ) %>%
    group_by(username) %>%
    summarise(
      n_changes = n(),
      .groups = "drop"
    ) %>%
    arrange(
      desc(n_changes),
      username
    )
}
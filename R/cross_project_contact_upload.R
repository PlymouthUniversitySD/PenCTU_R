#Title: REDCap Cross-Project Contact Upload
#Author: Paigan Aspinall
#Version & Date: V1.0.0 06JUL2026
#R version: 4.4.3

#' Run REDCap Cross-Project Contact Upload
#'
#' Uses a mapping dataframe to pipe contact information between REDCap projects
#' via the REDCap API. Each row of the mapping dataframe defines one source field
#' to destination field transfer.
#'
#' The function supports:
#' \itemize{
#'   \item Source and destination API tokens
#'   \item Source event filtering
#'   \item Destination event filtering
#'   \item Optional study-level filtering, for example \code{record_id == "BOOST"}
#'   \item Optional site-level matching between source and destination projects
#'   \item Exclusion of repeating instances from destination uploads
#'   \item Row-level error handling and upload logging
#' }
#'
#' @param mapping A dataframe containing the cross-project upload specification.
#'   Expected columns include:
#'   \describe{
#'     \item{contact}{Human-readable contact/upload description}
#'     \item{source_api_token}{REDCap API token for the source project}
#'     \item{destination_api_token}{REDCap API token for the destination project}
#'     \item{source_project_identifier}{Source project name/identifier}
#'     \item{source_field_name}{Source REDCap field to pull from}
#'     \item{source_field_event}{Source REDCap event, where applicable}
#'     \item{destination_project_identifier}{Destination project name/identifier}
#'     \item{destination_field_name}{Destination REDCap field to populate}
#'     \item{destination_field_event}{Destination REDCap event, where applicable}
#'     \item{site_source_field}{Source site-matching field, where applicable}
#'     \item{site_destination_field}{Destination site-matching field, where applicable}
#'     \item{study_filter_field}{Source field used for study-level filtering}
#'     \item{study_filter}{Value to match within \code{study_filter_field}}
#'   }
#'
#' @param url REDCap API URL.
#'
#' @return A dataframe containing a row-level upload log with:
#'   \describe{
#'     \item{row_number}{Mapping row number}
#'     \item{contact}{Contact/upload description}
#'     \item{source_project}{Source project identifier}
#'     \item{source_field}{Source field name}
#'     \item{destination_project}{Destination project identifier}
#'     \item{destination_field}{Destination field name}
#'     \item{status}{Success or Error}
#'     \item{records_updated}{Number of records updated, where successful}
#'     \item{error_message}{Error message, where unsuccessful}
#'   }
#'
#' @examples
#' url <- "https://clinicaltrials-pre.plymouth.ac.uk/api/"
#'
#' mapping <- read.csv("BOOST_ContactUpload.csv")
#'
#' contact_upload_log <- run_contact_upload(
#'   mapping = mapping,
#'   url = url
#' )
#'
#' @export

cross_project_contact_upload <- function(mapping, url) {
  
  mapping <- mapping %>%
    mutate(across(everything(), ~na_if(trimws(as.character(.x)), "")))
  
  upload_log <- data.frame()
  
  for (i in seq_len(nrow(mapping))) {
    
    mapping_row <- mapping %>%
      slice(i)
    
    log_row <- data.frame(
      row_number = i,
      contact = mapping_row$contact,
      source_project = mapping_row$source_project_identifier,
      source_field = mapping_row$source_field_name,
      destination_project = mapping_row$destination_project_identifier,
      destination_field = mapping_row$destination_field_name,
      status = NA_character_,
      records_updated = NA_character_,
      error_message = NA_character_,
      stringsAsFactors = FALSE
    )
    
    tryCatch({
      
      token_source <- mapping_row$source_api_token
      token_destination <- mapping_row$destination_api_token
      
      # Export source project data from REDCap.
      formData <- list(
        token = token_source,
        content = "record",
        action = "export",
        format = "csv",
        type = "flat",
        csvDelimiter = "",
        rawOrLabel = "raw",
        rawOrLabelHeaders = "raw",
        exportCheckboxLabel = "false",
        exportSurveyFields = "false",
        exportDataAccessGroups = "true",
        returnFormat = "csv"
      )
      
      response <- httr::POST(url, body = formData, encode = "form")
      source_df <- httr::content(response)
      
      # Export destination project data from REDCap.
      formData <- list(
        token = token_destination,
        content = "record",
        action = "export",
        format = "csv",
        type = "flat",
        csvDelimiter = "",
        rawOrLabel = "raw",
        rawOrLabelHeaders = "raw",
        exportCheckboxLabel = "false",
        exportSurveyFields = "false",
        exportDataAccessGroups = "true",
        returnFormat = "csv"
      )
      
      response <- httr::POST(url, body = formData, encode = "form")
      destination_df <- httr::content(response)
      
      # Apply optional source study filter, for example record_id == BOOST.
      if (!is.na(mapping_row$study_filter_field) &&
          !is.na(mapping_row$study_filter)) {
        
        source_df <- source_df %>%
          filter(.data[[mapping_row$study_filter_field]] == mapping_row$study_filter)
      }
      
      # Apply optional source event filter.
      if (!is.na(mapping_row$source_field_event)) {
        source_df <- source_df %>%
          filter(redcap_event_name == mapping_row$source_field_event)
      }
      
      # Apply optional destination event filter and exclude repeating instances.
      if (!is.na(mapping_row$destination_field_event)) {
        
        destination_df <- destination_df %>%
          filter(
            redcap_event_name == mapping_row$destination_field_event,
            is.na(redcap_repeat_instance) |
              trimws(as.character(redcap_repeat_instance)) == "" |
              trimws(as.character(redcap_repeat_instance)) == "NA"
          )
        
        # Retain destination ID, event, and site field where site matching is required.
        if (!is.na(mapping_row$site_destination_field)) {
          
          destination_df <- destination_df %>%
            select(
              all_of(names(destination_df)[1]),
              redcap_event_name,
              all_of(mapping_row$site_destination_field)
            )
          
        } else {
          
          destination_df <- destination_df %>%
            select(
              all_of(names(destination_df)[1]),
              redcap_event_name
            )
        }
        
      } else {
        
        # Retain destination ID and site field where site matching is required.
        if (!is.na(mapping_row$site_destination_field)) {
          
          destination_df <- destination_df %>%
            select(
              all_of(names(destination_df)[1]),
              all_of(mapping_row$site_destination_field)
            )
          
        } else {
          
          destination_df <- destination_df %>%
            select(
              all_of(names(destination_df)[1])
            )
        }
      }
      
      # Site-matched transfer:
      # match source and destination rows using the configured site fields.
      if (!is.na(mapping_row$site_source_field)) {
        
        source_df <- source_df %>%
          select(
            all_of(mapping_row$site_source_field),
            all_of(mapping_row$source_field_name)
          )
        
        output_data <- merge(
          destination_df,
          source_df,
          by.x = mapping_row$site_destination_field,
          by.y = mapping_row$site_source_field,
          all.x = TRUE
        )
        
        output_data <- output_data %>%
          rename(
            !!mapping_row$destination_field_name := all_of(mapping_row$source_field_name)
          )
        
      } else {
        
        # Non-site-matched transfer:
        # take the first source value and apply it to all destination records.
        source_df <- source_df %>%
          select(all_of(mapping_row$source_field_name))
        
        source_value <- source_df[[mapping_row$source_field_name]][1]
        
        destination_df[[mapping_row$destination_field_name]] <- source_value
        
        output_data <- destination_df
      }
      
      # Convert prepared import dataframe to JSON for REDCap upload.
      upload_json <- jsonlite::toJSON(output_data, auto_unbox = TRUE)
      
      formData <- list(
        token = token_destination,
        content = "record",
        format = "json",
        type = "flat",
        overwriteBehavior = "normal",
        forceAutoNumber = "false",
        data = upload_json,
        returnContent = "count",
        returnFormat = "json"
      )
      
      response <- httr::POST(url, body = formData, encode = "form")
      result_data <- httr::content(response)
      
      log_row$status <- "Success"
      log_row$records_updated <- as.character(result_data$count)
      
    }, error = function(e) {
      
      log_row$status <- "Error"
      log_row$error_message <- conditionMessage(e)
      
    })
    
    upload_log <- bind_rows(upload_log, log_row)
  }
  
  return(upload_log)
}
    
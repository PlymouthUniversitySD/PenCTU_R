#Title: NC Log - Identify and Log Unactioned Non-Conformances
#Author: Paigan Aspinall
#Version & Date: V1.0.0 24AUG2026
#R version: 4.4.3
#' Build Non-Conformance Log and Flag Actioned Records
#'
#' Loops through every row of a non-conformance (NC) metadata preset dataset,
#' identifies records that trigger that NC and have not yet been actioned,
#' builds a corresponding NC log entry for each affected record, and uploads
#' a "checked" flag back to the source REDCap project so the same record is
#' not re-identified on a subsequent run.
#'
#' For each metadata row, a record is considered affected where:
#' \itemize{
#'   \item the field named in \code{nc_trigger_field} equals \code{1}
#'   \item the field named in \code{nc_dm_field} is empty (either \code{NA} or \code{""})
#' }
#'
#' For every affected record, this function:
#' \itemize{
#'   \item appends one row to the returned NC log, combining the metadata's
#'     description with any site notes recorded in \code{nc_notes_field}
#'   \item uploads the record's identifying fields (\code{record_id},
#'     \code{redcap_event_name}, \code{redcap_repeat_instance},
#'     \code{redcap_repeat_instrument}) back to REDCap with the field named
#'     in \code{nc_dm_field} set to \code{1}, marking it as actioned
#' }
#'
#' The REDCap project and API token used for each metadata row are selected
#' dynamically from \code{datasets} and \code{tokens} using the value in the
#' metadata's \code{system} column - metadata rows whose \code{system} value
#' has no matching entry in \code{datasets} or \code{tokens} are skipped with
#' a warning.
#'
#' @param nc_metadata A data frame of NC preset metadata, one row per rule.
#'   Expected columns include:
#'   \describe{
#'     \item{nc_trigger_field}{Name of the field that flags an affected record (value \code{1} = triggered)}
#'     \item{nc_notes_field}{Name of the field containing site notes to append to the log description}
#'     \item{nc_dm_field}{Name of the field used to mark a record as actioned}
#'     \item{timepoint}{REDCap event name the rule applies to}
#'     \item{system}{Name identifying which dataset/project the rule applies to (must match a name in \code{datasets} and \code{tokens})}
#'     \item{nc_description}{Description of the non-conformance, used in the log entry}
#'     \item{category}{Category of the non-conformance, used in the log entry}
#'   }
#' @param initials Character. Initials of the person running the function, recorded in the log's \code{ctu_person} field.
#' @param job_role Character. Job title of the person running the function, recorded in the log's \code{ctu_person} field.
#' @param datasets A named list of data frames, one per REDCap project, keyed by the same names used in \code{nc_metadata$system}.
#' @param tokens A named list of REDCap API tokens, one per project, keyed by the same names used in \code{nc_metadata$system}.
#' @param url Character. REDCap API URL used for both export and upload requests.
#'
#' @return A data frame containing one row per affected record, with columns:
#'   \describe{
#'     \item{status}{Log status, currently always \code{"Closed"}}
#'     \item{log_number}{Reserved for manual completion}
#'     \item{database_id}{Reserved for manual completion}
#'     \item{ctu_person}{Initials and job role of the person who ran the function}
#'     \item{participant_id}{Identifier of the affected record (first column of the dataset)}
#'     \item{description}{NC description combined with any site notes}
#'     \item{remedial_action}{Reserved for manual completion}
#'     \item{nc_date}{Date the non-conformance was identified}
#'     \item{report_date}{Date the non-conformance was reported}
#'     \item{tm_date}{Reserved for manual completion}
#'     \item{category}{Category of the non-conformance}
#'     \item{classification}{Reserved for manual completion}
#'   }
#'
#' As a side effect, the function also uploads a "checked" flag
#' (\code{nc_dm_field = 1}) back to the relevant REDCap project for every
#' affected record processed. A warning is issued (rather than the function
#' stopping) if an individual upload fails or if a metadata row references an
#' unrecognised dataset/token.
#'
#' @examples
#' nc_log <- build_nc_log(
#'   nc_metadata = nc_metadata,
#'   initials    = initials,
#'   job_role    = job_role,
#'   datasets    = list(families = families, participants = participants),
#'   tokens      = list(families = family_token, participants = participant_token),
#'   url         = url
#' )
#'
#' @export

build_nc_log <- function(nc_metadata, initials, job_role, datasets, tokens, url) {
  
  today  <- Sys.Date()
  nc_log <- data.frame()
  
  for (mi in seq_len(nrow(nc_metadata))) {
    
    meta_row <- nc_metadata[mi, ]
    
    trigger      <- meta_row$nc_trigger_field
    notes        <- meta_row$nc_notes_field
    dm_check     <- meta_row$nc_dm_field
    timepoint    <- meta_row$timepoint
    dataset_name <- meta_row$system
    description  <- meta_row$nc_description
    category     <- meta_row$category
    
    dataset   <- datasets[[dataset_name]]
    api_token <- tokens[[dataset_name]]
    
    if (is.null(dataset) || is.null(api_token)) {
      warning("No dataset/token found for '", dataset_name, "' - skipping metadata row ", mi)
      next
    }
    
    #Isolate the timepoint data
    timepoint_data <- subset(dataset, redcap_event_name == timepoint)
    
    #Isolate affected rows: trigger fired AND dm_check hasn't been actioned yet
    affected_rows <- subset(timepoint_data,
                            get(trigger) == 1 &
                              (is.na(get(dm_check)) | get(dm_check) == ""))
    
    if (nrow(affected_rows) == 0) next
    
    for (ai in seq_len(nrow(affected_rows))) {
      
      affected_row <- affected_rows[ai, ]
      
      #Build the nc_log entry
      new_row <- data.frame(
        status          = "Closed",
        log_number      = "",
        database_id     = "NA",
        ctu_person      = paste0(initials, ', ', job_role),
        participant_id  = affected_row[[1]],
        description     = paste0(description, " Site notes: ", notes),
        remedial_action = "NA",
        nc_date         = today,
        report_date     = today,
        tm_date         = "",
        category        = category,
        classification  = ""
      )
      nc_log <- rbind(nc_log, new_row)
      
      #Build the upload payload: identifying columns + dm_check flipped to 1
      selected_cols <- c(colnames(affected_row)[1],
                         "redcap_event_name",
                         "redcap_repeat_instance",
                         "redcap_repeat_instrument",
                         dm_check)
      affected_row_selected <- affected_row[, selected_cols]
      affected_row_selected[[dm_check]] <- 1
      
      upload_json <- jsonlite::toJSON(
        affected_row_selected,
        auto_unbox = TRUE,
        na = "null"
      )
      
      #Upload to whichever project this row actually came from
      formData <- list(
        "token"            = api_token,
        "content"          = "record",
        "format"           = "json",
        "type"             = "flat",
        "overwriteBehavior" = "normal",
        "forceAutoNumber"  = "false",
        "data"             = upload_json,
        "returnContent"    = "count",
        "returnFormat"     = "json"
      )
      response    <- httr::POST(url, body = formData, encode = "form")
      http_status <- httr::status_code(response)
      
      if (http_status != 200) {
        warning("Upload failed for participant ", affected_row[[1]],
                " (", dataset_name, "), status ", http_status,
                ": ", httr::content(response, as = "text", encoding = "UTF-8"))
      }
    }
  }
  
  nc_log
}

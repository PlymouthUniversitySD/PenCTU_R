#Title: NC Log - Identify and Log Unactioned Non-Conformances
#Author: Paigan Aspinall
#Version & Date: V2.1.0 03NOV2026
#R version: 4.4.3

#' Build Non-Conformance Log and Flag Actioned Records
#'
#' Identifies records that trigger a non-conformance (NC) defined in an NC
#' metadata preset dataset and have not yet been actioned, writes an NC log
#' file, and then uploads a "checked" flag to the source REDCap project so the
#' same record is not identified again on a later run.
#'
#' The function works in four stages, and nothing is uploaded until the first
#' three have completed:
#' \enumerate{
#'   \item Checks the inputs and stops if anything needed is missing (see
#'     Checks below).
#'   \item Identifies affected records for every metadata row.
#'   \item Writes the NC log to \code{log_file}.
#'   \item Uploads the flags, one import per REDCap project and flag field, and
#'     stops if an import fails or REDCap reports a different number of records
#'     from the number sent.
#' }
#'
#' Both longitudinal and classic (non-longitudinal) projects are supported. A
#' project is treated as longitudinal when its dataset has a
#' \code{redcap_event_name} column. For classic projects, leave \code{timepoint}
#' blank in the metadata.
#'
#' A record (row) is affected by a metadata row where:
#' \itemize{
#'   \item for longitudinal projects, \code{redcap_event_name} equals the
#'     metadata \code{timepoint};
#'   \item the field named in \code{nc_trigger_field} equals \code{1}
#'     (blank, \code{NA}, \code{0} or any other value = not triggered); and
#'   \item the field named in \code{nc_dm_field} is empty (\code{NA}, \code{""}
#'     or white space).
#' }
#'
#' @section Checks:
#' The function stops, before anything is written or uploaded, if:
#' \itemize{
#'   \item a required metadata column is missing, or a required metadata value
#'     is blank (\code{nc_notes_field} may be blank);
#'   \item a \code{system} value has no matching entry in \code{datasets} or
#'     \code{tokens};
#'   \item a field named in the metadata is not in the matching dataset;
#'   \item \code{timepoint} is blank for a longitudinal project, or is given
#'     for a classic project;
#'   \item \code{log_file} already exists.
#' }
#' Exact duplicate metadata rows are removed with a warning. Metadata events
#' that do not appear in the matching dataset are listed in a message, as they
#' may be misspelt (or may simply have no data yet).
#'
#' @param nc_metadata A data frame of NC preset metadata, one row per rule, with
#'   columns \code{nc_trigger_field}, \code{nc_notes_field}, \code{nc_dm_field},
#'   \code{timepoint} (unique event name; blank for classic projects),
#'   \code{system}, \code{nc_description} and \code{category}.
#' @param initials Character. Initials of the person running the function.
#' @param job_role Character. Job title of the person running the function.
#' @param datasets A named list of data frames (REDCap exports, raw values),
#'   one per project, named as in \code{nc_metadata$system}. The first column
#'   must be the record ID.
#' @param tokens A named list of REDCap API tokens, named as in \code{datasets}.
#' @param url Character. REDCap API URL.
#' @param log_file Character. Path of the CSV file to write the NC log to. Must
#'   not already exist. Required when \code{upload = TRUE}.
#' @param upload Logical. If \code{FALSE}, the NC log is returned but no file is
#'   written and nothing is uploaded (a dry run for checking the output).
#'
#' @return A data frame with one row per affected record, event and instance
#'   for each metadata rule, with columns \code{status} ("Closed"),
#'   \code{log_number} (""), \code{database_id} ("NA"), \code{ctu_person}
#'   ("initials, job role"), \code{participant_id}, \code{description}
#'   (NC description, event (longitudinal projects), repeat instance where
#'   applicable, and site notes
#'   where recorded), \code{remedial_action} ("NA"), \code{nc_date} and
#'   \code{report_date} (date of the run), \code{tm_date} (""),
#'   \code{category} and \code{classification} ("").
#'
#' @examples
#' \dontrun{
#' nc_log <- build_nc_log(
#'   nc_metadata = nc_metadata,
#'   initials    = initials,
#'   job_role    = job_role,
#'   datasets    = list(families = families, participants = participants),
#'   tokens      = list(families = family_token, participants = participant_token),
#'   url         = url,
#'   log_file    = "NC_LOG_20261027_0915.csv"
#' )
#' }
#'
#' @param script Optional. Name and version of the script that ran this, e.g.
#'   "TEMPLATE_AutomatedNonCompliances V2.1.0". It is added, with this
#'   function's name and version, to the end of each NC description.
#'
#' @export
build_nc_log <- function(nc_metadata, initials, job_role, datasets, tokens, url,
                         log_file = NULL, upload = TRUE, script = NULL) {

  # ---- Name and version of this function (and the script that ran it), added to the output ----
  version <- "2.1.0"
  if (!is.null(script) && (length(script) != 1 || is.na(script) || !is.character(script))) {
    stop("script must be a single piece of text, e.g. \"TEMPLATE_X V2.1.0\", or left out.")
  }
  generated_by <- paste0("build_nc_log V", version,
                         if (!is.null(script) && trimws(script) != "") paste0(" (", trimws(script), ")") else "")

  today <- Sys.Date()
  log_cols <- c("status", "log_number", "database_id", "ctu_person",
                "participant_id", "description", "remedial_action", "nc_date",
                "report_date", "tm_date", "category", "classification")
  repeat_cols <- c("redcap_repeat_instrument", "redcap_repeat_instance")

  is_blank <- function(x) is.na(x) | trimws(as.character(x)) == ""

  # ---- 1. Check inputs (nothing is written or uploaded before this passes) ----
  required <- c("nc_trigger_field", "nc_notes_field", "nc_dm_field", "timepoint",
                "system", "nc_description", "category")
  missing_cols <- setdiff(required, names(nc_metadata))
  if (length(missing_cols) > 0) {
    stop("nc_metadata is missing column(s): ", paste(missing_cols, collapse = ", "))
  }
  nc_metadata <- as.data.frame(lapply(nc_metadata[required], as.character),
                               stringsAsFactors = FALSE)
  for (col in c("nc_notes_field", "timepoint")) {
    nc_metadata[[col]][is_blank(nc_metadata[[col]])] <- ""
  }
  for (col in setdiff(required, c("nc_notes_field", "timepoint"))) {
    blank_rows <- which(is_blank(nc_metadata[[col]]))
    if (length(blank_rows) > 0) {
      stop("nc_metadata column '", col, "' is blank in row(s): ",
           paste(blank_rows, collapse = ", "))
    }
  }
  dup <- duplicated(nc_metadata)
  if (any(dup)) {
    warning("Removed ", sum(dup), " duplicate nc_metadata row(s): ",
            paste(which(dup), collapse = ", "))
    nc_metadata <- nc_metadata[!dup, , drop = FALSE]
  }
  if (upload) {
    if (is.null(log_file) || is_blank(log_file)) {
      stop("log_file must be given when upload = TRUE")
    }
    if (file.exists(log_file)) {
      stop("log_file '", log_file, "' already exists; use a new file name")
    }
  }

  for (sys in unique(nc_metadata$system)) {
    if (is.null(datasets[[sys]]) || !is.data.frame(datasets[[sys]])) {
      stop("No dataset (data frame) found in 'datasets' for system '", sys, "'")
    }
    if (upload && is.null(tokens[[sys]])) {
      stop("No API token found in 'tokens' for system '", sys, "'")
    }
    ds <- datasets[[sys]]
    rows <- nc_metadata$system == sys
    longitudinal <- "redcap_event_name" %in% names(ds)
    if (longitudinal && any(nc_metadata$timepoint[rows] == "")) {
      stop("Dataset '", sys, "' is longitudinal, but nc_metadata timepoint is blank in row(s): ",
           paste(which(rows & nc_metadata$timepoint == ""), collapse = ", "))
    }
    if (!longitudinal && any(nc_metadata$timepoint[rows] != "")) {
      stop("Dataset '", sys, "' has no redcap_event_name column (classic project), ",
           "but nc_metadata gives a timepoint in row(s): ",
           paste(which(rows & nc_metadata$timepoint != ""), collapse = ", "),
           ". Leave timepoint blank for classic projects.")
    }
    fields <- unique(c(nc_metadata$nc_trigger_field[rows],
                       nc_metadata$nc_dm_field[rows],
                       nc_metadata$nc_notes_field[rows]))
    fields <- fields[fields != ""]
    missing_fields <- setdiff(fields, names(ds))
    if (length(missing_fields) > 0) {
      stop("Field(s) named in nc_metadata not found in dataset '", sys, "': ",
           paste(missing_fields, collapse = ", "))
    }
    missing_events <- if (longitudinal) setdiff(unique(nc_metadata$timepoint[rows]),
                                                unique(as.character(ds$redcap_event_name))) else character(0)
    if (length(missing_events) > 0) {
      message("Event(s) in nc_metadata with no data in dataset '", sys,
              "' (check the spelling): ", paste(missing_events, collapse = ", "))
    }
  }

  # Each NC description ends with how the NC was identified, e.g.
  # "... Identified by build_nc_log V2.1.0 (TEMPLATE_AutomatedNonCompliances V2.1.0)."
  with_note <- function(x) {
    x <- trimws(x)
    paste0(x, ifelse(grepl("\\.$", x), " ", ". "), "Identified by ", generated_by, ".")
  }

  # ---- 2. Identify affected records ----
  log_rows <- list()
  flag_rows <- list()

  for (mi in seq_len(nrow(nc_metadata))) {
    m  <- nc_metadata[mi, ]
    ds <- datasets[[m$system]]
    longitudinal <- "redcap_event_name" %in% names(ds)
    for (rc in setdiff(repeat_cols, names(ds))) ds[[rc]] <- NA
    if (!longitudinal) ds$redcap_event_name <- NA

    trig <- suppressWarnings(as.numeric(as.character(ds[[m$nc_trigger_field]])))
    in_event <- if (longitudinal) as.character(ds$redcap_event_name) == m$timepoint else TRUE
    hit <- in_event &
      !is.na(trig) & trig == 1 &
      is_blank(ds[[m$nc_dm_field]])
    hit[is.na(hit)] <- FALSE
    affected <- ds[hit, , drop = FALSE]
    if (nrow(affected) == 0) next

    record_id <- as.character(affected[[1]])
    instance  <- as.character(affected$redcap_repeat_instance)
    location  <- if (longitudinal) {
      paste0(" Event: ", m$timepoint,
             ifelse(is_blank(instance), "", paste0(", instance ", instance)), ".")
    } else {
      ifelse(is_blank(instance), "", paste0(" Instance ", instance, "."))
    }
    notes <- if (m$nc_notes_field == "") rep("", nrow(affected)) else
      as.character(affected[[m$nc_notes_field]])
    notes_text <- ifelse(is_blank(notes), "", paste0(" Site notes: ", trimws(notes)))

    log_rows[[length(log_rows) + 1]] <- data.frame(
      status          = "Closed",
      log_number      = "",
      database_id     = "NA",
      ctu_person      = paste0(initials, ", ", job_role),
      participant_id  = record_id,
      description     = with_note(paste0(m$nc_description, location, notes_text)),
      remedial_action = "NA",
      nc_date         = today,
      report_date     = today,
      tm_date         = "",
      category        = m$category,
      classification  = "",
      stringsAsFactors = FALSE
    )

    flag_rows[[length(flag_rows) + 1]] <- data.frame(
      system    = m$system,
      id_field  = names(ds)[1],
      record_id = record_id,
      redcap_event_name        = as.character(affected$redcap_event_name),
      redcap_repeat_instrument = as.character(affected$redcap_repeat_instrument),
      redcap_repeat_instance   = instance,
      dm_field  = m$nc_dm_field,
      stringsAsFactors = FALSE
    )
  }

  if (length(log_rows) == 0) {
    message("No new non-conformances found. Nothing written or uploaded.")
    empty <- as.data.frame(setNames(replicate(length(log_cols), character(0),
                                              simplify = FALSE), log_cols))
    return(empty)
  }

  nc_log <- do.call(rbind, log_rows)[, log_cols]
  rownames(nc_log) <- NULL
  flags  <- unique(do.call(rbind, flag_rows))

  if (!upload) {
    message(nrow(nc_log), " non-conformance(s) found. Dry run: nothing written or uploaded.")
    return(nc_log)
  }

  # ---- 3. Write the log before anything is uploaded ----
  utils::write.csv(nc_log, log_file, row.names = FALSE)
  message("NC log with ", nrow(nc_log), " row(s) written to ", log_file)

  # ---- 4. Upload flags: one import per project and flag field ----
  for (sys in unique(flags$system)) {
    has_repeat <- all(repeat_cols %in% names(datasets[[sys]]))
    has_event  <- "redcap_event_name" %in% names(datasets[[sys]])
    for (dm in unique(flags$dm_field[flags$system == sys])) {
      f <- unique(flags[flags$system == sys & flags$dm_field == dm, , drop = FALSE])

      payload <- data.frame(record_id = f$record_id, stringsAsFactors = FALSE)
      names(payload)[1] <- f$id_field[1]
      if (has_event) payload$redcap_event_name <- f$redcap_event_name
      if (has_repeat) {
        payload$redcap_repeat_instrument <- ifelse(is.na(f$redcap_repeat_instrument), "",
                                                   f$redcap_repeat_instrument)
        payload$redcap_repeat_instance <- ifelse(is.na(f$redcap_repeat_instance), "",
                                                 f$redcap_repeat_instance)
      }
      payload[[dm]] <- "1"

      upload_json <- jsonlite::toJSON(payload, auto_unbox = TRUE, dataframe = "rows")
      formData <- list(
        token             = tokens[[sys]],
        content           = "record",
        action            = "import",
        format            = "json",
        type              = "flat",
        overwriteBehavior = "normal",
        forceAutoNumber   = "false",
        data              = upload_json,
        returnContent     = "count",
        returnFormat      = "json"
      )
      response    <- httr::POST(url, body = formData, encode = "form")
      http_status <- httr::status_code(response)
      body_text   <- httr::content(response, as = "text", encoding = "UTF-8")

      expected <- length(unique(f$record_id))
      count <- tryCatch(as.integer(jsonlite::fromJSON(body_text)$count),
                        error = function(e) NA_integer_)
      if (http_status != 200 || length(count) != 1 || is.na(count) || count != expected) {
        stop("Upload of ", dm, " flags to '", sys, "' failed (HTTP ", http_status,
             ", REDCap response: ", substr(body_text, 1, 300), "). ",
             "The NC log has been written to ", log_file, ". These flags were NOT ",
             "saved; check which flags are in REDCap before re-running, as earlier ",
             "uploads may already have been saved.")
      }
      message(dm, " flags uploaded to '", sys, "' for ", nrow(payload),
              " row(s) (", expected, " record(s)).")
    }
  }

  nc_log
}

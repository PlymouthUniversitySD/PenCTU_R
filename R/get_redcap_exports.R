#Title: Get REDCap Exports Function
#Author: Paigan Aspinall
#Version & Date: V2.1.0 DDMMMYYYY
#R version: 4.4.3

#' Export multiple REDCap reports using metadata
#'
#' Exports each REDCap report listed in a metadata table through the API and
#' saves each one, exactly as REDCap sends it, as a CSV file named
#' \code{study_name_export_name_YYYY-MM-DD.csv}. An export log
#' (\code{study_name_ExportLog_YYYY-MM-DD.csv}) records what was exported.
#'
#' The function works in three stages:
#' \enumerate{
#'   \item Checks the inputs and the metadata, and stops with a clear message
#'     before anything is exported if something is wrong.
#'   \item If any of today's files already exist in \code{output_dir}, lists
#'     them and asks the user to confirm overwriting them (a pop-up box in
#'     RStudio; otherwise type Y in the console).
#'   \item Exports each report. If an export fails, the others still run; a
#'     warning at the end lists every export that failed.
#' }
#'
#' @param export_metadata A data frame with columns \code{export_id} (REDCap
#'   report ID), \code{export_name} (used in the file name; letters, numbers,
#'   \code{_} and \code{-} only, and unique) and \code{format} (\code{"raw"},
#'   \code{"labelled"} or \code{"label_headers"}). Completely blank rows are
#'   ignored; any other incomplete row is an error.
#' @param token REDCap API token. Read it with \code{keyring::key_get()}.
#' @param environment \code{"live"} or \code{"test"}.
#' @param study_name Study name used at the start of each file name.
#' @param output_dir Folder to save the files in. Created if it does not exist.
#'   Must not be inside a Git repository. Defaults to
#'   \code{Documents/REDCap_exports/<study_name>}.
#' @param overwrite \code{NULL} (default) asks (pop-up box in RStudio) before overwriting any of
#'   today's files; \code{TRUE} overwrites without asking; \code{FALSE} stops
#'   instead of overwriting.
#'
#' @return Invisibly, the export log as a data frame: one row per export with
#'   \code{export_id}, \code{export_name}, \code{format}, \code{file},
#'   \code{status} ("Exported" or "Failed"), \code{rows}, \code{columns},
#'   \code{exported_at} and \code{message}.
#'
#' @details Formats: \code{"raw"} = raw values and raw column names;
#'   \code{"labelled"} = labels and labelled column names;
#'   \code{"label_headers"} = raw values and labelled column names. Checkbox
#'   fields are exported as "Checked"/"Unchecked" in labelled exports.
#'
#' @examples
#' \dontrun{
#' get_redcap_exports(export_metadata, keyring::key_get("STUDY_TOKEN"),
#'                    environment = "live", study_name = "STUDY")
#' }
#' @param script Optional. Name and version of the script that ran this, e.g.
#'   "TEMPLATE_GetExports V2.1.0". It is added, with this function's name and
#'   version, to the export log (column generated_by). The report files are not changed.
#'
#' @export
get_redcap_exports <- function(export_metadata, token, environment, study_name,
                               output_dir = file.path(path.expand("~"), "REDCap_exports", study_name),
                               overwrite = NULL, script = NULL) {

  # ---- Name and version of this function (and the script that ran it), added to the output ----
  version <- "2.1.0"
  if (!is.null(script) && (length(script) != 1 || is.na(script) || !is.character(script))) {
    stop("script must be a single piece of text, e.g. \"TEMPLATE_X V2.1.0\", or left out.")
  }
  generated_by <- paste0("get_redcap_exports V", version,
                         if (!is.null(script) && trimws(script) != "") paste0(" (", trimws(script), ")") else "")

  urls <- c(live = "https://clinicaltrials.plymouth.ac.uk/api/",
            test = "https://clinicaltrials-pre.plymouth.ac.uk/api/")
  formats <- c("raw", "labelled", "label_headers")
  blank <- function(x) is.na(x) | trimws(as.character(x)) == ""

  # ---- 1. Check inputs (nothing is exported before this passes) ----
  env <- tolower(trimws(as.character(environment)))
  if (length(env) != 1 || !env %in% names(urls)) {
    stop("environment must be \"live\" or \"test\" (you gave \"", environment, "\").")
  }
  url <- urls[[env]]
  if (length(token) != 1 || blank(token)) stop("token is empty. Read it with keyring::key_get().")
  if (length(study_name) != 1 || blank(study_name) || grepl("[^A-Za-z0-9_-]", study_name)) {
    stop("study_name must contain only letters, numbers, _ and -.")
  }

  missing_cols <- setdiff(c("export_id", "export_name", "format"), names(export_metadata))
  if (length(missing_cols) > 0) {
    stop("export_metadata is missing column(s): ", paste(missing_cols, collapse = ", "))
  }
  md <- data.frame(export_id   = trimws(as.character(export_metadata$export_id)),
                   export_name = trimws(as.character(export_metadata$export_name)),
                   format      = tolower(trimws(as.character(export_metadata$format))),
                   row = seq_len(nrow(export_metadata)), stringsAsFactors = FALSE)
  md[is.na(md)] <- ""
  md <- md[!(md$export_id == "" & md$export_name == "" & md$format == ""), , drop = FALSE]
  if (nrow(md) == 0) stop("export_metadata has no exports listed.")

  bad <- md$row[!grepl("^[0-9]+$", md$export_id) | md$export_name == "" | !md$format %in% formats]
  if (length(bad) > 0) {
    stop("export_metadata row(s) ", paste(bad, collapse = ", "), " are incomplete or invalid. ",
         "Each row needs a numeric export_id, an export_name and a format of ",
         paste(formats, collapse = ", "), ". Delete any instruction text from the file.")
  }
  bad_names <- md$export_name[grepl("[^A-Za-z0-9_-]", md$export_name)]
  if (length(bad_names) > 0) {
    stop("export_name must contain only letters, numbers, _ and -: ", paste(bad_names, collapse = ", "))
  }
  dups <- unique(md$export_name[duplicated(md$export_name)])
  if (length(dups) > 0) stop("export_name must be unique; repeated: ", paste(dups, collapse = ", "))

  # output folder: create if needed, never inside a Git repository
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(output_dir)) stop("Could not create output_dir: ", output_dir)
  out_abs <- normalizePath(output_dir, winslash = "/")
  d <- out_abs
  repeat {
    if (file.exists(file.path(d, ".git"))) {
      stop("output_dir is inside a Git repository (", d, "). Save exports outside any repository.")
    }
    parent <- dirname(d)
    if (parent == d) break
    d <- parent
  }

  today <- format(Sys.Date(), "%Y-%m-%d")
  md$file <- file.path(out_abs, paste0(study_name, "_", md$export_name, "_", today, ".csv"))
  log_file <- file.path(out_abs, paste0(study_name, "_ExportLog_", today, ".csv"))

  # ---- 2. Overwrite check ----
  existing <- c(md$file, log_file)[file.exists(c(md$file, log_file))]
  if (length(existing) > 0) {
    if (is.null(overwrite)) {
      listing <- paste0("  ", basename(existing), collapse = "\n")
      cat("These files from today already exist:\n", listing, "\n", sep = "")
      if (!is.null(getOption("penctu.readline"))) {
        # used by the automated tests
        overwrite <- identical(toupper(trimws(getOption("penctu.readline")("Overwrite? "))), "Y")
      } else if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
        # In RStudio: a pop-up box, so it works however the script is run
        overwrite <- isTRUE(rstudioapi::showQuestion(
          "Overwrite today's exports?",
          paste0("These files from today already exist and will be overwritten:\n\n", listing),
          ok = "Overwrite", cancel = "Stop"))
      } else {
        overwrite <- identical(toupper(trimws(readline("Type Y to overwrite them, or anything else to stop: "))), "Y")
      }
    }
    if (!isTRUE(overwrite)) stop("Stopped: existing files were not overwritten. Nothing was exported.")
    # Files open in another program (e.g. Excel) cannot be overwritten - check before exporting
    locked <- existing[!vapply(existing, function(f) {
      con <- tryCatch(suppressWarnings(file(f, open = "ab")), error = function(e) NULL)
      if (is.null(con)) return(FALSE)
      close(con); TRUE
    }, logical(1))]
    if (length(locked) > 0) {
      stop("These files are open in another program (e.g. Excel). Close them and run again. ",
           "Nothing was exported:\n", paste0("  ", basename(locked), collapse = "\n"))
    }
  }

  # ---- 3. Export each report ----
  log <- data.frame(export_id = md$export_id, export_name = md$export_name, format = md$format,
                    file = basename(md$file), status = "Failed", rows = NA_integer_,
                    columns = NA_integer_, exported_at = "", message = "", generated_by = generated_by,
                    stringsAsFactors = FALSE)

  for (i in seq_len(nrow(md))) {
    fmt <- md$format[i]
    body <- list(
      token = token, content = "report", format = "csv", report_id = md$export_id[i],
      csvDelimiter = "",
      rawOrLabel = if (fmt %in% c("raw", "label_headers")) "raw" else "label",
      rawOrLabelHeaders = if (fmt == "raw") "raw" else "label",
      exportCheckboxLabel = "false", returnFormat = "json"
    )
    response <- tryCatch(httr::POST(url, body = body, encode = "form"),
                         error = function(e) e)
    log$exported_at[i] <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    if (inherits(response, "error")) {
      log$message[i] <- paste("Could not connect:", conditionMessage(response))
      next
    }
    status <- httr::status_code(response)
    raw <- httr::content(response, as = "raw")
    if (status != 200) {
      log$message[i] <- paste0("HTTP ", status, ": ", substr(rawToChar(raw), 1, 200))
      next
    }

    # Save exactly what REDCap sent (keeps special characters intact)
    saved <- tryCatch({ writeBin(raw, md$file[i]); TRUE }, error = function(e) FALSE)
    if (!saved) {
      log$message[i] <- "Could not save the file (is it open in another program, e.g. Excel?)"
      next
    }

    parsed <- tryCatch(utils::read.csv(text = rawToChar(raw), check.names = FALSE,
                                       colClasses = "character", encoding = "UTF-8"),
                       error = function(e) NULL)
    log$status[i]  <- "Exported"
    log$rows[i]    <- if (is.null(parsed)) NA_integer_ else nrow(parsed)
    log$columns[i] <- if (is.null(parsed)) NA_integer_ else ncol(parsed)
    message("Saved: ", basename(md$file[i]), " (", log$rows[i], " rows, ", log$columns[i], " columns)")
  }

  log$message <- gsub(token, "[token]", log$message, fixed = TRUE)
  tryCatch(utils::write.csv(log, log_file, row.names = FALSE, na = ""),
           error = function(e) warning("Could not save the export log (is it open in Excel?): ", log_file, call. = FALSE))
  message("Export log saved: ", basename(log_file), "\nFiles are in: ", out_abs)

  failed <- log[log$status == "Failed", , drop = FALSE]
  if (nrow(failed) > 0) {
    warning(nrow(failed), " of ", nrow(log), " export(s) FAILED and were not saved:\n",
            paste0("  ", failed$export_name, " (report ", failed$export_id, "): ", failed$message,
                   collapse = "\n"),
            call. = FALSE)
  }
  invisible(log)
}

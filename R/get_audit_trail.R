#Title: Get processed audit trail data
#Author: Paigan Aspinall
#Version & Date: V3.0.0 04NOV2026
#R version: 4.4.3

#' Download and process a REDCap audit trail (logging)
#'
#' Downloads the whole REDCap log for a project through the API and splits
#' every data entry into one row per field, showing the value before and after.
#'
#' How it works:
#' \enumerate{
#'   \item Downloads the log from the start of the project (so every value's
#'     history is complete) up to \code{end_time}.
#'   \item Reads each data entry: record, event, repeat instance, and each
#'     field and value. Checkbox options are kept separate (\code{field} plus
#'     \code{option}).
#'   \item Puts every value for the same record, event, instance and field in
#'     time order, from every source (staff, survey, auto calculation, API,
#'     Data Quality), so \code{old_value} is always the value just before.
#' }
#'
#' @param token REDCap API token with logging rights. Read it with
#'   \code{keyring::key_get()}.
#' @param study_name Study name. Only used in the file name when
#'   \code{write_file = TRUE}.
#' @param environment \code{"live"} or \code{"test"}.
#' @param end_time Optional. Only include log entries up to this time,
#'   \code{"YYYY-MM-DD HH:MM"}. Blank (default) = up to now.
#' @param write_file \code{FALSE} (default) or \code{TRUE} to also save a CSV.
#' @param output_dir Folder for the CSV (created if needed; must not be inside
#'   a Git repository). Defaults to \code{Documents/REDCap_audit_trails/<study_name>}.
#'
#' @return A data frame with one row per log entry that is not a data entry
#'   (exports, design changes, etc., copied as they are) and one row per field
#'   for each data entry. Columns:
#'   \describe{
#'     \item{project}{Project title from REDCap.}
#'     \item{timestamp}{Date-time of the log entry (as shown in REDCap).}
#'     \item{username}{Who made the entry (\code{[survey respondent]} for surveys).}
#'     \item{action}{Type of entry: \code{"Update record"} (staff edit on a
#'       form), \code{"Create record"}, \code{"Update record (Auto calculation)"},
#'       \code{"Update record (API)"}, \code{"Update record (Data Quality)"},
#'       \code{"Create response"}/\code{"Update response"} (survey),
#'       \code{"Assign record to Data Access Group"}; other entries keep
#'       REDCap's text.}
#'     \item{log_action}{REDCap's original action text.}
#'     \item{record}{Record ID (text, so leading zeros are kept).}
#'     \item{event}{Event label as shown in the log (blank if not longitudinal).}
#'     \item{instance}{Repeat instance (1 if not repeating).}
#'     \item{field}{Field name.}
#'     \item{option}{Checkbox option code (blank for other fields).}
#'     \item{old_value, new_value}{Value before and after. Checkboxes are
#'       \code{"checked"}/\code{"unchecked"}. \code{old_value} is blank (NA)
#'       for a first value.}
#'     \item{old_timestamp, new_timestamp}{When the old and new values were entered.}
#'     \item{change_type}{\code{"First value"}, \code{"Changed"} (includes
#'       clearing a value) or \code{"Unchanged"} (same value saved again).}
#'     \item{reason}{Reason for change for this field. When REDCap's reason is in the
#'       one-line-per-field format written by PenCTU_ReasonForChange.js
#'       ("field_name: reason"), this is the field's own reason, without the
#'       "field_name: " part (blank if the field is not listed). Otherwise it is
#'       the reason as REDCap saved it.}
#'     \item{log_reason}{The reason exactly as REDCap saved it.}
#'     \item{details}{REDCap's original list of data changes.}
#'   }
#'
#' @details A "Delete record" entry starts the record's history again, so a
#'   re-created record is not compared with the deleted one. REDCap's internal
#'   \code{__GROUPID__} entries are left out. The REDCap log only records times
#'   to the minute, so entries in the same minute are kept in REDCap's order.
#'
#' @examples
#' \dontrun{
#' atr_data <- get_audit_trail(keyring::key_get("STUDY_TOKEN"), "STUDY", "live")
#' }
#' @param script Optional. Name and version of the script that ran this, e.g.
#'   "TEMPLATE_ATR V2.0.0". It is added, with this function's name and version,
#'   in the generated_by column.
#'
#' @export
get_audit_trail <- function(token, study_name, environment, end_time = "",
                            write_file = FALSE,
                            output_dir = file.path(path.expand("~"), "REDCap_audit_trails", study_name),
                            script = NULL) {

  # ---- Name and version of this function (and the script that ran it), added to the output ----
  version <- "3.0.0"
  if (!is.null(script) && (length(script) != 1 || is.na(script) || !is.character(script))) {
    stop("script must be a single piece of text, e.g. \"TEMPLATE_X V2.1.0\", or left out.")
  }
  generated_by <- paste0("get_audit_trail V", version,
                         if (!is.null(script) && trimws(script) != "") paste0(" (", trimws(script), ")") else "")

  urls <- c(live = "https://clinicaltrials.plymouth.ac.uk/api/",
            test = "https://clinicaltrials-pre.plymouth.ac.uk/api/")
  blank <- function(x) is.na(x) | trimws(as.character(x)) == ""

  # V3.0.0: split a reason written one line per field ("field_name: reason", as saved by
  # PenCTU_ReasonForChange.js) into each field's own reason. `fields` = the fields in that log entry.
  # Returns NULL (keep the reason as it is) unless the reason starts with "name: " and at least one of the
  # names is a field in the entry. A line starts at the beginning, after a line break (or <br>), or - if
  # REDCap has joined the lines with spaces - at " field_name: " for a field in the entry.
  split_reason <- function(reason, fields) {
    if (length(reason) != 1 || blank(reason)) return(NULL)
    txt <- gsub("<br\\s*/?>", "\n", reason, ignore.case = TRUE, perl = TRUE)
    if (!grepl("^\\s*[a-z][a-z0-9_]*: ", txt, perl = TRUE)) return(NULL)
    f <- fields[grepl("^[a-z][a-z0-9_]*$", fields)]
    f <- f[order(-nchar(f))]
    starts <- paste0("(?:^\\s*|[\\r\\n]+\\s*)([a-z][a-z0-9_]*): ",
                     if (length(f)) paste0("| (", paste(f, collapse = "|"), "): ") else "")
    m <- gregexpr(starts, txt, perl = TRUE)[[1]]
    cs <- attr(m, "capture.start"); cl <- attr(m, "capture.length")
    nm <- vapply(seq_along(m), function(i) {
      j <- which(cs[i, ] > 0)[1]; substr(txt, cs[i, j], cs[i, j] + cl[i, j] - 1)
    }, "")
    if (!any(nm %in% fields)) return(NULL)
    ends <- c(as.integer(m)[-1] - 1L, nchar(txt))
    val <- trimws(substring(txt, as.integer(m) + attr(m, "match.length"), ends))
    val[val == ""] <- NA_character_
    out <- stats::setNames(val, nm)
    out[!duplicated(names(out))]
  }

  # ---- 1. Check inputs ----
  env <- tolower(trimws(as.character(environment)))
  if (length(env) != 1 || !env %in% names(urls)) {
    stop("environment must be \"live\" or \"test\" (you gave \"", environment, "\").")
  }
  url <- urls[[env]]
  if (length(token) != 1 || blank(token)) stop("token is empty. Read it with keyring::key_get().")
  end_time <- trimws(as.character(end_time))
  if (length(end_time) != 1 || (end_time != "" && !grepl("^\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}$", end_time))) {
    stop("end_time must be blank or \"YYYY-MM-DD HH:MM\".")
  }
  if (isTRUE(write_file) && (length(study_name) != 1 || blank(study_name) || grepl("[^A-Za-z0-9_-]", study_name))) {
    stop("study_name must contain only letters, numbers, _ and -.")
  }

  # ---- 2. Download from REDCap (stops with a clear message if it fails) ----
  call_api <- function(body, what) {
    r <- tryCatch(httr::POST(url, body = c(list(token = token), body), encode = "form"),
                  error = function(e) stop("Could not connect to REDCap to get the ", what, ": ",
                                           gsub(token, "[token]", conditionMessage(e), fixed = TRUE), call. = FALSE))
    txt <- rawToChar(httr::content(r, as = "raw"))
    Encoding(txt) <- "UTF-8"
    if (httr::status_code(r) != 200) {
      stop("REDCap refused the request for the ", what, " (HTTP ", httr::status_code(r), "): ",
           gsub(token, "[token]", substr(txt, 1, 200), fixed = TRUE),
           ". Check the token, environment and that you have logging/API rights.", call. = FALSE)
    }
    txt
  }
  read_csv_text <- function(txt) {
    if (trimws(txt) == "") return(data.frame())
    utils::read.csv(text = txt, colClasses = "character", check.names = FALSE,
                    na.strings = character(0), encoding = "UTF-8")
  }

  project <- read_csv_text(call_api(list(content = "project", format = "csv", returnFormat = "json"), "project details"))
  project_title <- if ("project_title" %in% names(project)) project$project_title[1] else NA_character_

  log <- read_csv_text(call_api(list(content = "log", logtype = "", user = "", record = "",
                                     beginTime = "", endTime = end_time,
                                     format = "csv", returnFormat = "json"), "log"))
  needed <- c("timestamp", "username", "action", "details")
  missing_cols <- setdiff(needed, names(log))
  if (nrow(log) > 0 && length(missing_cols) > 0) {
    stop("The REDCap log is missing column(s): ", paste(missing_cols, collapse = ", "),
         ". REDCap's log format may have changed - the function needs updating.", call. = FALSE)
  }
  for (col in c(needed, "record", "reason")) if (!col %in% names(log)) log[[col]] <- character(nrow(log))

  out_cols <- c("project", "timestamp", "username", "action", "log_action", "record", "event", "instance",
                "field", "option", "old_value", "new_value", "old_timestamp", "new_timestamp",
                "change_type", "reason", "log_reason", "details", "generated_by")
  if (nrow(log) == 0) {
    message("The REDCap log is empty.")
    empty <- as.data.frame(stats::setNames(replicate(length(out_cols), character(0), simplify = FALSE), out_cols))
    return(empty)
  }

  # ---- 3. Put the log in time order ----
  log$timestamp_dt <- as.POSIXct(log$timestamp, tz = "UTC", tryFormats = c("%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M"))
  # REDCap gives the newest entry first. Number entries oldest = 1 so entries in the same minute keep their order.
  n <- nrow(log)
  log$seq <- if (all(diff(as.numeric(log$timestamp_dt)) <= 0, na.rm = TRUE)) rev(seq_len(n)) else seq_len(n)

  # ---- 4. Read each data entry ----
  # Action text: "<Create|Update> <record|Response>[ (sub-type)] <record ID>[ (event label)]"
  # The event label can itself contain brackets, e.g. "Event 1 Arm 2 (Arm 2: Arm 2)".
  action_re <- "^(Create|Update) (record|Response)(?: \\(([^)]*)\\))? (\\S+)(?: \\((.*)\\))?\\s*$"
  is_data <- grepl(action_re, log$action, perl = TRUE)
  is_delete <- grepl("^Delete record(?: \\([^)]*\\))? \\S+", log$action, perl = TRUE)
  dag_re <- "^Assign record to Data Access Group \\((?:\\[instance = (\\d+)\\], )?(redcap_data_access_group) = '(.*)'\\)$"

  # One item = "field = 'value'" or "field(option) = checked/unchecked".
  # A quoted value ends at a quote that is followed by ", next_field =" or the end, so commas and
  # apostrophes inside values are kept.
  item_re <- paste0("([A-Za-z0-9_]+)(?:\\(([^)]*)\\))? = (?:'([\\s\\S]*?)'|(checked|unchecked))",
                    "(?=, [A-Za-z0-9_]+(?:\\([^)]*\\))? = |\\s*$)")

  parse_entry <- function(i) {
    a <- regmatches(log$action[i], regexec(action_re, log$action[i], perl = TRUE))[[1]]
    details <- log$details[i]
    action_label <- paste0(a[2], " ", if (a[3] == "Response") "response" else "record",
                           if (a[4] != "") paste0(" (", a[4], ")") else "")
    instance <- 1L
    dag <- regmatches(details, regexec(dag_re, details, perl = TRUE))[[1]]
    if (length(dag) > 0) {
      if (dag[2] != "") instance <- as.integer(dag[2])
      items <- data.frame(field = dag[3], option = "", value = dag[4], stringsAsFactors = FALSE)
      action_label <- "Assign record to Data Access Group"
    } else {
      m <- regmatches(details, regexec("^\\[instance = (\\d+)\\](?:,\\s*)?", details))[[1]]
      if (length(m) > 0) {
        instance <- as.integer(m[2])
        details <- substring(details, nchar(m[1]) + 1)
      }
      found <- regmatches(details, gregexpr(item_re, details, perl = TRUE))[[1]]
      if (length(found) == 0) return(NULL)
      parts <- regmatches(found, regexec(item_re, found, perl = TRUE))
      items <- data.frame(field  = vapply(parts, `[`, "", 2),
                          option = vapply(parts, `[`, "", 3),
                          value  = vapply(parts, function(p) if (p[5] != "") p[5] else p[4], ""),
                          stringsAsFactors = FALSE)
      items <- items[items$field != "__GROUPID__", , drop = FALSE]   # REDCap internal, not a field
      if (nrow(items) == 0) return(NULL)
    }
    data.frame(row = i, action = action_label, record = a[5], event = a[6], instance = instance,
               items, item_order = seq_len(nrow(items)), stringsAsFactors = FALSE)
  }
  changes <- do.call(rbind, lapply(which(is_data), parse_entry))

  # ---- 5. Value history: old value = the value just before, from any source ----
  if (!is.null(changes) && nrow(changes) > 0) {
    changes$seq <- log$seq[changes$row]
    changes$timestamp_dt <- log$timestamp_dt[changes$row]
    # "Delete record" starts the record's history again
    del <- data.frame(record = sub("^Delete record(?: \\([^)]*\\))? (\\S+).*$", "\\1", log$action[is_delete], perl = TRUE),
                      seq = log$seq[is_delete], stringsAsFactors = FALSE)
    changes$deleted_before <- vapply(seq_len(nrow(changes)), function(k)
      sum(del$record == changes$record[k] & del$seq < changes$seq[k]), integer(1))

    # A Data Access Group belongs to the whole record, so its history ignores the event and instance
    # REDCap sometimes writes next to it
    is_dag <- changes$field == "redcap_data_access_group"
    h_event <- ifelse(is_dag, "", changes$event)
    h_instance <- ifelse(is_dag, 1L, changes$instance)
    changes <- changes[order(changes$record, h_event, h_instance, changes$field, changes$option,
                             changes$deleted_before, changes$seq, changes$item_order), , drop = FALSE]
    is_dag <- changes$field == "redcap_data_access_group"
    key <- paste(changes$record, ifelse(is_dag, "", changes$event), ifelse(is_dag, 1L, changes$instance),
                 changes$field, changes$option, changes$deleted_before, sep = "\r")
    first <- c(TRUE, key[-1] != key[-length(key)])
    changes$old_value <- c(NA, changes$value[-nrow(changes)]); changes$old_value[first] <- NA
    changes$old_timestamp <- c(changes$timestamp_dt[1], changes$timestamp_dt[-nrow(changes)])
    changes$old_timestamp[first] <- NA
    changes$change_type <- ifelse(first, "First value",
                                  ifelse(changes$old_value == changes$value, "Unchanged", "Changed"))
    l <- log[changes$row, , drop = FALSE]
    data_rows <- data.frame(seq = changes$seq, item_order = changes$item_order, project = project_title,
                            timestamp = l$timestamp_dt, username = l$username, action = changes$action,
                            log_action = l$action, record = changes$record, event = changes$event,
                            instance = changes$instance, field = changes$field, option = changes$option,
                            old_value = changes$old_value, new_value = changes$value,
                            old_timestamp = changes$old_timestamp, new_timestamp = l$timestamp_dt,
                            change_type = changes$change_type, reason = l$reason, log_reason = l$reason,
                            details = l$details, stringsAsFactors = FALSE)
    # V3.0.0: reasons written one line per field ("field_name: reason") are split, so each field's row
    # has its own reason
    for (r in unique(changes$row)) {
      k <- which(changes$row == r)
      own <- split_reason(log$reason[r], unique(changes$field[k]))
      if (!is.null(own)) data_rows$reason[k] <- unname(own[changes$field[k]])
    }
  } else {
    data_rows <- NULL
  }

  # ---- 6. Other log entries are kept as they are ----
  other <- log[!is_data, , drop = FALSE]
  no <- nrow(other)   # V3.0.0: rep() so a log with only data entries works (V2.1.0 stopped with an error)
  other_rows <- data.frame(seq = other$seq, item_order = rep(0L, no), project = rep(project_title, no),
                           timestamp = other$timestamp_dt, username = other$username, action = other$action,
                           log_action = other$action, record = other$record, event = rep(NA_character_, no),
                           instance = rep(NA_integer_, no), field = rep(NA_character_, no), option = rep(NA_character_, no),
                           old_value = rep(NA_character_, no), new_value = rep(NA_character_, no),
                           old_timestamp = rep(as.POSIXct(NA, tz = "UTC"), no), new_timestamp = rep(as.POSIXct(NA, tz = "UTC"), no),
                           change_type = rep(NA_character_, no), reason = other$reason, log_reason = other$reason,
                           details = other$details, stringsAsFactors = FALSE)
  # entries REDCap logs as data entries but with nothing to read (e.g. only "[instance = 2]") are kept too
  unread <- setdiff(which(is_data), if (is.null(data_rows)) integer(0) else unique(changes$row))
  if (length(unread) > 0) {
    u <- log[unread, , drop = FALSE]
    other_rows <- rbind(other_rows, data.frame(
      seq = u$seq, item_order = 0L, project = project_title, timestamp = u$timestamp_dt, username = u$username,
      action = u$action, log_action = u$action, record = u$record, event = NA_character_, instance = NA_integer_,
      field = NA_character_, option = NA_character_, old_value = NA_character_, new_value = NA_character_,
      old_timestamp = as.POSIXct(NA, tz = "UTC"), new_timestamp = as.POSIXct(NA, tz = "UTC"),
      change_type = NA_character_, reason = u$reason, log_reason = u$reason, details = u$details, stringsAsFactors = FALSE))
  }

  audit_data <- rbind(other_rows, data_rows)
  audit_data <- audit_data[order(-audit_data$seq, audit_data$item_order), , drop = FALSE]   # newest first, like REDCap
  audit_data$event[!is.na(audit_data$event) & audit_data$event == ""] <- NA
  audit_data$option[!is.na(audit_data$option) & audit_data$option == ""] <- NA
  audit_data$reason[!is.na(audit_data$reason) & trimws(audit_data$reason) == ""] <- NA
  audit_data$log_reason[!is.na(audit_data$log_reason) & trimws(audit_data$log_reason) == ""] <- NA
  audit_data$generated_by <- rep(generated_by, nrow(audit_data))
  audit_data <- audit_data[, out_cols]
  rownames(audit_data) <- NULL

  # ---- 7. Optional: save a CSV outside any repository ----
  if (isTRUE(write_file)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    if (!dir.exists(output_dir)) stop("Could not create output_dir: ", output_dir)
    out_abs <- normalizePath(output_dir, winslash = "/")
    d <- out_abs
    repeat {
      if (file.exists(file.path(d, ".git"))) {
        stop("output_dir is inside a Git repository (", d, "). Save audit trails outside any repository.")
      }
      parent <- dirname(d)
      if (parent == d) break
      d <- parent
    }
    f <- file.path(out_abs, paste0(study_name, "_AuditTrail_", format(Sys.time(), "%Y-%m-%d_%H%M"), ".csv"))
    utils::write.csv(audit_data, f, row.names = FALSE, na = "", fileEncoding = "UTF-8")
    message("Audit trail saved: ", f)
  }

  audit_data
}

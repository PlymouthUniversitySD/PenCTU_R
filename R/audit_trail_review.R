#Title: Audit trail review
#Author: Paigan Aspinall
#Version & Date: V1.0.0 04NOV2026
#R version: 4.4.3

#' Unhelpful reasons for change
#'
#' The list of reasons for change that do not say why a value was changed. A
#' reason is unhelpful if it is blank, one character, or matches the list after
#' both are made lower case and punctuation, symbols and spaces are removed (so
#' "Corrected." matches "corrected", but "Corrected - weight was in lbs" does
#' not).
#'
#' @param add Optional extra reasons for the study, e.g. \code{c("as per query")}.
#' @return A character vector of reasons (default list plus \code{add}).
#' @examples
#' atr_unhelpful_reasons()
#' atr_unhelpful_reasons(add = c("as per query", "site request"))
#' @export
atr_unhelpful_reasons <- function(add = NULL) {
  default <- c(
    # blank or near blank (blank and one-character reasons are always unhelpful)
    "na", "n/a", "none", "nil", "null", "no reason", "unknown",
    # changed
    "change", "changed", "changes", "change made", "changes made", "changed data", "data changed",
    "data change", "value changed", "changed value", "changed entry",
    # corrected
    "correct", "corrected", "correction", "corrections", "corrected data", "data corrected",
    "corrected value", "corrected entry", "corrected error", "error corrected", "error", "errors",
    "mistake", "wrong", "incorrect", "wrong entry", "wrong value", "incorrect entry", "incorrect value",
    "entered incorrectly", "entered wrong",
    # updated
    "update", "updated", "updates", "update data", "updated data", "data updated", "updated value",
    "updated entry", "updated info", "updated information", "amend", "amended", "amendment", "edit",
    "edited", "edits", "modified", "modify", "revised", "fix", "fixed", "adjusted",
    # no information
    "ok", "okay", "yes", "no", "done", "test", "testing", "same", "ditto", "as above", "see above",
    "see notes", "see comment", "other", "misc", "admin", "data entry", "entry", "re-entered",
    "reentered", "re-entry", "input", "inputted", "entered"
  )
  if (!is.null(add)) {
    if (!is.character(add)) stop("add must be text, e.g. c(\"as per query\").")
    add <- add[!is.na(add) & trimws(add) != ""]
  }
  unique(c(default, add))
}

#' Audit trail review
#'
#' Runs the PenCTU audit trail review checks on an audit trail from
#' \code{get_audit_trail} (V2.1.0 or later) and saves one Excel workbook.
#'
#' A change is a staff edit on a form (action "Update record") where the value
#' changed (\code{change_type} "Changed", which includes clearing a value). API,
#' data import, Data Quality, auto calculation and survey entries are not
#' changes. Fields annotated #DataManagement, #SystemFunctionality or
#' #TrialManagement, form status fields (ending "_complete") and the DAG are left
#' out of every check except Critical Data Items, which counts every change to
#' the fields in \code{critical_data_items}.
#'
#' Each record's site is its latest DAG in the audit trail; "Unknown" if it has
#' none; "All participants" if the audit trail has no DAGs at all.
#'
#' Sheets: Cover, Site Summary, Critical Data Items, Changes per User, Unstable
#' Fields, Time to Change, Out of Hours Changes, Reasons for Change, Deletions.
#'
#' @param atr_data Audit trail from \code{get_audit_trail} (several projects can be
#'   combined with \code{rbind}).
#' @param metadata Data dictionary (REDCap metadata export) with field_name and
#'   field_annotation; several projects can be combined with \code{rbind}.
#' @param critical_data_items Data frame (or path to the CSV) with field_name,
#'   critical_data_item and critical_data_item_type; NULL = not used.
#' @param study_name Study name for the file name (letters, numbers, _ and -).
#' @param document_id Optional document ID shown on the cover sheet.
#' @param output_dir Folder to save in (created if needed; not inside a Git repository).
#' @param working_hours Start and end of working hours, \code{c("08:00", "18:00")}.
#'   A change at or after the end time is out of hours.
#' @param weekends_out_of_hours TRUE (default): every change on a Saturday or Sunday is out of hours.
#' @param days_threshold Changes made more than this many days after the value was
#'   entered are listed on Time to Change (default 1).
#' @param unhelpful_reasons Reasons for change that are unhelpful; default
#'   \code{atr_unhelpful_reasons()}.
#' @param script Name and version of the script that ran this, for the cover sheet.
#' @param export_time Optional date-time the audit trail was exported, for the cover sheet.
#' @return Invisibly, a list with one data frame per sheet and \code{file}.
#' @export
audit_trail_review <- function(atr_data, metadata, critical_data_items = NULL, study_name, document_id = NULL,
                               output_dir = file.path(path.expand("~"), "REDCap_audit_trails", study_name),
                               working_hours = c("08:00", "18:00"), weekends_out_of_hours = TRUE,
                               days_threshold = 1, unhelpful_reasons = atr_unhelpful_reasons(),
                               script = NULL, export_time = NULL) {

  version <- "1.0.0"
  blank <- function(x) is.na(x) | trimws(as.character(x)) == ""
  one_text <- function(x) length(x) == 1 && is.character(x) && !is.na(x)

  # ---- 1. Check inputs (nothing is saved before this passes) ----
  if (!one_text(study_name) || !grepl("^[A-Za-z0-9_-]+$", study_name)) {
    stop("study_name must contain only letters, numbers, _ and -.")
  }
  if (!is.null(script) && !one_text(script)) stop("script must be a single piece of text, e.g. \"TEMPLATE_ATR V2.0.0\", or left out.")
  if (!is.null(document_id) && !one_text(document_id)) stop("document_id must be a single piece of text, or left out.")
  hm <- function(x) {
    ok <- grepl("^([01][0-9]|2[0-3]):[0-5][0-9]$", x)
    ifelse(ok, as.integer(substr(x, 1, 2)) * 60L + as.integer(substr(x, 4, 5)), NA_integer_)
  }
  if (!is.character(working_hours) || length(working_hours) != 2 || any(is.na(hm(working_hours))) ||
      hm(working_hours[1]) >= hm(working_hours[2])) {
    stop("working_hours must be a start and end time, e.g. c(\"08:00\", \"18:00\"), with the start before the end.")
  }
  if (!is.logical(weekends_out_of_hours) || length(weekends_out_of_hours) != 1 || is.na(weekends_out_of_hours)) {
    stop("weekends_out_of_hours must be TRUE or FALSE.")
  }
  if (!is.numeric(days_threshold) || length(days_threshold) != 1 || is.na(days_threshold) || days_threshold < 0) {
    stop("days_threshold must be a number of days, 0 or more.")
  }
  if (!is.character(unhelpful_reasons)) stop("unhelpful_reasons must be text, e.g. atr_unhelpful_reasons(add = \"as per query\").")

  atr_need <- c("project", "timestamp", "username", "action", "record", "event", "instance", "field", "option",
                "old_value", "new_value", "old_timestamp", "new_timestamp", "change_type", "reason")
  if (!is.data.frame(atr_data) || !all(atr_need %in% names(atr_data))) {
    stop("atr_data is missing column(s): ", paste(setdiff(atr_need, names(atr_data)), collapse = ", "),
         ". Use the output of get_audit_trail V2.1.0 or later.")
  }
  metadata <- as.data.frame(metadata, stringsAsFactors = FALSE)
  if (!all(c("field_name", "field_annotation") %in% names(metadata))) {
    stop("metadata must be the REDCap data dictionary export, with field_name and field_annotation.")
  }
  crit_source <- "Not used"
  if (!is.null(critical_data_items)) {
    if (is.character(critical_data_items) && length(critical_data_items) == 1) {
      if (!file.exists(critical_data_items)) stop("critical_data_items file not found: ", critical_data_items)
      crit_source <- basename(critical_data_items)
      critical_data_items <- utils::read.csv(critical_data_items, colClasses = "character", check.names = FALSE)
    } else if (is.data.frame(critical_data_items)) {
      crit_source <- "data frame"
    } else stop("critical_data_items must be a data frame, the path to a CSV file, or NULL.")
    crit_need <- c("field_name", "critical_data_item", "critical_data_item_type")
    if (!all(crit_need %in% names(critical_data_items))) {
      stop("critical_data_items is missing column(s): ", paste(setdiff(crit_need, names(critical_data_items)), collapse = ", "))
    }
    critical_data_items <- critical_data_items[!blank(critical_data_items$field_name), crit_need, drop = FALSE]
    critical_data_items[] <- lapply(critical_data_items, function(x) trimws(as.character(x)))
    bad <- setdiff(critical_data_items$field_name, metadata$field_name)
    if (length(bad) > 0) stop("These critical data item fields are not in the data dictionary: ", paste(bad, collapse = ", "))
  }

  # ---- 2. Prepare the audit trail ----
  a <- as.data.frame(atr_data, stringsAsFactors = FALSE)
  for (col in c("project", "username", "action", "record", "event", "field", "option", "old_value", "new_value",
                "change_type", "reason")) a[[col]] <- as.character(a[[col]])
  to_time <- function(x) if (inherits(x, "POSIXct")) x else
    as.POSIXct(as.character(x), tz = "UTC", tryFormats = c("%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M"), optional = TRUE)
  for (col in c("timestamp", "old_timestamp", "new_timestamp")) a[[col]] <- to_time(a[[col]])
  # a deleted record's ID is in the action text ("Delete record [(API)] <record>") if the record column is blank
  dr <- grepl("^Delete record", a$action) & blank(a$record)
  a$record[dr] <- sub("^Delete record(?: \\([^)]*\\))? (\\S+).*$", "\\1", a$action[dr], perl = TRUE)
  fmt_time <- function(x) format(x, "%Y-%m-%d %H:%M", tz = "UTC")   # REDCap's times, as shown in the log
  multi_project <- length(unique(a$project[!is.na(a$project)])) > 1
  rec_key <- paste(a$project, a$record, sep = "\r")

  # Site: each record's latest DAG ("Unknown" if none; "All participants" if no DAGs at all)
  dag_rows <- which(a$action == "Assign record to Data Access Group" & a$field %in% "redcap_data_access_group")
  has_dags <- length(dag_rows) > 0
  site_of <- if (has_dags) {
    d <- a[dag_rows, , drop = FALSE]
    d <- d[order(d$timestamp, decreasing = TRUE), , drop = FALSE]   # keeps REDCap's order within a minute (newest first)
    k <- paste(d$project, d$record, sep = "\r")
    latest <- d$new_value[!duplicated(k)]; names(latest) <- k[!duplicated(k)]
    function(key) { s <- unname(latest[key]); ifelse(is.na(s) | trimws(s) == "", "Unknown", s) }
  } else function(key) rep("All participants", length(key))
  a$site <- site_of(rec_key)

  # What counts as a change
  excluded_fields <- metadata$field_name[grepl("#DataManagement|#SystemFunctionality|#TrialManagement",
                                               metadata$field_annotation)]
  is_change <- a$action %in% "Update record" & a$change_type %in% "Changed" & !a$username %in% c("SYSTEM", "[survey respondent]")
  is_reviewed <- is_change & !a$field %in% excluded_fields & !grepl("_complete$", a$field) &
    !a$field %in% "redcap_data_access_group"
  ch <- a[is_reviewed, , drop = FALSE]
  field_label <- function(d) ifelse(blank(d$option), d$field, paste0(d$field, "(", d$option, ")"))
  where <- function(d) ifelse(blank(d$event), d$record, paste0(d$record, " (", d$event, ")"))
  norm <- function(x) {
    x <- tolower(ifelse(is.na(x), "", x))
    gsub("[\\p{P}\\p{S}\\s]", "", x, perl = TRUE)   # ignore case, punctuation, symbols and spaces
  }
  reason_list <- unique(norm(unhelpful_reasons))
  unhelpful <- function(r) { n <- norm(r); nchar(n) <= 1 | n %in% reason_list }
  ch$unhelpful <- unhelpful(ch$reason)
  drop_project <- function(d) { if (!multi_project) d$project <- NULL; d }
  # Sites in a fixed order (not R's language setting): DAGs A-Z, then Unknown / All participants
  sort_sites <- function(x) { x <- unique(x); c(sort(setdiff(x, c("Unknown", "All participants")), method = "radix"),
                                                intersect(c("Unknown", "All participants"), x)) }

  # ---- 3. Critical Data Items ----
  critical <- NULL
  if (!is.null(critical_data_items)) {
    cc <- a[is_change & a$field %in% critical_data_items$field_name, , drop = FALSE]
    cc$where <- where(cc)
    rows <- list()
    for (i in seq_len(nrow(critical_data_items))) {
      ci <- critical_data_items[i, ]
      x <- cc[cc$field == ci$field_name, , drop = FALSE]
      if (nrow(x) == 0) next
      wk <- paste(x$project, x$where, sep = "\r")
      tab <- table(factor(wk, levels = sort(unique(wk), method = "radix")))
      lab <- sub("^.*\r", "", names(tab))
      rows[[length(rows) + 1]] <- data.frame(
        critical_data_item_type = ci$critical_data_item_type, critical_data_item = ci$critical_data_item,
        field = ci$field_name, n_changes = nrow(x), n_affected_records = length(tab),
        affected_records = paste(ifelse(tab > 1, paste0(lab, " [", tab, " changes]"), lab), collapse = "; "),
        stringsAsFactors = FALSE)
    }
    critical <- if (length(rows)) do.call(rbind, rows) else
      data.frame(critical_data_item_type = character(), critical_data_item = character(), field = character(),
                 n_changes = integer(), n_affected_records = integer(), affected_records = character())
    critical <- critical[order(critical$critical_data_item_type, -critical$n_changes, critical$field, method = "radix"), , drop = FALSE]
    rownames(critical) <- NULL
  }

  # ---- 4. Changes per User (every month from the first to the last month in the log) ----
  all_times <- a$timestamp[!is.na(a$timestamp)]
  months <- if (length(all_times)) {
    first <- as.Date(format(min(all_times), "%Y-%m-01", tz = "UTC"))
    last <- as.Date(format(max(all_times), "%Y-%m-01", tz = "UTC"))
    format(seq(first, last, by = "month"), "%Y-%m")
  } else character(0)
  users <- sort(unique(ch$username), method = "radix")
  per_user <- data.frame(username = users, stringsAsFactors = FALSE)
  per_user$sites <- vapply(users, function(u) paste(sort_sites(ch$site[ch$username == u]), collapse = "; "), "")
  per_user$n_changes <- vapply(users, function(u) sum(ch$username == u), integer(1))
  per_user$n_unhelpful_reasons <- vapply(users, function(u) sum(ch$username == u & ch$unhelpful), integer(1))
  ch_month <- format(ch$timestamp, "%Y-%m", tz = "UTC")
  for (m in months) per_user[[m]] <- vapply(users, function(u) sum(ch$username == u & ch_month == m), integer(1))
  per_user <- per_user[order(-per_user$n_changes, per_user$username, method = "radix"), , drop = FALSE]
  rownames(per_user) <- NULL

  # ---- 5. Unstable Fields ----
  ch$where <- where(ch)
  entered <- a[!is.na(a$field) & !blank(a$new_value) & a$field %in% unique(ch$field), , drop = FALSE]
  entered_key <- unique(paste(entered$project, entered$field, entered$record, entered$event, sep = "\r"))
  uf <- unique(ch[, c("project", "field")])
  unstable <- do.call(rbind, c(list(data.frame(project = character(), field = character(), n_changes = integer(),
                                              n_affected_records = integer(), pct_records_changed = numeric(),
                                              affected_records = character())),
    lapply(seq_len(nrow(uf)), function(i) {
      x <- ch[ch$project %in% uf$project[i] & ch$field == uf$field[i], , drop = FALSE]
      tab <- table(factor(x$where, levels = sort(unique(x$where), method = "radix")))
      n_rec <- sum(startsWith(entered_key, paste(uf$project[i], uf$field[i], "", sep = "\r")))
      data.frame(project = uf$project[i], field = uf$field[i], n_changes = nrow(x), n_affected_records = length(tab),
                 pct_records_changed = if (n_rec > 0) round(100 * length(tab) / n_rec, 1) else NA_real_,
                 affected_records = paste(ifelse(tab > 1, paste0(names(tab), " [", tab, " changes]"), names(tab)),
                                          collapse = "; "), stringsAsFactors = FALSE)
    })))
  unstable <- drop_project(unstable[order(-unstable$n_changes, unstable$field, method = "radix"), , drop = FALSE])
  rownames(unstable) <- NULL

  # ---- 6. Time to Change ----
  ch$days <- as.numeric(difftime(ch$new_timestamp, ch$old_timestamp, units = "days"))
  tc <- ch[!is.na(ch$days) & ch$days > days_threshold, , drop = FALSE]
  time_to_change <- drop_project(data.frame(project = tc$project, site = tc$site, record = tc$record, event = tc$event,
                                            instance = tc$instance, field = field_label(tc), old_value = tc$old_value,
                                            new_value = tc$new_value, reason = tc$reason,
                                            days_between_changes = round(tc$days, 1), stringsAsFactors = FALSE))
  time_to_change <- time_to_change[order(-time_to_change$days_between_changes, time_to_change$record, method = "radix"), , drop = FALSE]
  rownames(time_to_change) <- NULL

  # ---- 7. Out of Hours Changes (day from the date, not R's language setting) ----
  day_num <- as.integer(format(ch$timestamp, "%u", tz = "UTC"))
  mins <- as.integer(format(ch$timestamp, "%H", tz = "UTC")) * 60L + as.integer(format(ch$timestamp, "%M", tz = "UTC"))
  start <- hm(working_hours[1]); end <- hm(working_hours[2])
  ooh <- (mins < start | mins >= end) | (weekends_out_of_hours & day_num %in% 6:7)
  ooh[is.na(ooh)] <- FALSE
  days_en <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
  o <- ch[ooh, , drop = FALSE]
  out_of_hours <- drop_project(data.frame(project = o$project, timestamp = fmt_time(o$timestamp),
                                          day = days_en[day_num[ooh]], username = o$username, site = o$site,
                                          record = o$record, event = o$event, instance = o$instance,
                                          field = field_label(o), old_value = o$old_value, new_value = o$new_value,
                                          reason = o$reason, stringsAsFactors = FALSE))
  rownames(out_of_hours) <- NULL

  # ---- 8. Reasons for Change ----
  r <- ch[ch$unhelpful, , drop = FALSE]
  reasons <- drop_project(data.frame(project = r$project, timestamp = fmt_time(r$timestamp), username = r$username,
                                     site = r$site, record = r$record, event = r$event, instance = r$instance,
                                     field = field_label(r), old_value = r$old_value, new_value = r$new_value,
                                     reason = ifelse(is.na(r$reason), "(blank)", r$reason), stringsAsFactors = FALSE))
  rownames(reasons) <- NULL

  # ---- 9. Deletions: records deleted by staff, and values cleared ----
  del <- a[grepl("^Delete record", a$action) & !grepl("\\(API\\)", a$action), , drop = FALSE]
  cleared <- ch[blank(ch$new_value), , drop = FALSE]
  deletions <- drop_project(rbind(
    data.frame(project = del$project, type = rep("Record deleted", nrow(del)), timestamp = fmt_time(del$timestamp),
               username = del$username, site = del$site, record = del$record, event = rep(NA_character_, nrow(del)),
               field = rep(NA_character_, nrow(del)), old_value = rep(NA_character_, nrow(del)), reason = del$reason,
               stringsAsFactors = FALSE),
    data.frame(project = cleared$project, type = rep("Value cleared", nrow(cleared)), timestamp = fmt_time(cleared$timestamp),
               username = cleared$username, site = cleared$site, record = cleared$record, event = cleared$event,
               field = field_label(cleared), old_value = cleared$old_value, reason = cleared$reason,
               stringsAsFactors = FALSE)))
  deletions <- deletions[order(deletions$timestamp, decreasing = TRUE), , drop = FALSE]
  rownames(deletions) <- NULL

  # ---- 10. Site Summary ----
  data_rows <- !is.na(a$field)
  site_order <- sort_sites(a$site[data_rows & !blank(a$record)])
  crit_ch <- if (is.null(critical_data_items)) a[0, ] else a[is_change & a$field %in% critical_data_items$field_name, , drop = FALSE]
  count_site <- function(s) c(
    records = length(unique(rec_key[data_rows & !blank(a$record) & a$site == s])),
    changes = sum(ch$site == s), critical_data_changes = sum(crit_ch$site == s),
    out_of_hours_changes = sum(o$site == s), unhelpful_reasons = sum(r$site == s),
    deletions = sum(del$site == s) + sum(cleared$site == s))
  ss <- do.call(rbind, lapply(site_order, count_site))
  if (is.null(ss)) ss <- matrix(integer(0), ncol = 6, dimnames = list(NULL, names(count_site("x"))))
  site_summary <- data.frame(site = site_order, ss, stringsAsFactors = FALSE, check.names = FALSE)
  total <- data.frame(site = "All sites", t(colSums(ss)), stringsAsFactors = FALSE, check.names = FALSE)
  total$records <- length(unique(rec_key[data_rows & !blank(a$record)]))
  site_summary <- rbind(site_summary, total)
  rownames(site_summary) <- NULL

  # ---- 11. Output folder: created if needed, never inside a Git repository ----
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(output_dir)) stop("Could not create output_dir: ", output_dir)
  out_abs <- normalizePath(output_dir, winslash = "/")
  d <- out_abs
  repeat {
    if (file.exists(file.path(d, ".git"))) stop("output_dir is inside a Git repository (", d, "). Save reports outside any repository.")
    p <- dirname(d); if (p == d) break; d <- p
  }

  # ---- 12. Workbook ----
  wb <- openxlsx::createWorkbook()
  title <- openxlsx::createStyle(fontSize = 14, textDecoration = "bold", fgFill = "#83CCEB", border = "TopBottomLeftRight")
  head <- openxlsx::createStyle(textDecoration = "bold", fgFill = "#D9D9D9", border = "TopBottomLeftRight", valign = "top", wrapText = TRUE)
  cell <- openxlsx::createStyle(border = "TopBottomLeftRight", valign = "top", wrapText = TRUE)
  example <- openxlsx::createStyle(fontColour = "#FF0000", border = "TopBottomLeftRight", valign = "top", wrapText = TRUE)
  gb <- unique(as.character(atr_data$generated_by[!is.na(atr_data$generated_by)]))
  cover <- data.frame(Item = c(
    "Report", "Document ID", "Study", "Project(s)", "Audit trail covers", "Audit trail exported", "Report saved by",
    "Script", "Function", "Audit trail from", "What counts as a change", "Left out", "Working hours", "Weekends",
    "Time to Change lists changes made more than", "Unhelpful reasons", "Critical data items", "Sites"),
    Value = c(
      "Audit trail review (ATR)", if (is.null(document_id)) "" else document_id, study_name,
      paste(unique(a$project[!is.na(a$project)]), collapse = "; "),
      if (length(all_times)) paste(fmt_time(min(all_times)), "to", fmt_time(max(all_times))) else "(empty)",
      if (is.null(export_time)) "(not given)" else format(export_time, "%d %b %Y %H:%M"),
      Sys.info()[["user"]], if (is.null(script)) "(not given)" else script, paste0("audit_trail_review V", version),
      if (length(gb)) paste(gb, collapse = "; ") else "(not given)",
      "Staff edits on forms where the value changed (including clearing a value). API, data import, Data Quality, auto calculation and survey entries are not counted.",
      "Fields annotated #DataManagement, #SystemFunctionality or #TrialManagement, form status (_complete) fields and the DAG (all sheets except Critical Data Items).",
      paste(working_hours, collapse = " to "),
      if (weekends_out_of_hours) "Out of hours" else "Working days",
      paste(days_threshold, "day(s) after the value was entered"),
      paste0(length(reason_list), " reasons (default list", if (length(setdiff(norm(unhelpful_reasons), norm(atr_unhelpful_reasons()))))
        paste0(" plus: ", paste(setdiff(unhelpful_reasons, atr_unhelpful_reasons()), collapse = ", ")) else "",
        "), plus blank and one-character reasons"),
      if (is.null(critical_data_items)) "Not used" else paste0(crit_source, ": ", nrow(critical_data_items), " field(s)"),
      if (has_dags) "Each record's latest DAG; 'Unknown' = no DAG" else "No DAGs: 'All participants'"),
    stringsAsFactors = FALSE)
  openxlsx::addWorksheet(wb, "Cover")
  openxlsx::writeData(wb, "Cover", paste0(study_name, " audit trail review"), startRow = 1)
  openxlsx::mergeCells(wb, "Cover", cols = 1:3, rows = 1)
  openxlsx::addStyle(wb, "Cover", title, rows = 1, cols = 1:3, gridExpand = TRUE)
  openxlsx::writeData(wb, "Cover", cover, startRow = 3, headerStyle = head)
  openxlsx::addStyle(wb, "Cover", cell, rows = 4:(nrow(cover) + 3), cols = 1:2, gridExpand = TRUE)
  fr <- nrow(cover) + 5
  openxlsx::writeData(wb, "Cover", "Summary of findings and actions from the review:", startRow = fr)
  openxlsx::mergeCells(wb, "Cover", cols = 1:3, rows = fr)
  openxlsx::addStyle(wb, "Cover", head, rows = fr, cols = 1:3, gridExpand = TRUE)
  openxlsx::writeData(wb, "Cover", data.frame(a = "Description of finding:", b = "Corrective action(s) taken:",
                                              c = "Preventative action(s) taken:"), startRow = fr + 1, colNames = FALSE)
  openxlsx::addStyle(wb, "Cover", head, rows = fr + 1, cols = 1:3, gridExpand = TRUE)
  openxlsx::writeData(wb, "Cover", data.frame(
    a = c("e.g. primary outcome field 'field_name' was identified as having been subject to 18 data changes in the past month.",
          "e.g. user 'user_name' was identified as having made 52 changes in the past month, when their typical changes are under 10 per month."),
    b = c("e.g. the reasons for the changes were reviewed and it was identified that a new site had not known how to complete the outcome. The TM and QA manager were notified. Other sites were contacted to check whether the same mistake had been made. Additional training on the outcome was offered to sites.",
          "e.g. the reasons for change were reviewed; many were '-'. The user was contacted and explained they had entered values to 1 decimal place instead of 2. The user was reminded that they must enter a reason for change."),
    c = c("e.g. additional training will be offered to all new sites. A note was added to the form. The outcome will be monitored for upcoming participants.",
          "e.g. a reminder was sent to sites that they must enter a sufficient reason for change, and to tell the data team in advance if many data changes are planned.")),
    startRow = fr + 2, colNames = FALSE)
  openxlsx::addStyle(wb, "Cover", example, rows = (fr + 2):(fr + 3), cols = 1:3, gridExpand = TRUE)
  openxlsx::addStyle(wb, "Cover", cell, rows = (fr + 4):(fr + 8), cols = 1:3, gridExpand = TRUE)
  openxlsx::setColWidths(wb, "Cover", cols = 1:3, widths = c(40, 60, 50))

  scale <- c("#63BE7B", "#FFEB84", "#F8696B")
  sheet <- function(name, df, scale_cols = NULL, note = NULL) {
    openxlsx::addWorksheet(wb, name)
    if (nrow(df) == 0) {
      openxlsx::writeData(wb, name, if (is.null(note)) "None found." else note, startRow = 1)
      openxlsx::writeData(wb, name, df, startRow = 3, headerStyle = head)
      return(invisible())
    }
    openxlsx::writeData(wb, name, df, headerStyle = head)
    openxlsx::freezePane(wb, name, firstRow = TRUE)
    openxlsx::addFilter(wb, name, rows = 1, cols = seq_along(df))
    openxlsx::setColWidths(wb, name, cols = seq_along(df), widths = "auto")
    for (sc in intersect(scale_cols, names(df))) {
      openxlsx::conditionalFormatting(wb, name, cols = which(names(df) == sc), rows = 2:(nrow(df) + 1),
                                      type = "colourScale", style = scale)
    }
  }
  sheet("Site Summary", site_summary)
  if (!is.null(critical)) sheet("Critical Data Items", critical, "n_changes", "No changes to critical data items.")
  sheet("Changes per User", per_user, "n_changes")
  if (length(months) && nrow(per_user) > 0) {
    mc <- which(names(per_user) %in% months)
    for (i in seq_len(nrow(per_user))) {
      openxlsx::conditionalFormatting(wb, "Changes per User", cols = mc, rows = i + 1, type = "colourScale", style = scale)
    }
  }
  sheet("Unstable Fields", unstable, "n_changes")
  sheet("Time to Change", time_to_change, "days_between_changes")
  sheet("Out of Hours Changes", out_of_hours)
  sheet("Reasons for Change", reasons)
  sheet("Deletions", deletions)

  file <- file.path(out_abs, paste0(study_name, "_ATROutput_", format(Sys.time(), "%Y-%m-%d_%H%M"), ".xlsx"))
  openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
  message("Audit trail review saved: ", file)
  invisible(list(site_summary = site_summary, critical_data_items = critical, changes_per_user = per_user,
                 unstable_fields = unstable, time_to_change = time_to_change, out_of_hours = out_of_hours,
                 reasons = reasons, deletions = deletions, file = file))
}

#Title: completeness_tracker_data_prep
#Version & Date: V2.0.0 03NOV2026
#Author: Paigan Aspinall
#R version: 4.4.3

#' Prepare data for the data completeness report
#'
#' Downloads a study's records, data access groups (DAGs), events, forms and
#' repeating set-up from REDCap through the API, and counts the status of every
#' form at every event, by site. The result is passed to
#' \code{write_excel_completeness()}.
#'
#' Rules (see the requirements for the full list):
#' \itemize{
#'   \item For each event, only participants with data at that event are counted.
#'   \item Status: 0 = incomplete, 1 = partially complete (REDCap "Unverified"),
#'     2 = complete; a form with no status counts as incomplete.
#'   \item Every instance of a repeating form or repeating event is counted, and
#'     each form uses its own number of instances.
#'   \item Forms listed in \code{as_needed_forms} (for example an adverse event
#'     log) are only counted where they have been started.
#'   \item If \code{timepoints} is given, a participant's forms at an event (or an
#'     instance of a repeating event) marked as not having taken place are left out.
#'   \item Records with no DAG are counted under "Unknown". A project with no DAGs
#'     is reported as one group, "All participants".
#' }
#'
#' @param token REDCap API token. Read it with keyring::key_get(); never type it.
#' @param environment "live" or "test".
#' @param events Optional unique event names to include (default: all events).
#'   Use this to leave out, for example, an admin arm.
#' @param exclude_forms Optional form names to leave out of the report.
#' @param as_needed_forms Optional form names only counted where started (for
#'   example adverse events, withdrawal, death).
#' @param timepoints Optional timepoints file (a data frame, or the path to the CSV
#'   used by the timepoint report). Uses the columns timepoint_name and
#'   timepoint_occurred_field_name, and baseline_name and
#'   baseline_occurred_field_name if present. A value of 0 ("No") in the occurred
#'   field means the timepoint did not take place.
#'
#' @return A list of class "completeness_data": \code{counts} (one row per event,
#'   form, site and status, with n), \code{participants} (participants counted and
#'   timepoints left out, per event and site), \code{events}, \code{forms},
#'   \code{sites} and \code{settings}.
#' @export
completeness_tracker_data_prep <- function(token, environment, events = NULL, exclude_forms = NULL,
                                           as_needed_forms = NULL, timepoints = NULL) {

  version <- "2.0.0"
  urls <- c(live = "https://clinicaltrials.plymouth.ac.uk/api/",
            test = "https://clinicaltrials-pre.plymouth.ac.uk/api/")
  blank <- function(x) is.na(x) | trimws(as.character(x)) == ""
  statuses <- c("Complete", "Partially complete", "Incomplete")

  # ---- 1. Check inputs (before contacting REDCap) ----
  env <- tolower(trimws(as.character(environment)))
  if (length(env) != 1 || !env %in% names(urls)) {
    stop("environment must be \"live\" or \"test\" (you gave \"", paste(environment, collapse = ", "), "\").")
  }
  url <- urls[[env]]
  if (length(token) != 1 || blank(token)) stop("token is empty. Read it with keyring::key_get().")
  tp_source <- "none"
  if (!is.null(timepoints)) {
    if (is.character(timepoints) && length(timepoints) == 1) {
      if (!file.exists(timepoints)) stop("timepoints file not found: ", timepoints)
      tp_source <- basename(timepoints)
      timepoints <- utils::read.csv(timepoints, colClasses = "character", na.strings = "")
    } else if (is.data.frame(timepoints)) {
      tp_source <- "data frame"
      timepoints[] <- lapply(timepoints, as.character)
    } else {
      stop("timepoints must be a data frame or the path to a CSV file.")
    }
    need <- c("timepoint_name", "timepoint_occurred_field_name")
    if (!all(need %in% names(timepoints))) {
      stop("timepoints must have the columns: ", paste(need, collapse = ", "), ".")
    }
  }

  # ---- 2. Download from REDCap (clear message if it fails; token never shown) ----
  call_api <- function(body, what) {
    r <- tryCatch(httr::POST(url, body = c(list(token = token), body), encode = "form"),
                  error = function(e) stop("Could not connect to REDCap to get the ", what, ": ",
                                           gsub(token, "[token]", conditionMessage(e), fixed = TRUE), call. = FALSE))
    txt <- rawToChar(httr::content(r, as = "raw"))
    Encoding(txt) <- "UTF-8"
    if (httr::status_code(r) != 200) {
      stop("REDCap refused the request for the ", what, " (HTTP ", httr::status_code(r), "): ",
           gsub(token, "[token]", substr(txt, 1, 200), fixed = TRUE),
           ". Check the token, environment and your API export rights.", call. = FALSE)
    }
    txt
  }
  read_csv_text <- function(txt) {
    if (trimws(txt) == "") return(data.frame())
    utils::read.csv(text = txt, colClasses = "character", na.strings = character(0), check.names = FALSE)
  }
  fetch <- function(content, what, ...) read_csv_text(call_api(c(list(content = content, format = "csv"), list(...)), what))

  project <- fetch("project", "project information")
  longitudinal <- nrow(project) > 0 && "is_longitudinal" %in% names(project) && project$is_longitudinal[1] == "1"
  instruments <- fetch("instrument", "list of forms")
  dags <- fetch("dag", "data access groups")
  if (longitudinal) {
    ev <- fetch("event", "list of events")
    mapping <- fetch("formEventMapping", "form-event mapping")
  } else {
    ev <- data.frame(unique_event_name = "(all data)", event_name = "All data", arm_num = "1")
    mapping <- data.frame(unique_event_name = "(all data)", form = instruments$instrument_name)
  }
  export_time <- Sys.time()
  records <- fetch("record", "records", type = "flat", rawOrLabel = "raw", rawOrLabelHeaders = "raw",
                 exportCheckboxLabel = "false", exportSurveyFields = "false", exportDataAccessGroups = "true")

  # ---- 3. Check the optional settings against the project ----
  all_events <- ev$unique_event_name
  if (!is.null(events)) {
    bad <- setdiff(events, all_events)
    if (length(bad) > 0) stop("These events are not in the project: ", paste(bad, collapse = ", "), ".")
  } else {
    events <- all_events
  }
  events <- all_events[all_events %in% events]          # keep REDCap's event order
  form_names <- instruments$instrument_name
  for (arg in c("exclude_forms", "as_needed_forms")) {
    bad <- setdiff(get(arg), form_names)
    if (length(bad) > 0) stop(arg, ": these forms are not in the project: ", paste(bad, collapse = ", "), ".")
  }
  for (col in c("record_id", "redcap_event_name", "redcap_repeat_instrument", "redcap_repeat_instance",
                "redcap_data_access_group")) {
    if (nrow(records) > 0 && !col %in% names(records) && col != "record_id") {
      if (col == "redcap_event_name" && !longitudinal) next
      records[[col]] <- ""
    }
  }
  if (nrow(records) > 0) {
    names(records)[1] <- "record_id"
    if (!longitudinal) records$redcap_event_name <- "(all data)"
  }

  # Occurred field for each event listed in the timepoints file
  occurred <- character(0)
  if (!is.null(timepoints)) {
    tp <- timepoints[!blank(timepoints$timepoint_name), ]
    occurred <- stats::setNames(tp$timepoint_occurred_field_name, tp$timepoint_name)
    if (all(c("baseline_name", "baseline_occurred_field_name") %in% names(tp))) {
      b <- unique(tp[!blank(tp$baseline_name) & !blank(tp$baseline_occurred_field_name),
                     c("baseline_name", "baseline_occurred_field_name")])
      occurred <- c(occurred, stats::setNames(b$baseline_occurred_field_name, b$baseline_name))
    }
    occurred <- occurred[!duplicated(names(occurred)) & !blank(occurred)]
    bad <- setdiff(unique(occurred), names(records))
    if (length(bad) > 0) stop("timepoints: these occurred fields are not in the data: ", paste(bad, collapse = ", "), ".")
    bad <- setdiff(names(occurred), all_events)
    if (length(bad) > 0) stop("timepoints: these events are not in the project: ", paste(bad, collapse = ", "), ".")
  }

  # ---- 4. Sites: DAG names in REDCap's order, plus "Unknown" for records with no DAG.
  #      A project with no DAGs is reported as one group, "All participants". ----
  no_dag <- if (nrow(dags) > 0) "Unknown" else "All participants"
  dag_names <- if (nrow(dags) > 0) stats::setNames(dags$data_access_group_name, dags$unique_group_name) else character(0)
  rec_site <- character(0)
  if (nrow(records) > 0) {
    first_dag <- tapply(records$redcap_data_access_group, records$record_id,
                        function(x) { x <- x[!blank(x)]; if (length(x)) x[1] else "" })
    rec_site <- ifelse(first_dag == "", no_dag,
                       ifelse(first_dag %in% names(dag_names), dag_names[first_dag], first_dag))
    names(rec_site) <- names(first_dag)
  }
  # Repeating set-up is read from the data (rows for a repeating event have a repeat instance but no
  # repeat instrument), so no design rights are needed. A repeating form or event with no data yet
  # gives the same counts either way.
  rep_events <- character(0); rep_forms <- data.frame(event_name = character(), form_name = character())
  if (nrow(records) > 0) {
    re <- blank(records$redcap_repeat_instrument) & !blank(records$redcap_repeat_instance)
    rep_events <- unique(records$redcap_event_name[re])
    rf <- !blank(records$redcap_repeat_instrument)
    rep_forms <- unique(data.frame(event_name = records$redcap_event_name[rf], form_name = records$redcap_repeat_instrument[rf]))
  }

  # ---- 5. Count form status for each event ----
  status_of <- function(x) {
    x <- trimws(as.character(x)); x[is.na(x)] <- ""
    bad <- unique(x[!x %in% c("", "0", "1", "2")])
    if (length(bad) > 0) stop("Unexpected form status value(s): ", paste(bad, collapse = ", "),
                              ". Export the data as raw codes.")
    c("0" = "Incomplete", "1" = "Partially complete", "2" = "Complete", " " = "")[ifelse(x == "", " ", x)]
  }
  counts <- list(); people <- list(); forms_out <- list()
  for (e in events) {
    e_forms <- mapping$form[mapping$unique_event_name == e]
    e_forms <- form_names[form_names %in% e_forms & !form_names %in% exclude_forms]
    forms_out[[e]] <- e_forms
    rows <- if (nrow(records) > 0) records[records$redcap_event_name == e, , drop = FALSE] else records
    is_rep_event <- e %in% rep_events
    main <- blank(rows$redcap_repeat_instrument)
    left_out <- character(0)
    if (e %in% names(occurred) && nrow(rows) > 0) {
      not_taken <- main & trimws(rows[[occurred[[e]]]]) == "0"
      if (is_rep_event) {
        # an instance of a repeating event that did not take place is left out
        left_out <- paste(rows$record_id[not_taken], rows$redcap_repeat_instance[not_taken])
        rows <- rows[!(paste(rows$record_id, rows$redcap_repeat_instance) %in% left_out & main), , drop = FALSE]
      } else {
        # the whole event is left out for that participant
        left_out <- unique(rows$record_id[not_taken])
        rows <- rows[!rows$record_id %in% left_out, , drop = FALSE]
      }
      main <- blank(rows$redcap_repeat_instrument)
    }
    recs <- unique(rows$record_id)
    tally <- list()
    for (f in e_forms) {
      st_col <- paste0(f, "_complete")
      if (!st_col %in% names(rows)) rows[[st_col]] <- rep("", nrow(rows))
      rep_form <- any(rep_forms$event_name == e & rep_forms$form_name == f)
      if (is_rep_event) {
        inst <- rows[main, c("record_id", st_col), drop = FALSE]
      } else if (rep_form) {
        inst <- rows[!main & rows$redcap_repeat_instrument == f, c("record_id", st_col), drop = FALSE]
        none <- setdiff(recs, inst$record_id)        # participants at the event with no instance
        if (length(none) > 0 && !f %in% as_needed_forms) {
          inst <- rbind(inst, stats::setNames(data.frame(none, "", stringsAsFactors = FALSE), c("record_id", st_col)))
        }
      } else {
        m <- rows[main, c("record_id", st_col), drop = FALSE]
        inst <- data.frame(record_id = recs, stringsAsFactors = FALSE)
        inst[[st_col]] <- m[[st_col]][match(recs, m$record_id)]
      }
      st <- status_of(inst[[st_col]])
      if (f %in% as_needed_forms) { keep <- st != ""; inst <- inst[keep, , drop = FALSE]; st <- st[keep] }
      st[st == ""] <- "Incomplete"
      tally[[f]] <- data.frame(form = rep(f, length(st)), site = unname(rec_site[inst$record_id]), status = st,
                               stringsAsFactors = FALSE)
    }
    tally <- do.call(rbind, c(list(data.frame(form = character(), site = character(), status = character())), tally))
    lo_rec <- if (is_rep_event) sub(" [^ ]*$", "", left_out) else left_out
    people[[e]] <- list(counted = unname(rec_site[recs]), left_out = unname(rec_site[lo_rec]))
    counts[[e]] <- tally
  }

  # ---- 6. Complete grid: every event x form x site x status, with zero where none ----
  sites <- unname(dag_names)
  if (any(rec_site[unique(unlist(lapply(events, function(e)
    if (nrow(records)) records$record_id[records$redcap_event_name == e] else character(0))))] == no_dag)) {
    sites <- c(sites, no_dag)
  }
  extra <- setdiff(unique(unlist(lapply(counts, function(x) x$site))), sites)
  sites <- c(sites, extra)
  out <- list(); pp <- list()
  for (e in events) {
    for (f in forms_out[[e]]) {
      x <- counts[[e]][counts[[e]]$form == f, ]
      for (s in sites) for (k in statuses) {
        out[[length(out) + 1]] <- data.frame(event = e, form = f, site = s, status = k,
                                             n = sum(x$site == s & x$status == k), stringsAsFactors = FALSE)
      }
    }
    for (s in sites) {
      pp[[length(pp) + 1]] <- data.frame(event = e, site = s,
                                         participants = sum(people[[e]]$counted == s),
                                         left_out = sum(people[[e]]$left_out == s),
                                         stringsAsFactors = FALSE)
    }
  }
  counts_df <- if (length(out)) do.call(rbind, out) else
    data.frame(event = character(), form = character(), site = character(), status = character(), n = integer())
  part_df <- if (length(pp)) do.call(rbind, pp) else
    data.frame(event = character(), site = character(), participants = integer(), left_out = integer())
  rownames(counts_df) <- NULL; rownames(part_df) <- NULL

  ev_out <- ev[match(events, ev$unique_event_name), c("unique_event_name", "event_name", "arm_num")]
  rownames(ev_out) <- NULL
  structure(list(
    counts = counts_df,
    participants = part_df,
    events = ev_out,
    forms = data.frame(form = form_names, label = instruments$instrument_label, stringsAsFactors = FALSE),
    forms_by_event = forms_out,
    sites = sites,
    settings = list(version = version, environment = env,
                    project_title = if (nrow(project) > 0 && "project_title" %in% names(project)) project$project_title[1] else "",
                    export_time = export_time, events = events,
                    exclude_forms = if (is.null(exclude_forms)) character(0) else exclude_forms,
                    as_needed_forms = if (is.null(as_needed_forms)) character(0) else as_needed_forms,
                    timepoints = tp_source)
  ), class = "completeness_data")
}

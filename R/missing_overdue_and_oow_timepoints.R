#Title: Missing, Overdue and Out-Of-Window Timepoints
#Author: Eduard Mazuru 
#Version & Date: V2.0.0 04NOV2026
#R version: 4.4.3
#' Missing, overdue and out-of-window timepoints
#'
#' For each participant and each scheduled timepoint in the timepoints file
#' (\code{metadata}), works out the visit window from the baseline date and shows
#' whether the timepoint was attended in window, attended early or late, is missing
#' (overdue or not yet overdue), did not take place, is no longer expected (because a
#' stop rule such as a withdrawal or death applies), or cannot be assessed. Saves an
#' Excel workbook with a cover sheet, a summary by site and timepoint, one sheet per
#' category and the follow-up window plot.
#'
#' V1 calls (\code{data} and \code{metadata} only) still work: \code{study_name} is
#' then taken from the calling script, as in V1.
#'
#' @param data A REDCap export (flat, raw codes, with the DAG column if the project
#'   has DAGs), as read by the template.
#' @param metadata The timepoints file (data frame), one row per timepoint.
#'   Repeating events in it are not assessed.
#' @param by_site,simplify_ID,simplify_event_date,id_number_delimiter,id_number_position
#'   As in V1 (by_site and the id_number arguments are used by the plot).
#' @param include_split_CSVs Accepted so V1 calls still work; CSV files are no longer saved.
#' @param stop_rules Optional stop rules: a data frame or the path to a CSV with columns
#'   rule, event, date_field and logic (REDCap-style: [field]='x', <>, [field(code)],
#'   AND, OR, brackets; blank logic = any record with a date).
#' @param as_of Date days overdue are counted from (Date or "YYYY-MM-DD"); default today.
#' @param study_name Study name for the file names. If not given, taken from the
#'   calling script (as in V1).
#' @param output_dir Folder to save in; default Documents/REDCap_timepoints/<study_name>.
#'   Must not be inside a Git repository.
#' @param script Name and version of the script that ran this, shown on the cover sheet.
#' @param export_time Optional date-time the data were exported, shown on the cover sheet.
#' @return Invisibly, a list: \code{results} (one row per participant and timepoint),
#'   \code{summary} (counts by site and timepoint) and \code{file}.
#' @export
missing_overdue_and_oow_timepoints <- function(data, metadata, by_site = FALSE, include_split_CSVs = FALSE,
                                               simplify_ID = FALSE, simplify_event_date = FALSE,
                                               id_number_delimiter = "-", id_number_position = "last",
                                               stop_rules = NULL, as_of = Sys.Date(), study_name = NULL,
                                               output_dir = NULL, script = NULL, export_time = NULL) {

  version <- "2.0.0"
  categories <- c("Missing and overdue", "Missing, not yet overdue", "Attended out of window",
                  "Attended in window", "Did not take place", "Not expected", "Cannot be assessed")
  sheet_names <- c("Missing And Overdue", "Missing NOT Overdue", "Attended Out Of Window",
                   "Attended In Window", "Did Not Take Place", "Not Expected", "Cannot Be Assessed")

  #CHECK INPUTS (V2.0.0)--------------------------------------------------------SECTION 0-----------------------------------------------------
  #Study name: as in V1, taken from the calling script if it is not given.
  if (is.null(study_name)) {
    if (exists("study_name", envir = parent.frame(), inherits = TRUE)) {
      study_name <- get("study_name", envir = parent.frame(), inherits = TRUE)
    } else {
      stop("study_name is needed: give study_name = \"STUDY\" (or set study_name in the script).")
    }
  }
  if (length(study_name) != 1 || is.na(study_name) || !grepl("^[A-Za-z0-9_-]+$", trimws(study_name))) {
    stop("study_name must contain only letters, numbers, _ and -.")
  }
  study_name <- trimws(study_name)
  if (is.character(as_of) && !grepl("^\\d{4}-\\d{2}-\\d{2}$", trimws(as_of[1]))) as_of <- NA   # e.g. "03/11/2026" would be read as year 3
  as_of <- tryCatch(as.Date(as_of), error = function(e) NA)
  if (length(as_of) != 1 || is.na(as_of)) stop("as_of must be a date, e.g. \"2026-11-03\".")
  if (!is.null(script) && (length(script) != 1 || is.na(script) || !is.character(script))) {
    stop("script must be a single piece of text, e.g. \"TEMPLATE_TimepointMonitoring V2.0.0\", or left out.")
  }
  if (isTRUE(include_split_CSVs)) message("include_split_CSVs is no longer used: everything is in the workbook.")
  if (is.null(output_dir)) output_dir <- file.path(path.expand("~"), "REDCap_timepoints", study_name)

  metadata <- as.data.frame(metadata, stringsAsFactors = FALSE)
  meta_need <- c("timepoint_name", "expected_days_after_baseline", "baseline_name", "minus_days", "plus_days",
                 "record_id_field_name", "timepoint_occurred_field_name", "timepoint_date_field_name",
                 "baseline_date_field_name")
  if (!all(meta_need %in% names(metadata))) {
    stop("metadata (the timepoints file) is missing column(s): ", paste(setdiff(meta_need, names(metadata)), collapse = ", "))
  }
  for (col in c("expected_days_after_baseline", "minus_days", "plus_days")) {
    v <- suppressWarnings(as.numeric(as.character(metadata[[col]])))
    if (any(is.na(v))) stop("metadata column ", col, " must be a number on every row.")
    metadata[[col]] <- v
  }
  data <- as.data.frame(data, stringsAsFactors = FALSE, check.names = FALSE)
  for (col in c("redcap_repeat_instrument", "redcap_repeat_instance")) if (!col %in% names(data)) data[[col]] <- NA
  # blank text counts as missing, as REDCap exports read by the template have NA
  data[] <- lapply(data, function(x) { if (is.character(x)) x[trimws(x) == ""] <- NA; x })
  if (!"redcap_event_name" %in% names(data)) stop("data must have a redcap_event_name column (a longitudinal project).")
  need_fields <- unique(c(metadata$record_id_field_name, metadata$timepoint_occurred_field_name,
                          metadata$timepoint_date_field_name, metadata$baseline_date_field_name))
  if (!all(need_fields %in% names(data))) {
    stop("These fields in the timepoints file are not in the data: ", paste(setdiff(need_fields, names(data)), collapse = ", "))
  }
  #Sites: a project with no DAGs (no DAG column, or none filled in) is one group, "All participants"
  no_dags <- !"redcap_data_access_group" %in% names(data) || all(is.na(data$redcap_data_access_group))
  if (no_dags) data$redcap_data_access_group <- "All participants"
  #Repeating events have no schedule, so they are not assessed
  rep_rows <- !is.na(data$redcap_repeat_instance) & is.na(data$redcap_repeat_instrument)
  repeating_events <- unique(as.character(data$redcap_event_name[rep_rows]))
  not_assessed <- metadata$timepoint_name[metadata$timepoint_name %in% repeating_events]
  metadata <- metadata[!metadata$timepoint_name %in% repeating_events, , drop = FALSE]
  if (nrow(metadata) == 0) stop("No timepoints to assess: every timepoint in the timepoints file is a repeating event.")

  #Stop rules
  stop_source <- "Not used"
  if (!is.null(stop_rules)) {
    if (is.character(stop_rules) && length(stop_rules) == 1) {
      if (!file.exists(stop_rules)) stop("stop_rules file not found: ", stop_rules)
      stop_source <- basename(stop_rules)
      stop_rules <- utils::read.csv(stop_rules, colClasses = "character", na.strings = character(0))
    } else if (is.data.frame(stop_rules)) {
      stop_source <- "data frame"
    } else stop("stop_rules must be a data frame or the path to a CSV file.")
    stop_rules <- as.data.frame(lapply(stop_rules, function(x) { x <- as.character(x); x[is.na(x)] <- ""; trimws(x) }),
                                stringsAsFactors = FALSE)
    if (!"logic" %in% names(stop_rules)) stop_rules$logic <- ""
    if (!all(c("rule", "event", "date_field") %in% names(stop_rules))) {
      stop("stop_rules must have the columns rule, event, date_field and logic.")
    }
    stop_rules <- stop_rules[!(stop_rules$rule == "" & stop_rules$event == "" & stop_rules$date_field == ""), , drop = FALSE]
    bad <- which(stop_rules$rule == "" | stop_rules$event == "" | stop_rules$date_field == "")
    if (length(bad) > 0) stop("stop_rules row(s) ", paste(bad, collapse = ", "), ": rule, event and date_field must all be filled in.")
    bad <- setdiff(stop_rules$date_field, names(data))
    if (length(bad) > 0) stop("stop_rules: these date fields are not in the data: ", paste(bad, collapse = ", "))
    stop_rules$test <- lapply(stop_rules$logic, read_stop_logic, fields = names(data))
    for (ev in setdiff(unique(stop_rules$event), unique(as.character(data$redcap_event_name)))) {
      message("stop_rules: there is no data yet at event '", ev, "' - check the spelling if you expected some.")
    }
  }

  #REQUIRED EVENTS--------------------------------------------------------------SECTION 1-----------------------------------------------------
  
  #Defining required events (and matching baseline events) into new character vectors.
  events <- metadata$timepoint_name
  baselines <- metadata$baseline_name #These are somewhat redundant.
  #Defining a new vector with all events including baseline.
  events_include_bl <-unique(append(baselines, events))
  
  
  #PARTICIPANT IDS--------------------------------------------------------------SECTION 2-----------------------------------------------------
  
  #Defining character vector of unique participant IDs within the data set (to be compared against IDs which have completed events).
  record_ids <- unique(metadata$record_id_field_name)
  data_unique_IDs <- unique(
    dplyr::select(data,
                  all_of(record_ids)
    )
  )
  
  #Store data from metadata file into data frame sites_by_unique_IDs in which each participant's site/DAG (Data Access Group) will be stored.
  #First, filter out record ids and the redcap_data_access_group (site).
  sites_by_unique_IDs <- dplyr::select(data,
                                       redcap_data_access_group,
                                       all_of(record_ids)
  )
  #Then filter duplicate rows, which should leave a 1 to 1 map of record IDs to site/DAG.
  sites_by_unique_IDs <- dplyr::distinct(sites_by_unique_IDs)
  #V2.0.0 (AI-drafted fix): keep one row per participant (their first DAG), so a participant whose DAG is
  #blank on some rows is not listed twice (that put later participants against the wrong site in V1).
  sites_by_unique_IDs <- sites_by_unique_IDs[order(is.na(sites_by_unique_IDs$redcap_data_access_group)), , drop = FALSE]
  sites_by_unique_IDs <- sites_by_unique_IDs[!duplicated(sites_by_unique_IDs[[record_ids[1]]]), , drop = FALSE]
  sites_by_unique_IDs <- sites_by_unique_IDs[match(unique(data[[record_ids[1]]]), sites_by_unique_IDs[[record_ids[1]]]), , drop = FALSE]
  
  
  #Additionally define list of sites/DAGs
  all_sites <- dplyr::select(sites_by_unique_IDs,
                             redcap_data_access_group
  )
  #Filter to only include unique sites/DAGs
  all_sites <- dplyr::distinct(all_sites)
  
  #Sort list of sites/DAGs
  all_sites <- dplyr::arrange(all_sites,
                              redcap_data_access_group
  )
  
  
  #REQUIRED FIELDS--------------------------------------------------------------SECTION 3-----------------------------------------------------
  
  #Defining character vectors to store field names pulled from the metadata file.
  field_timepoint_yn <- metadata$timepoint_occurred_field_name
  field_timepoint_date <- metadata$timepoint_date_field_name
  field_baseline_date <- metadata$baseline_date_field_name
  
  #Grouping fields into new variables for ease of use.
  
  #Defining a variable to store all fields that store information on whether or not a timepoint occurred.
  #Baseline "occurred" status is no longer tracked via a separate yes/no field - it's inferred directly
  #from whether the baseline date field is populated (see Section 4 and Section 5).
  events_occurredyn_fields <- field_timepoint_yn
  
  #Defining a variable to store all required fields
  required_fields <- unique(c(record_ids,
                              "redcap_event_name",
                              "redcap_data_access_group",
                              field_baseline_date,
                              field_timepoint_yn,
                              field_timepoint_date
  )
  )
  
  
  #DEFINING TEMPORARY DATA FRAMES FOR FUNCTION----------------------------------SECTION 4-----------------------------------------------------
  
  #Create temporary data frame to store and manipulate data.
  df <- data %>%
    #Filter to exclude redundant information.
    dplyr::filter(
      #Filter to include only required events listed in metadata.
      redcap_event_name %in% events_include_bl,
      #Filter to remove forms with repeated visit instances.
      is.na(redcap_repeat_instance),
      #Keep rows where a timepoint was marked as occurred, OR a baseline date has been recorded
      #(baseline "occurred" is inferred from the date field being populated, not a yes/no field).
      if_any(all_of(events_occurredyn_fields), ~ .x == 1) |
        if_any(all_of(field_baseline_date), ~ !is.na(.x))
    ) %>%
    #Select columns to only show required variables for function.
    dplyr::select(all_of(required_fields))
  
  #CLASSIFY EACH PARTICIPANT AND TIMEPOINT (V2.0.0)-----------------------------SECTION 5-----------------------------------------------------
  rec_col <- record_ids[1]
  as_d <- function(x) as.Date(as.character(x))
  is_main <- is.na(data$redcap_repeat_instance) & is.na(data$redcap_repeat_instrument)
  main <- data[is_main, , drop = FALSE]
  #Site for each participant: their first DAG; "Unknown" if they have none
  first_dag <- tapply(as.character(data$redcap_data_access_group), as.character(data[[rec_col]]),
                      function(x) { x <- x[!is.na(x)]; if (length(x)) x[1] else NA_character_ })
  site_of <- function(id) { s <- unname(first_dag[as.character(id)]); ifelse(is.na(s), "Unknown", s) }

  #Stop date for each participant: the earliest date from any stop rule that matches
  stop_date <- stats::setNames(as.Date(character(0)), character(0)); stop_rule <- character(0)
  if (!is.null(stop_rules)) {
    for (k in seq_len(nrow(stop_rules))) {
      rows <- data[as.character(data$redcap_event_name) == stop_rules$event[k], , drop = FALSE]
      if (nrow(rows) == 0) next
      d <- as_d(rows[[stop_rules$date_field[k]]])
      hit <- stop_rules$test[[k]](rows) & !is.na(d)
      for (i in which(hit)) {
        id <- as.character(rows[[rec_col]][i])
        if (!id %in% names(stop_date) || d[i] < stop_date[[id]]) {
          stop_date[id] <- d[i]; stop_rule[id] <- stop_rules$rule[k]
        }
      }
    }
  }
  fmt <- function(d) ifelse(is.na(d), "", format(d, "%d %b %Y"))

  res <- list()
  for (j in seq_len(nrow(metadata))) {
    m <- metadata[j, ]
    base_rows <- main[as.character(main$redcap_event_name) == m$baseline_name, , drop = FALSE]
    ev_rows <- main[as.character(main$redcap_event_name) == m$timepoint_name, , drop = FALSE]
    for (id in unique(as.character(base_rows[[rec_col]]))) {
      b <- as_d(base_rows[[m$baseline_date_field_name]][as.character(base_rows[[rec_col]]) == id])
      b <- b[!is.na(b)]; b <- if (length(b)) b[1] else as.Date(NA)
      e <- ev_rows[as.character(ev_rows[[rec_col]]) == id, , drop = FALSE]
      occ <- if (nrow(e)) trimws(as.character(e[[m$timepoint_occurred_field_name]][1])) else NA
      tdate <- if (nrow(e)) as_d(e[[m$timepoint_date_field_name]][1]) else as.Date(NA)
      expd <- b + m$expected_days_after_baseline; ws <- expd - m$minus_days; we <- expd + m$plus_days
      status <- NA; days <- NA_integer_; detail <- ""
      if (is.na(b)) {
        status <- "Cannot be assessed"; detail <- "No baseline date"
      } else if (!is.na(occ) && occ == "1") {
        if (is.na(tdate)) {
          status <- "Cannot be assessed"; detail <- "Marked as taken place but no date recorded"
        } else if (tdate < ws) {
          status <- "Attended out of window"; days <- as.integer(ws - tdate); detail <- paste0("Early by ", days, " day(s)")
        } else if (tdate > we) {
          status <- "Attended out of window"; days <- as.integer(tdate - we); detail <- paste0("Late by ", days, " day(s)")
        } else {
          status <- "Attended in window"
        }
      } else if (is.na(occ) && !is.na(tdate)) {
        status <- "Cannot be assessed"; detail <- "Date recorded but not marked as taken place"
      } else if (id %in% names(stop_date) && expd > stop_date[[id]]) {
        status <- "Not expected"
        detail <- paste0("Stopped by: ", stop_rule[[id]], " (", fmt(stop_date[[id]]), ")")
      } else if (!is.na(occ) && occ == "0") {
        status <- "Did not take place"
      } else if (as_of > we) {
        status <- "Missing and overdue"; days <- as.integer(as_of - we)
        detail <- paste0("Overdue by ", days, " day(s) as of ", fmt(as_of))
      } else {
        status <- "Missing, not yet overdue"; days <- as.integer(we - as_of)
        detail <- paste0("Overdue in ", days, " day(s) (as of ", fmt(as_of), ")")
      }
      res[[length(res) + 1]] <- data.frame(
        site = site_of(id), record_id = id, timepoint = m$timepoint_name, status = status,
        baseline_date = b, expected_date = expd, window_start = ws, window_end = we,
        timepoint_date = tdate, days = days, detail = detail, stringsAsFactors = FALSE)
    }
  }
  results <- if (length(res)) do.call(rbind, res) else
    data.frame(site = character(), record_id = character(), timepoint = character(), status = character(),
               baseline_date = as.Date(character()), expected_date = as.Date(character()), window_start = as.Date(character()),
               window_end = as.Date(character()), timepoint_date = as.Date(character()), days = integer(),
               detail = character(), stringsAsFactors = FALSE)
  site_order <- sort(unique(results$site)); site_order <- c(setdiff(site_order, "Unknown"), intersect(site_order, "Unknown"))
  results <- results[order(match(results$site, site_order), results$record_id,
                           match(results$timepoint, metadata$timepoint_name)), , drop = FALSE]
  rownames(results) <- NULL
  names(results)[names(results) == "record_id"] <- rec_col

  #Summary: count in each category for each site and timepoint, plus all sites
  summ <- list()
  for (tp in metadata$timepoint_name) for (s in c(site_order, "All sites")) {
    x <- results[results$timepoint == tp & (s == "All sites" | results$site == s), , drop = FALSE]
    summ[[length(summ) + 1]] <- data.frame(timepoint = tp, site = s,
                                           t(stats::setNames(sapply(categories, function(k) sum(x$status == k)), categories)),
                                           Total = nrow(x), check.names = FALSE, stringsAsFactors = FALSE)
  }
  summary_table <- do.call(rbind, summ); rownames(summary_table) <- NULL

  #Output folder: created if needed, never inside a Git repository
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(output_dir)) stop("Could not create output_dir: ", output_dir)
  out_abs <- normalizePath(output_dir, winslash = "/")
  d_up <- out_abs
  repeat {
    if (file.exists(file.path(d_up, ".git"))) stop("output_dir is inside a Git repository (", d_up, "). Save the report outside any repository.")
    parent <- dirname(d_up); if (parent == d_up) break; d_up <- parent
  }

  #FOLLOW-UP WINDOW PLOT DATA----------------------------------------------------SECTION 6b-----------------------------------------------------
  
  #Build a simple one-row-per-participant table for the plot: one column per timepoint, holding "days
  #after baseline" for that timepoint. This is deliberately separate from (and much simpler than) the
  #detailed missing/overdue/out-of-window logic above - it reuses the same "occurred"/"date" field
  #names already pulled from "metadata" in Section 3, just pivoted into a wide, easy-to-plot shape,
  #the same way as: test <- subset(data, is.na(redcap_repeat_instance) & <event> & <occurred>==1);
  #pivot_wider(test, id_cols = record_id, names_from = redcap_event_name, values_from = <date field>)
  
  #Map each event name to its own "date" field (from metadata), so each row's date is pulled from the
  #right column even when different timepoints use different field names.
  event_date_field_lookup <- data.frame(
    redcap_event_name = c(baselines, events),
    date_field = c(field_baseline_date, field_timepoint_date),
    stringsAsFactors = FALSE
  )
  event_date_field_lookup <- dplyr::distinct(event_date_field_lookup)
  
  #"df" (defined in Section 4) is already filtered to required events, non-repeating rows, and
  #occurred == 1 - exactly what's needed here too, so it's reused as-is rather than re-filtering data.
  plot_source <- df
  plot_source$date_field <- event_date_field_lookup$date_field[
    match(plot_source$redcap_event_name, event_date_field_lookup$redcap_event_name)
  ]
  #Pull each row's date from whichever field the lookup above says holds it - looped over the handful
  #of distinct date field names (not over every row, which would be much slower).
  plot_source$event_date <- as.Date(NA)
  for (date_field_name in unique(stats::na.omit(plot_source$date_field))) {
    field_rows <- which(plot_source$date_field == date_field_name)
    plot_source$event_date[field_rows] <- as.Date(plot_source[[date_field_name]][field_rows])
  }
  
  #Pivot to one row per participant, one column per event (values_fn keeps just the first date if a
  #participant somehow has more than one row for the same event).
  plot_wide <- dplyr::select(plot_source, dplyr::all_of(record_ids[1]), redcap_event_name, event_date)
  plot_wide <- tidyr::pivot_wider(plot_wide,
                                  id_cols = dplyr::all_of(record_ids[1]),
                                  names_from = redcap_event_name,
                                  values_from = event_date,
                                  values_fn = ~ .x[1]
  )
  
  #Compute "days after baseline" for each timepoint, using that timepoint's own associated baseline
  #(matching "events" and "baselines" up by position, same pairing the detailed loop above uses).
  for (j in seq_along(events)) {
    days_col <- paste0(events[j], "_days")
    if (all(c(as.character(events[j]), as.character(baselines[j])) %in% names(plot_wide))) {
      plot_wide[[days_col]] <- as.numeric(plot_wide[[events[j]]] - plot_wide[[baselines[j]]])
    } else {
      plot_wide[[days_col]] <- NA_real_
    }
  }
  
  #Attach each participant's site, matching the same site/DAG assignment used for the rest of the
  #workbook (rather than re-parsing a site prefix out of the ID text).
  plot_table <- dplyr::left_join(plot_wide, sites_by_unique_IDs, by = record_ids[1])
  
  #Parse a sortable participant number from the ID (letters ignored, e.g. "01-A014B" -> 14) purely to
  #order participants sensibly within their own site - this number is never shown on the plot itself.
  extract_participant_number <- function(id, delimiter = "-", position = "last") {
    id <- as.character(id)
    segments <- strsplit(id, delimiter, fixed = TRUE)[[1]]
    if (length(segments) == 0) {
      return(NA_real_)
    }
    segment <- if (position == "last") {
      segments[length(segments)]
    } else if (position == "first") {
      segments[1]
    } else {
      segments[as.numeric(position)]
    }
    if (is.na(segment)) {
      return(NA_real_)
    }
    digits_only <- gsub("[^0-9]", "", segment)
    if (digits_only == "") {
      return(NA_real_)
    }
    as.numeric(digits_only)
  }
  plot_table <- dplyr::mutate(plot_table,
                              participant_sort_number = sapply(.data[[record_ids[1]]],
                                                               extract_participant_number,
                                                               delimiter = id_number_delimiter,
                                                               position = id_number_position
                              )
  )
  
  #Compact per-site y-position: each participant's rank within their own site, stacked onto a running
  #offset so every site gets its own block sized to its OWN participant count (plus a small gap) - not
  #a fixed allowance per site, which would waste a lot of vertical space once sites vary a lot in size.
  plot_positions <- dplyr::arrange(plot_table, redcap_data_access_group, participant_sort_number)
  plot_positions <- dplyr::group_by(plot_positions, redcap_data_access_group)
  plot_positions <- dplyr::mutate(plot_positions, rank_in_site = dplyr::row_number())
  plot_positions <- dplyr::ungroup(plot_positions)
  
  site_blocks <- dplyr::group_by(plot_positions, redcap_data_access_group)
  site_blocks <- dplyr::summarise(site_blocks, site_n = dplyr::n())
  site_blocks <- dplyr::arrange(site_blocks, redcap_data_access_group)
  site_blocks <- dplyr::mutate(site_blocks, block_start = dplyr::lag(cumsum(site_n + 2), default = 0))
  site_blocks <- dplyr::select(site_blocks, redcap_data_access_group, block_start)
  
  plot_positions <- dplyr::left_join(plot_positions, site_blocks, by = "redcap_data_access_group")
  plot_positions <- dplyr::mutate(plot_positions, plot_position = block_start + rank_in_site)
  plot_table <- dplyr::left_join(plot_table,
                                 dplyr::select(plot_positions, dplyr::all_of(record_ids[1]), plot_position),
                                 by = record_ids[1]
  )
  
  #One y-axis label per site, positioned at that site's own block midpoint. DAG values are often
  #formatted like "01 - Risley" - only the leading token (before any space/hyphen) is used so the axis
  #stays short, e.g. "01".
  label_rows <- dplyr::group_by(plot_positions, redcap_data_access_group)
  label_rows <- dplyr::summarise(label_rows, label_position = mean(range(plot_position)))
  label_rows <- dplyr::mutate(label_rows, site_label = sub("^([^\\s-]+).*$", "\\1", trimws(as.character(redcap_data_access_group))))
  label_rows <- dplyr::arrange(label_rows, label_position)
  
  #EXPORT PLOT------------------------------------------------------------------SECTION 6c-----------------------------------------------------
  
  timepoint_plot <- function(plot_table, metadata) {
    
    days_cols <- paste0(events, "_days")
    
    plot_long <- tidyr::pivot_longer(plot_table,
                                     cols = dplyr::all_of(days_cols),
                                     names_to = "timepoint_name",
                                     values_to = "days_after_baseline"
    )
    plot_long <- dplyr::mutate(plot_long, timepoint_name = sub("_days$", "", timepoint_name))
    plot_long <- dplyr::mutate(plot_long, timepoint_name = factor(timepoint_name, levels = as.character(events)))
    plot_long <- dplyr::filter(plot_long, !is.na(days_after_baseline))
    
    xmin <- suppressWarnings(min(plot_long$days_after_baseline, 0, na.rm = TRUE))
    xmax <- suppressWarnings(max(plot_long$days_after_baseline, na.rm = TRUE))
    x_breaks <- seq(from = 50 * (floor(xmin / 50) - 1), to = xmax + 50, by = 50)
    
    timepoint_windows <- dplyr::select(metadata, timepoint_name, expected_days_after_baseline, minus_days, plus_days)
    timepoint_windows <- dplyr::mutate(timepoint_windows,
                                       timepoint_window_min = expected_days_after_baseline - minus_days,
                                       timepoint_window_max = expected_days_after_baseline + plus_days,
                                       timepoint_name = factor(as.character(timepoint_name), levels = levels(plot_long$timepoint_name))
    )
    
    #Define a distinct, easily distinguishable colour per timepoint (rather than a continuous viridis
    #gradient), so timepoints are easy to tell apart at a glance. Recycled via colorRampPalette if
    #there are more timepoints than defined colours.
    base_event_colours <- c("#8aadf4", "#ed8796", "#a6da95", "#eed49f", "#c6a0f6", "#f5a97f", "#91d7e3")
    if (length(events) > length(base_event_colours)) {
      base_event_colours <- grDevices::colorRampPalette(base_event_colours)(length(events))
    }
    event_colours <- setNames(base_event_colours[seq_along(events)], as.character(events))
    
    p <- ggplot(data = plot_long, aes(x = days_after_baseline, y = plot_position)) +
      #Shaded window band, spanning the full height of each panel. ymin/ymax are fixed at -Inf/Inf
      #(rather than a computed min/max) specifically so this works correctly per-facet: a computed
      #min/max would be the same constant on every panel, forcing every facet to expand its own
      #y-range out to the full study-wide range instead of just that site's own participants. -Inf/Inf
      #always fills whatever range the panel actually ends up with.
      geom_rect(data = timepoint_windows,
                aes(xmin = timepoint_window_min, xmax = timepoint_window_max, fill = timepoint_name),
                ymin = -Inf, ymax = Inf, alpha = 0.1, inherit.aes = FALSE
      ) +
      #Dashed line marking each timepoint's expected/target day within its window.
      geom_vline(data = timepoint_windows,
                 aes(xintercept = expected_days_after_baseline),
                 linetype = "dashed", colour = "grey30", linewidth = 0.4
      ) +
      geom_point(aes(colour = timepoint_name), size = 2, alpha = 0.8) +
      scale_x_continuous(breaks = x_breaks) +
      #Label the Y-axis with one tick per site (e.g. "01", "02"...). Even in facet mode, each panel
      #now only ever shows the labels that actually fall inside its own (correctly free-scaled) range,
      #since the geom_rect fix above stopped every panel's range from being inflated to the full
      #study-wide extent.
      scale_y_continuous(breaks = label_rows$label_position, labels = label_rows$site_label) +
      #Set distinct, easily distinguishable colours for event timepoints and timepoint windows.
      scale_fill_manual(values = event_colours, name = "Timepoint") +
      scale_colour_manual(values = event_colours, name = "Timepoint") +
      labs(
        title = "Follow-up Window Completion",
        x = "Days after Baseline Visit",
        y = "Site"
      ) +
      theme_minimal()
    
    if (by_site == TRUE) {
      p <- p + facet_grid(redcap_data_access_group ~ ., scales = "free_y")
    }
    
    p
    
  }
  
  #Store the plot object (rather than just calling it) so it can be saved and inserted into the
  #exported workbook further down. A bare function call mid-function does not print or save a ggplot
  #object.
  #V2.0.0 (AI-drafted fix): V1 stopped with an error when no timepoint had been attended yet (nothing to plot),
  #for example early in a study. The plot is now only drawn when there is something to plot.
  has_points <- any(!is.na(unlist(plot_table[paste0(events, "_days")])))
  if (has_points) csm_plot <- timepoint_plot(plot_table, metadata)
  #EXPORT XLSX (V2.0.0)---------------------------------------------------------SECTION 6d----------------------------------------------------
  file_name_final <- file.path(out_abs, paste0(study_name, "_Missing_Overdue_And_OOW_Timepoints_", format(as_of, "%Y-%m-%d"), ".xlsx"))
  export_workbook <- openxlsx::createWorkbook()
  tabColourlist <- c("#ed8796", "#f5a97f", "#eed49f", "#a6da95", "#c6a0f6", "#91d7e3", "#7dc4e4")
  head_style <- function(col) openxlsx::createStyle(textDecoration = "BOLD", fontColour = "#000000", fontSize = 12,
                                                     fgFill = col, border = "TopBottomLeftRight", wrapText = TRUE)
  #Cover sheet
  cover <- data.frame(Item = c("Report", "Study", "As of (days overdue counted from)", "Data exported", "Report saved by",
                               "Script", "Function", "Timepoints assessed", "Not assessed (repeating events)",
                               "Stop rules", "How timepoints are shown"),
                      Value = c("Missing, overdue and out-of-window timepoints", study_name, fmt(as_of),
                                if (is.null(export_time)) "(not given)" else format(as.POSIXct(export_time), "%d %b %Y %H:%M"),
                                Sys.info()[["user"]], if (is.null(script) || trimws(script) == "") "(not given)" else trimws(script),
                                paste0("missing_overdue_and_oow_timepoints V", version),
                                paste0(metadata$timepoint_name, " (", metadata$expected_days_after_baseline, " days, -",
                                       metadata$minus_days, "/+", metadata$plus_days, ")", collapse = "; "),
                                if (length(not_assessed)) paste(not_assessed, collapse = ", ") else "None",
                                if (is.null(stop_rules)) "Not used" else
                                  paste0(stop_source, ": ", paste0(stop_rules$rule, " (", stop_rules$event, ", ", stop_rules$date_field,
                                                                   ifelse(stop_rules$logic == "", "", paste0(", ", stop_rules$logic)), ")",
                                                                   collapse = "; ")),
                                paste("Window = baseline date + expected days, from minus days early to plus days late.",
                                      "Missing and overdue = the window has closed by the 'as of' date.",
                                      "Not expected = the timepoint's expected date is after the earliest date from a matching stop rule;",
                                      "timepoints expected before that date still count.",
                                      "Participants with no data at the baseline event are not included.")),
                      stringsAsFactors = FALSE)
  openxlsx::addWorksheet(export_workbook, "Cover")
  openxlsx::writeData(export_workbook, "Cover", cover, headerStyle = head_style("#BBBBBB"))
  openxlsx::addStyle(export_workbook, "Cover", openxlsx::createStyle(wrapText = TRUE, valign = "top", border = "TopBottomLeftRight"),
                     rows = 2:(nrow(cover) + 1), cols = 1:2, gridExpand = TRUE)
  openxlsx::setColWidths(export_workbook, "Cover", cols = 1:2, widths = c(34, 110))
  #Summary
  openxlsx::addWorksheet(export_workbook, "Summary")
  openxlsx::writeData(export_workbook, "Summary", summary_table, headerStyle = head_style("#BBBBBB"), borders = "all")
  openxlsx::addStyle(export_workbook, "Summary", openxlsx::createStyle(textDecoration = "BOLD"),
                     rows = which(summary_table$site == "All sites") + 1, cols = 1:ncol(summary_table), gridExpand = TRUE, stack = TRUE)
  openxlsx::freezePane(export_workbook, "Summary", firstRow = TRUE)
  openxlsx::setColWidths(export_workbook, "Summary", cols = 1:ncol(summary_table), widths = c(24, 18, rep(14, length(categories) + 1)))
  #One sheet per category
  show <- results
  for (col in c("baseline_date", "expected_date", "window_start", "window_end", "timepoint_date")) show[[col]] <- fmt(show[[col]])
  for (k in seq_along(categories)) {
    x <- show[show$status == categories[k], setdiff(names(show), "status"), drop = FALSE]
    openxlsx::addWorksheet(export_workbook, sheet_names[k], tabColour = tabColourlist[k])
    openxlsx::writeData(export_workbook, sheet_names[k], x, borders = "all", headerStyle = head_style(tabColourlist[k]))
    openxlsx::freezePane(export_workbook, sheet_names[k], firstRow = TRUE)
    openxlsx::setColWidths(export_workbook, sheet_names[k], cols = 1:ncol(x), widths = "auto")
    if (categories[k] == "Missing and overdue" && nrow(x) > 0) {
      openxlsx::conditionalFormatting(export_workbook, sheet_names[k], rows = 2:(nrow(x) + 1), cols = which(names(x) == "days"),
                                      style = c("white", "red"), type = "colourScale")
    }
  }

  if (!has_points) {
    openxlsx::addWorksheet(export_workbook, "Plot", tabColour = "#8aadf4")
    openxlsx::writeData(export_workbook, "Plot", "No timepoints have been attended yet, so there is nothing to plot.")
  } else {
  #ADD FOLLOW-UP WINDOW PLOT AS ITS OWN SHEET------------------------------------------------------------------------------------------------
  
  #Add a worksheet to hold the follow-up window completion plot, matching the style of the other tabs.
  openxlsx::addWorksheet(export_workbook, "Plot", tabColour = "#8aadf4")
  
  #Set a plot image file name, following the same naming convention as the other exports.
  plot_file_name <- file.path(out_abs, paste0(study_name, "_Followup_Window_Plot_", format(as_of, "%Y-%m-%d"), ".png"))  #V2.0.0: saved with the workbook, named by the as_of date like the workbook
  
  #Height scales with how many participants (or how wide a numeric range) need to be shown vertically,
  #rather than a fixed constant - a fixed height works fine for a small pilot study but crushes every
  #participant's dots into what looks like a single line once a study has more than a handful of people.
  #~0.12in per unit of participant-number range keeps adjacent participants' dots visibly separated at
  #150dpi; a 7in floor and 50in ceiling keep small studies sensibly proportioned and huge ones exportable
  #(a very large study may still need the ceiling raised further, or geom_point's size reduced, by hand).
  INCHES_PER_PARTICIPANT_UNIT <- 0.12
  
  if (by_site == TRUE) {
    #Facet panels are equal height by default, so the height needed is driven by whichever single site
    #has the most participants, multiplied by the number of site panels.
    site_ranges <- dplyr::group_by(plot_positions, redcap_data_access_group)
    site_ranges <- dplyr::summarise(site_ranges, site_range = suppressWarnings(max(rank_in_site, na.rm = TRUE)))
    site_ranges <- dplyr::mutate(site_ranges, site_range = ifelse(is.infinite(site_range), 1, site_range))
    widest_site_range <- max(site_ranges$site_range, 1)
    plot_height_in <- min(50, max(6, nrow(all_sites) * max(2, widest_site_range * INCHES_PER_PARTICIPANT_UNIT)))
  } else {
    combined_range <- suppressWarnings(max(plot_positions$plot_position, na.rm = TRUE))
    if (!is.finite(combined_range) || combined_range <= 0) {
      combined_range <- 1
    }
    plot_height_in <- min(50, max(7, combined_range * INCHES_PER_PARTICIPANT_UNIT))
  }
  
  #Save the plot to file so it can be inserted into the workbook (a bare ggplot object can't be
  #inserted directly - openxlsx::insertImage needs an actual image file on disk).
  ggsave(
    filename = plot_file_name,
    plot = csm_plot,
    width = 12,
    height = plot_height_in,
    dpi = 150,
    limitsize = FALSE
  )
  
  #Insert the saved plot image into the "Plot" worksheet.
  openxlsx::insertImage(
    export_workbook,
    "Plot",
    plot_file_name,
    width = 12,
    height = plot_height_in,
    units = "in",
    dpi = 150
  )
  }

  #Save the workbook
  openxlsx::saveWorkbook(export_workbook, file = file_name_final, overwrite = TRUE)
  message("Timepoint report saved: ", file_name_final)
  invisible(list(results = results, summary = summary_table, file = file_name_final))
}

#STOP RULE LOGIC (V2.0.0)---------------------------------------------------------------------------------------------------------------------
#' Read stop rule logic written like REDCap branching logic
#'
#' Supports [field]='value', [field]<>'value', checkboxes as [field(code)]='1', AND, OR
#' and brackets. Values can be in single or double quotes, or plain numbers. Blank
#' logic matches every row. Returns a function that takes data rows and returns TRUE
#' or FALSE for each row. Stops with a clear message if the logic can't be read or
#' names a field that isn't in the data.
#' @param logic The logic (one piece of text).
#' @param fields The column names of the data.
#' @export
read_stop_logic <- function(logic, fields) {
  logic <- trimws(as.character(logic))
  if (is.na(logic) || logic == "") return(function(rows) rep(TRUE, nrow(rows)))
  pattern <- "\\s+|\\[([A-Za-z0-9_]+)(?:\\(([A-Za-z0-9_-]+)\\))?\\]|'([^']*)'|\"([^\"]*)\"|<>|=|\\(|\\)|(?i:\\bAND\\b)|(?i:\\bOR\\b)|-?[0-9]+(?:\\.[0-9]+)?"
  pos <- 1; out <- character(0); used <- character(0); n <- nchar(logic)
  while (pos <= n) {
    rest <- substr(logic, pos, n)
    m <- regexpr(paste0("^(?:", pattern, ")"), rest, perl = TRUE)
    if (m != 1) stop("Stop rule logic can't be read at \"", substr(rest, 1, 20), "\" in: ", logic,
                     ". Use [field]='value', <>, [field(code)], AND, OR and brackets.", call. = FALSE)
    tok <- substr(rest, 1, attr(m, "match.length"))
    pos <- pos + nchar(tok)
    if (grepl("^\\s+$", tok)) next
    if (grepl("^\\[", tok)) {
      f <- sub("^\\[([A-Za-z0-9_]+).*$", "\\1", tok)
      code <- if (grepl("\\(", tok)) sub("^\\[[A-Za-z0-9_]+\\(([A-Za-z0-9_-]+)\\)\\]$", "\\1", tok) else ""
      col <- if (code != "") paste0(f, "___", gsub("-", "_", code)) else f
      used <- c(used, col)
      out <- c(out, paste0(".v(\"", col, "\")"))
    } else if (grepl("^['\"]", tok)) {
      val <- substr(tok, 2, nchar(tok) - 1)
      out <- c(out, deparse(val))
    } else if (grepl("^-?[0-9]", tok)) {
      out <- c(out, deparse(tok))
    } else if (tok == "=") { out <- c(out, "==")
    } else if (tok == "<>") { out <- c(out, "!=")
    } else if (toupper(tok) == "AND") { out <- c(out, "&")
    } else if (toupper(tok) == "OR") { out <- c(out, "|")
    } else out <- c(out, tok)
  }
  bad <- setdiff(used, fields)
  if (length(bad) > 0) stop("Stop rule logic names field(s) not in the data: ", paste(bad, collapse = ", "),
                            " (in: ", logic, ")", call. = FALSE)
  expr <- tryCatch(parse(text = paste(out, collapse = " ")), error = function(e)
    stop("Stop rule logic can't be read (check the brackets and AND/OR): ", logic, call. = FALSE))
  # every comparison must be field-versus-value: check the structure
  ok_tree <- function(e) {
    if (is.call(e)) {
      op <- as.character(e[[1]])
      if (op == "(") return(ok_tree(e[[2]]))
      if (op %in% c("&", "|")) return(length(e) == 3 && ok_tree(e[[2]]) && ok_tree(e[[3]]))
      if (op %in% c("==", "!=")) {
        is_field <- function(z) is.call(z) && identical(as.character(z[[1]]), ".v")
        return(length(e) == 3 && ((is_field(e[[2]]) && is.character(e[[3]])) || (is.character(e[[2]]) && is_field(e[[3]]))))
      }
    }
    FALSE
  }
  if (length(expr) != 1 || !ok_tree(expr[[1]])) {
    stop("Stop rule logic can't be read: each part must compare a [field] with a value, joined by AND or OR: ", logic, call. = FALSE)
  }
  function(rows) {
    .v <- function(col) { x <- as.character(rows[[col]]); x[is.na(x)] <- ""; trimws(x) }
    r <- eval(expr[[1]], list(.v = .v))
    r[is.na(r)] <- FALSE
    r
  }
}

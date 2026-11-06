#Title: Central statistical monitoring review
#Author: Paigan Aspinall
#Version & Date: V1.0.0 06NOV2026
#R version: 4.4.3
#
#Replaces overall_distribution_summary, site_distribution_summary, z_score_analysis, mahalanobis_analysis,
#variance_analysis, ad_distribution_analysis, clustering_analysis, missing_data_analysis, similarity_detection,
#decimal_preference_analysis, summary_stats and export_csm_workbook.

#' Read REDCap missing data codes
#'
#' Turns the missing data codes from REDCap's project information (Project Setup > Additional
#' customizations, exported by the API as \code{missing_data_codes}) into a vector of codes, and lets a
#' study add codes or replace them.
#'
#' @param x The \code{missing_data_codes} text from REDCap's project information, e.g.
#'   "NI, No information | UNK, Unknown". May be blank.
#' @param add Codes to add to those from REDCap, e.g. c("ND").
#' @param replace Codes to use instead of those from REDCap. Use this or \code{add}, not both.
#' @return A character vector of codes, e.g. c("NI", "UNK").
#' @examples
#' csm_missing_codes("NI, No information | UNK, Unknown")
#' csm_missing_codes("NI, No information", add = "ND")
#' @export
csm_missing_codes <- function(x = "", add = NULL, replace = NULL) {
  if (!is.null(add) && !is.null(replace)) stop("Use add or replace, not both.")
  if (!is.null(replace)) {
    if (!is.character(replace)) stop("replace must be text, e.g. c(\"NI\", \"UNK\").")
    return(unique(trimws(replace[!is.na(replace) & trimws(replace) != ""])))
  }
  if (is.null(x) || length(x) == 0 || is.na(x[1])) x <- ""
  parts <- trimws(unlist(strsplit(paste(x, collapse = "\n"), "\\||\n|\r")))
  parts <- parts[parts != ""]
  codes <- trimws(sub(",.*$", "", parts))
  if (!is.null(add)) {
    if (!is.character(add)) stop("add must be text, e.g. c(\"ND\").")
    codes <- c(codes, trimws(add))
  }
  unique(codes[codes != ""])
}

#' Central statistical monitoring review
#'
#' Runs the central statistical monitoring (CSM) analyses described in WIDM107 on one or more REDCap
#' exports and saves one Excel workbook. Each analysis is run separately for each event listed for the
#' item in the critical data items file. Only items with required_yn = "y" are used.
#'
#' Analyses: overall and site distribution summaries; z-scores (each site against the rest of the
#' study); Brown-Forsythe (variability between sites); Anderson-Darling (each site's distribution against
#' the rest); Mahalanobis distance and k-means clustering for multivariate groups; missing data items;
#' similar records; digit preference. Sites with fewer than \code{min_site_n} records for an item and
#' event are not assessed and are listed on the "Not assessed" sheet with the reason.
#'
#' @param data A REDCap record export (flat, raw values, with DAGs), or a named list of exports, one per
#'   project, named as in the critical data items \code{project} column. Values should be text exactly as
#'   exported (read with colClasses = "character"), so that decimal places are kept.
#' @param metadata The critical data items: a data frame or the path to the CSV. Columns field_name,
#'   variable_type, required_yn, event_names, multivariate_yn, multivariate_group, and project if
#'   \code{data} is a list.
#' @param data_dictionary The REDCap data dictionary export (field_name, form_name, field_type,
#'   branching_logic), or a named list of them, one per project.
#' @param study_name Letters, numbers, _ and - only.
#' @param document_id The document ID of this CSM output.
#' @param output_dir Folder to save in (created if needed; not inside a Git repository).
#' @param missing_codes REDCap missing data codes (see \code{csm_missing_codes}): a vector used for every
#'   project, or a named list, one per project.
#' @param min_site_n Fewest records a site needs for an item and event to be assessed.
#' @param z_moderate,z_large Z-score thresholds (absolute value greater than).
#' @param p_threshold Flag when the adjusted p-value is less than this.
#' @param p_adjust Multiple testing correction, one of stats::p.adjust.methods (default "BH").
#' @param mahalanobis_p Flag a record when its Mahalanobis p-value is less than this.
#' @param min_multivariate_n Fewest complete records needed for Mahalanobis and k-means.
#' @param k,nstart Number of clusters and random starts for k-means.
#' @param cluster_prop_threshold Flag a site when this share of its records or more is in one cluster.
#' @param similarity_threshold,min_comparable_fields Similar records: flag a pair when the share of
#'   matching values is this or more, across at least this many fields that both records have.
#' @param missing_threshold Flag a field when this share of its values or more is missing.
#' @param seed Random seed for k-means and digit preference tests, so results can be repeated.
#' @param script Name and version of the script that ran this, for the cover sheet.
#' @param export_time When the data were exported, for the cover sheet and file name.
#' @return Invisibly, a list with one data frame per sheet and \code{file}.
#' @export
csm_review <- function(data, metadata, data_dictionary, study_name, document_id = NULL,
                       output_dir = file.path(path.expand("~"), "REDCap_CSM", study_name),
                       missing_codes = character(0), min_site_n = 5, z_moderate = 2, z_large = 3,
                       p_threshold = 0.05, p_adjust = "BH", mahalanobis_p = 0.001, min_multivariate_n = 10,
                       k = 2, nstart = 25, cluster_prop_threshold = 0.8, similarity_threshold = 0.8,
                       min_comparable_fields = 3, missing_threshold = 0.10, seed = 123,
                       script = NULL, export_time = NULL) {

  version <- "1.0.0"
  site_col <- "redcap_data_access_group"
  no_site <- "(no site)"
  blank <- function(x) is.na(x) | trimws(as.character(x)) == ""
  one_text <- function(x) length(x) == 1 && is.character(x) && !is.na(x)
  one_num <- function(x, lo = -Inf, hi = Inf) is.numeric(x) && length(x) == 1 && !is.na(x) && x >= lo && x <= hi

  # ---- 1. Check settings and inputs (nothing is saved before this passes) ----
  if (!one_text(study_name) || !grepl("^[A-Za-z0-9_-]+$", study_name)) stop("study_name must contain only letters, numbers, _ and -.")
  if (!is.null(document_id) && !one_text(document_id)) stop("document_id must be a single piece of text, or left out.")
  if (!is.null(script) && !one_text(script)) stop("script must be a single piece of text, or left out.")
  if (!is.null(export_time) && !inherits(export_time, "POSIXct")) stop("export_time must be a date-time, e.g. Sys.time(), or left out.")
  checks <- list(min_site_n = one_num(min_site_n, 2), z_moderate = one_num(z_moderate, 0), z_large = one_num(z_large, 0),
                 p_threshold = one_num(p_threshold, 0, 1), mahalanobis_p = one_num(mahalanobis_p, 0, 1),
                 min_multivariate_n = one_num(min_multivariate_n, 3), k = one_num(k, 2), nstart = one_num(nstart, 1),
                 cluster_prop_threshold = one_num(cluster_prop_threshold, 0, 1),
                 similarity_threshold = one_num(similarity_threshold, 0, 1),
                 min_comparable_fields = one_num(min_comparable_fields, 1), missing_threshold = one_num(missing_threshold, 0, 1),
                 seed = one_num(seed))
  bad <- names(checks)[!unlist(checks)]
  if (length(bad)) stop("These settings are not valid numbers (see the help for the allowed range): ", paste(bad, collapse = ", "))
  if (z_large < z_moderate) stop("z_large must be the same as or bigger than z_moderate.")
  if (!one_text(p_adjust) || !p_adjust %in% stats::p.adjust.methods) {
    stop("p_adjust must be one of: ", paste(stats::p.adjust.methods, collapse = ", "))
  }

  if (is.character(metadata) && length(metadata) == 1) {
    if (!file.exists(metadata)) stop("Critical data items file not found: ", metadata)
    meta_source <- basename(metadata)
    metadata <- utils::read.csv(metadata, colClasses = "character", check.names = FALSE, fileEncoding = "UTF-8-BOM")
  } else if (is.data.frame(metadata)) {
    meta_source <- "data frame"
  } else stop("metadata must be the critical data items data frame or the path to its CSV file.")
  metadata <- as.data.frame(lapply(metadata, as.character), stringsAsFactors = FALSE, check.names = FALSE)
  meta_need <- c("field_name", "variable_type", "required_yn", "event_names", "multivariate_yn", "multivariate_group")
  if (!all(meta_need %in% names(metadata))) {
    stop("The critical data items are missing column(s): ", paste(setdiff(meta_need, names(metadata)), collapse = ", "))
  }

  single <- is.data.frame(data)
  if (single) {
    data <- list(data)
    names(data) <- if ("project" %in% names(metadata) && length(unique(trimws(metadata$project[!blank(metadata$project)]))) == 1) {
      unique(trimws(metadata$project[!blank(metadata$project)]))
    } else "main"
    metadata$project <- names(data)
  } else {
    if (!is.list(data) || is.null(names(data)) || any(names(data) == "") || !all(vapply(data, is.data.frame, TRUE))) {
      stop("data must be a data frame, or a named list of data frames (one per project).")
    }
    if (!"project" %in% names(metadata)) {
      if (length(data) > 1) stop("data has more than one project, so the critical data items need a project column.")
      metadata$project <- names(data)
    }
  }
  metadata$project <- trimws(metadata$project)
  projects <- names(data)

  if (is.data.frame(data_dictionary)) {
    if (length(projects) > 1) stop("data has more than one project, so data_dictionary must be a named list, one per project.")
    data_dictionary <- stats::setNames(list(data_dictionary), projects)
  }
  if (!is.list(data_dictionary) || !all(projects %in% names(data_dictionary))) {
    stop("data_dictionary must be given for every project: ", paste(projects, collapse = ", "))
  }
  for (p in projects) {
    dd <- data_dictionary[[p]]
    if (!is.data.frame(dd) || !all(c("field_name", "form_name", "field_type", "branching_logic") %in% names(dd))) {
      stop("The data dictionary for ", p, " must have field_name, form_name, field_type and branching_logic.")
    }
    if (ncol(data[[p]]) < 2) stop("The export for ", p, " has fewer than two columns.")
  }
  if (is.list(missing_codes)) {
    if (!all(projects %in% names(missing_codes))) stop("missing_codes is a list, so it needs an entry for every project.")
  } else {
    if (!is.character(missing_codes)) stop("missing_codes must be text, e.g. csm_missing_codes(\"NI, No information\").")
    missing_codes <- stats::setNames(rep(list(missing_codes), length(projects)), projects)
  }

  # Critical data items: required only, one row per project, field and event
  metadata$required_yn <- tolower(trimws(metadata$required_yn))
  metadata$variable_type <- tolower(trimws(metadata$variable_type))
  metadata$field_name <- trimws(metadata$field_name)
  req <- metadata[metadata$required_yn == "y" & !blank(metadata$field_name), , drop = FALSE]
  if (nrow(req) == 0) stop("No critical data items have required_yn = y.")
  split_list <- function(x) { v <- trimws(unlist(strsplit(ifelse(is.na(x), "", x), ";"))); v[v != ""] }

  # ---- 2. Output folder: created if needed, never inside a Git repository ----
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(output_dir)) stop("Could not create output_dir: ", output_dir)
  out_abs <- normalizePath(output_dir, winslash = "/")
  d <- out_abs
  repeat {
    if (file.exists(file.path(d, ".git"))) stop("output_dir is inside a Git repository (", d, "). Save reports outside any repository.")
    pd <- dirname(d); if (pd == d) break; d <- pd
  }

  # ---- 3. Helpers ----
  not_assessed <- list()
  na_add <- function(analysis, project, event, item, site, reason) {
    not_assessed[[length(not_assessed) + 1]] <<- data.frame(analysis = analysis, project = project, event = event, item = item,
                                                            site = site, reason = reason, stringsAsFactors = FALSE)
  }
  num_info <- function(x, codes) {
    x <- trimws(as.character(x))
    is_blank <- is.na(x) | x == ""
    is_code <- !is_blank & x %in% codes
    val <- suppressWarnings(as.numeric(x))
    val[is_blank | is_code] <- NA_real_
    list(value = val, text = ifelse(is_blank | is_code | is.na(val), NA_character_, x),
         n_not_numeric = sum(!is_blank & !is_code & is.na(val)))
  }
  prep <- function(p) {
    df <- as.data.frame(lapply(data[[p]], as.character), stringsAsFactors = FALSE, check.names = FALSE)
    s <- if (site_col %in% names(df)) df[[site_col]] else rep(NA_character_, nrow(df))
    s[blank(s)] <- no_site
    list(df = df, record = df[[1]], site = s,
         event = if ("redcap_event_name" %in% names(df)) df$redcap_event_name else rep("(no events)", nrow(df)),
         instrument = if ("redcap_repeat_instrument" %in% names(df)) df$redcap_repeat_instrument else rep(NA_character_, nrow(df)),
         instance = if ("redcap_repeat_instance" %in% names(df)) df$redcap_repeat_instance else rep(NA_character_, nrow(df)),
         longitudinal = "redcap_event_name" %in% names(df))
  }
  P <- lapply(stats::setNames(projects, projects), prep)
  events_for <- function(p, ev) if (P[[p]]$longitudinal) split_list(ev) else "(no events)"
  real_sites <- function(s) sort(unique(s[s != no_site]))
  n_rec <- function(rec) length(unique(rec))

  # Numeric items (continuous and derived), one row per project, event and field
  num_items <- req[req$variable_type %in% c("continuous", "derived"), , drop = FALSE]
  units <- list()
  for (i in seq_len(nrow(num_items))) {
    p <- num_items$project[i]; f <- num_items$field_name[i]
    if (!p %in% projects) { na_add("All", p, "", f, "", "Project not in the data"); next }
    if (!f %in% names(P[[p]]$df)) { na_add("All", p, "", f, "", "Field not in the export"); next }
    for (e in events_for(p, num_items$event_names[i])) {
      key <- paste(p, e, f, sep = "\r")
      if (!is.null(units[[key]])) next
      rows <- which(P[[p]]$event == e)
      if (length(rows) == 0) { na_add("All", p, e, f, "", "No records at this event yet"); next }
      ni <- num_info(P[[p]]$df[[f]][rows], missing_codes[[p]])
      if (ni$n_not_numeric > 0) na_add("All", p, e, f, "", paste0(ni$n_not_numeric, " value(s) not numbers and not missing data codes - left out"))
      keep <- !is.na(ni$value)
      units[[key]] <- data.frame(record = P[[p]]$record[rows][keep], site = P[[p]]$site[rows][keep],
                                 instance = P[[p]]$instance[rows][keep], value = ni$value[keep], text = ni$text[keep],
                                 stringsAsFactors = FALSE)
      attr(units[[key]], "id") <- c(project = p, event = e, field = f, n_missing = sum(!keep))
    }
  }
  uid <- function(u) attr(u, "id")

  # ---- 4. Overall and site distribution summaries ----
  stats_row <- function(v) {
    n <- length(v)
    q <- if (n) stats::quantile(v, c(0.25, 0.75), names = FALSE) else c(NA, NA)
    data.frame(n = n, mean = if (n) mean(v) else NA, sd = if (n > 1) stats::sd(v) else NA,
               median = if (n) stats::median(v) else NA, q1 = q[1], q3 = q[2], IQR = q[2] - q[1],
               min = if (n) min(v) else NA, max = if (n) max(v) else NA)
  }
  overall <- do.call(rbind, lapply(units, function(u) {
    id <- uid(u); s <- stats_row(u$value)
    ratio <- if (!is.na(s$sd) && s$mean != 0) s$sd / abs(s$mean) else NA
    diff <- if (!is.na(s$mean) && s$mean != 0) abs(s$median - s$mean) / abs(s$mean) else NA
    cbind(data.frame(project = id[["project"]], event = id[["event"]], variable = id[["field"]],
                     n_missing = as.integer(id[["n_missing"]]), stringsAsFactors = FALSE), s,
          data.frame(sd_mean_ratio = round(ratio, 3), mean_median_difference = round(diff, 3),
                     spread_check = ifelse(is.na(ratio), "", ifelse(ratio < 0.05 | ratio > 1, "red",
                                                                     ifelse(ratio < 0.10 | ratio > 0.75, "amber", ""))),
                     skew_check = ifelse(is.na(diff), "", ifelse(diff > 0.5, "red", ifelse(diff > 0.25, "amber", ""))),
                     stringsAsFactors = FALSE))
  }))
  site_summary <- do.call(rbind, lapply(units, function(u) {
    id <- uid(u)
    do.call(rbind, lapply(sort(unique(u$site)), function(s) {
      w <- u$site == s; st <- stats_row(u$value[w]); nr <- n_rec(u$record[w])
      assessed <- s != no_site && nr >= min_site_n
      cbind(data.frame(project = id[["project"]], event = id[["event"]], variable = id[["field"]], site = s,
                       n_records = nr, stringsAsFactors = FALSE), st,
            data.frame(assessed = if (s == no_site) "No - no site" else if (assessed) "Yes" else "Too few to assess",
                       no_variation = assessed && st$n > 1 && !is.na(st$sd) && st$sd == 0, stringsAsFactors = FALSE))
    }))
  }))
  for (u in units) {
    id <- uid(u)
    for (s in real_sites(u$site)) {
      nr <- n_rec(u$record[u$site == s])
      if (nr < min_site_n) na_add("Site comparisons", id[["project"]], id[["event"]], id[["field"]], s,
                                  paste0("Too few to assess (", nr, " record(s); minimum ", min_site_n, ")"))
    }
    if (any(u$site == no_site)) na_add("Site comparisons", id[["project"]], id[["event"]], id[["field"]], no_site,
                                       paste0(n_rec(u$record[u$site == no_site]), " record(s) with no site - in the overall summary only"))
  }

  # ---- 5. Z-scores: each site against the rest of the study ----
  z_scores <- do.call(rbind, lapply(units, function(u) {
    id <- uid(u); us <- u[u$site != no_site, ]
    out <- NULL
    for (s in real_sites(us$site)) {
      a <- us$value[us$site == s]; b <- us$value[us$site != s]
      if (n_rec(us$record[us$site == s]) < min_site_n) next
      if (n_rec(us$record[us$site != s]) < min_site_n) {
        na_add("Z-scores", id[["project"]], id[["event"]], id[["field"]], s, "Too few records at the other sites to compare with"); next
      }
      se <- sqrt(stats::var(a) / length(a) + stats::var(b) / length(b))
      if (is.na(se) || se == 0) { na_add("Z-scores", id[["project"]], id[["event"]], id[["field"]], s, "No variation in the values"); next }
      z <- (mean(a) - mean(b)) / se
      out <- rbind(out, data.frame(project = id[["project"]], event = id[["event"]], variable = id[["field"]], site = s,
                                   n_site = length(a), mean_site = mean(a), n_rest = length(b), mean_rest = mean(b),
                                   se = se, z_score = round(z, 3),
                                   flag = if (abs(z) > z_large) "large" else if (abs(z) > z_moderate) "moderate" else "",
                                   stringsAsFactors = FALSE))
    }
    out
  }))

  # ---- 6. Brown-Forsythe: does variability differ between sites? ----
  bf <- do.call(rbind, lapply(units, function(u) {
    id <- uid(u)
    ok <- Filter(function(s) n_rec(u$record[u$site == s]) >= min_site_n, real_sites(u$site))
    if (length(ok) < 2) { na_add("Brown-Forsythe", id[["project"]], id[["event"]], id[["field"]], "", "Fewer than two sites with enough records"); return(NULL) }
    df <- u[u$site %in% ok, ]
    if (stats::sd(df$value) == 0) { na_add("Brown-Forsythe", id[["project"]], id[["event"]], id[["field"]], "", "No variation in the values"); return(NULL) }
    df$site <- factor(df$site)
    t <- car::leveneTest(value ~ site, data = df, center = stats::median)
    data.frame(project = id[["project"]], event = id[["event"]], variable = id[["field"]], sites = paste(ok, collapse = "; "),
               n = nrow(df), p_value = t$`Pr(>F)`[1], stringsAsFactors = FALSE)
  }))

  # ---- 7. Anderson-Darling: each site's distribution against the rest ----
  ad <- do.call(rbind, lapply(units, function(u) {
    id <- uid(u); us <- u[u$site != no_site, ]; out <- NULL
    for (s in real_sites(us$site)) {
      a <- us$value[us$site == s]; b <- us$value[us$site != s]
      if (n_rec(us$record[us$site == s]) < min_site_n) next
      if (n_rec(us$record[us$site != s]) < min_site_n) {
        na_add("Anderson-Darling", id[["project"]], id[["event"]], id[["field"]], s, "Too few records at the other sites to compare with"); next
      }
      if (length(unique(c(a, b))) < 2) { na_add("Anderson-Darling", id[["project"]], id[["event"]], id[["field"]], s, "No variation in the values"); next }
      t <- tryCatch(kSamples::ad.test(a, b), error = function(e) NULL)
      if (is.null(t)) { na_add("Anderson-Darling", id[["project"]], id[["event"]], id[["field"]], s, "Test could not be run"); next }
      out <- rbind(out, data.frame(project = id[["project"]], event = id[["event"]], variable = id[["field"]], site = s,
                                   n_site = length(a), n_rest = length(b), p_value = t$ad[1, grep("P-value", colnames(t$ad))[1]],
                                   stringsAsFactors = FALSE))
    }
    out
  }))

  # ---- 8. Multivariate groups: Mahalanobis distance and k-means ----
  mv <- req[tolower(trimws(req$multivariate_yn)) == "y" & req$variable_type %in% c("continuous", "derived"), , drop = FALSE]
  groups <- list()
  for (i in seq_len(nrow(mv))) for (g in split_list(mv$multivariate_group[i])) for (e in events_for(mv$project[i], mv$event_names[i])) {
    key <- paste(mv$project[i], e, g, sep = "\r")
    groups[[key]] <- unique(c(groups[[key]], mv$field_name[i]))
  }
  maha <- NULL; km_site <- NULL
  for (key in names(groups)) {
    parts <- strsplit(key, "\r")[[1]]; p <- parts[1]; e <- parts[2]; g <- parts[3]
    if (!p %in% projects) next
    vars <- intersect(groups[[key]], names(P[[p]]$df))
    if (length(vars) < 2) { na_add("Multivariate", p, e, g, "", "Fewer than two of the group's fields in the export"); next }
    rows <- which(P[[p]]$event == e)
    x <- sapply(vars, function(v) num_info(P[[p]]$df[[v]][rows], missing_codes[[p]])$value)
    if (!is.matrix(x)) x <- matrix(x, ncol = length(vars), dimnames = list(NULL, vars))
    cc <- stats::complete.cases(x)
    x <- x[cc, , drop = FALSE]; rec <- P[[p]]$record[rows][cc]; site <- P[[p]]$site[rows][cc]; inst <- P[[p]]$instance[rows][cc]
    if (nrow(x) < max(min_multivariate_n, length(vars) + 2)) {
      na_add("Multivariate", p, e, g, "", paste0("Too few records with every field complete (", nrow(x), ")")); next
    }
    if (any(apply(x, 2, stats::sd) == 0)) { na_add("Multivariate", p, e, g, "", "A field in the group has no variation"); next }
    cv <- stats::cov(x)
    if (rcond(cv) < 1e-10) { na_add("Mahalanobis", p, e, g, "", "The fields are too closely related to each other to compute distances") } else {
      d2 <- stats::mahalanobis(x, colMeans(x), cv)
      pv <- stats::pchisq(d2, df = length(vars), lower.tail = FALSE)
      maha <- rbind(maha, data.frame(project = p, event = e, group = g, fields = paste(vars, collapse = "; "), record = rec,
                                     instance = inst, site = site, distance_squared = round(d2, 3), p_value = signif(pv, 4),
                                     flag = pv < mahalanobis_p, stringsAsFactors = FALSE))
    }
    if (nrow(unique(x)) < k) { na_add("k-means", p, e, g, "", "Fewer different records than clusters"); next }
    set.seed(seed)
    cl <- stats::kmeans(scale(x), centers = k, nstart = nstart)$cluster
    for (s in sort(unique(site))) {
      w <- site == s; nr <- n_rec(rec[w])
      if (s == no_site) next
      if (nr < min_site_n) { na_add("k-means", p, e, g, s, paste0("Too few to assess (", nr, " record(s); minimum ", min_site_n, ")")); next }
      tab <- table(factor(cl[w], levels = seq_len(k)))
      km_site <- rbind(km_site, data.frame(project = p, event = e, group = g, site = s, n_records = sum(w),
                                           cluster = seq_len(k), n = as.integer(tab), prop = round(as.numeric(tab) / sum(w), 3),
                                           flag = as.numeric(tab) / sum(w) >= cluster_prop_threshold, stringsAsFactors = FALSE))
    }
  }

  # ---- 9. Missing data items (blanks and missing data codes, on forms that have been started) ----
  miss <- NULL
  for (i in seq_len(nrow(req))) {
    p <- req$project[i]; f <- req$field_name[i]
    if (!p %in% projects) next
    dd <- data_dictionary[[p]]; df <- P[[p]]$df
    if (!f %in% dd$field_name) { na_add("Missing data", p, "", f, "", "Field not in the data dictionary"); next }
    form <- dd$form_name[dd$field_name == f][1]
    branching <- !blank(dd$branching_logic[dd$field_name == f][1])
    is_cb <- !f %in% names(df)
    fcols <- if (is_cb) grep(paste0("^", f, "___"), names(df), value = TRUE) else f
    if (length(fcols) == 0) { na_add("Missing data", p, "", f, "", "Field not in the export"); next }
    form_fields <- dd$field_name[dd$form_name == form & dd$field_type != "descriptive"]
    form_cols <- names(df)[names(df) %in% form_fields | sub("___.*$", "", names(df)) %in% form_fields[dd$field_type[match(form_fields, dd$field_name)] == "checkbox"]]
    status_col <- paste0(form, "_complete")
    codes <- missing_codes[[p]]
    for (e in events_for(p, req$event_names[i])) {
      key <- paste(p, e, f)
      if (!is.null(miss) && any(paste(miss$project, miss$event, miss$field) == key)) next
      inst <- P[[p]]$instrument
      rows <- which(P[[p]]$event == e & (blank(inst) | inst == form))
      sub <- df[rows, , drop = FALSE]
      has_data <- if (length(form_cols)) {
        apply(sub[, form_cols, drop = FALSE], 1, function(r) {
          r <- r[!blank(r)]
          length(r) > 0 && any(!(names(r) %in% grep("___", names(r), value = TRUE)) | r == "1")
        })
      } else rep(FALSE, nrow(sub))
      started <- has_data | (status_col %in% names(sub) & sub[[status_col]] %in% c("1", "2"))
      if (!any(started)) { na_add("Missing data", p, e, f, "", "Form not started at this event yet"); next }
      sub <- sub[started, , drop = FALSE]; ssite <- P[[p]]$site[rows][started]; srec <- P[[p]]$record[rows][started]
      if (is_cb) {
        suffix <- toupper(sub("^.*___", "", fcols))
        code_cols <- fcols[suffix %in% toupper(codes)]; opt_cols <- setdiff(fcols, code_cols)
        is_code <- if (length(code_cols)) apply(sub[, code_cols, drop = FALSE] == "1", 1, any) else rep(FALSE, nrow(sub))
        code_val <- if (length(code_cols)) apply(sub[, code_cols, drop = FALSE], 1, function(r) toupper(sub("^.*___", "", code_cols))[which(r == "1")[1]]) else rep(NA, nrow(sub))
        is_blank <- !is_code & !apply(sub[, opt_cols, drop = FALSE] == "1", 1, any)
      } else {
        v <- trimws(sub[[f]])
        is_blank <- blank(v); is_code <- !is_blank & v %in% codes; code_val <- ifelse(is_code, v, NA)
      }
      is_code[is.na(is_code)] <- FALSE; is_blank[is.na(is_blank)] <- FALSE
      for (s in c("All sites", sort(unique(ssite)))) {
        w <- if (s == "All sites") rep(TRUE, length(ssite)) else ssite == s
        n <- sum(w); nb <- sum(is_blank[w]); nc <- sum(is_code[w]); nr <- n_rec(srec[w])
        ct <- table(code_val[w & is_code])
        prop_flag <- ((if (branching) 0 else nb) + nc) / n
        assessed <- nr >= min_site_n && s != no_site
        miss <- rbind(miss, data.frame(
          project = p, event = e, field = f, form = form, site = s, n_expected = n, n_blank = nb, n_missing_code = nc,
          missing_codes = if (length(ct)) paste(paste0(names(ct), " = ", as.integer(ct)), collapse = "; ") else "",
          prop_missing = round((nb + nc) / n, 3), has_branching_logic = branching,
          assessed = if (s == no_site) "No - no site" else if (assessed) "Yes" else "Too few to assess",
          flag = assessed && prop_flag >= missing_threshold, stringsAsFactors = FALSE))
        if (!assessed && s != no_site) na_add("Missing data", p, e, f, s, paste0("Too few to assess (", nr, " record(s); minimum ", min_site_n, ")"))
      }
    }
  }
  ranking <- if (is.null(miss)) NULL else {
    a <- miss[miss$site == "All sites", ]
    r <- do.call(rbind, lapply(split(seq_len(nrow(miss)), paste(miss$project, miss$field, sep = "\r")), function(ix) {
      m <- miss[ix, ]; al <- m[m$site == "All sites", ]
      data.frame(project = m$project[1], field = m$field[1], form = m$form[1], n_events = nrow(al),
                 n_expected = sum(al$n_expected), n_blank = sum(al$n_blank), n_missing_code = sum(al$n_missing_code),
                 prop_missing = round((sum(al$n_blank) + sum(al$n_missing_code)) / sum(al$n_expected), 3),
                 n_flags = sum(m$flag), flagged_at = paste(unique(paste0(m$event[m$flag], " (", m$site[m$flag], ")")), collapse = "; "),
                 has_branching_logic = m$has_branching_logic[1], stringsAsFactors = FALSE)
    }))
    r$flagged_at[r$n_flags == 0] <- ""
    r <- r[order(-r$n_flags, -r$prop_missing, r$field), ]
    rownames(r) <- NULL; r
  }

  # ---- 10. Similar records: same event, different records ----
  sim <- NULL
  for (p in projects) {
    rp <- req[req$project == p, , drop = FALSE]
    ev_fields <- list()
    for (i in seq_len(nrow(rp))) for (e in events_for(p, rp$event_names[i])) ev_fields[[e]] <- unique(c(ev_fields[[e]], rp$field_name[i]))
    df <- P[[p]]$df
    for (e in names(ev_fields)) {
      fields <- ev_fields[[e]]
      cols <- fields[fields %in% names(df) | vapply(fields, function(f) any(grepl(paste0("^", f, "___"), names(df))), TRUE)]
      rows <- which(P[[p]]$event == e)
      if (length(rows) < 2 || length(cols) < min_comparable_fields) {
        if (length(rows) >= 2) na_add("Similar records", p, e, "", "", paste0("Fewer than ", min_comparable_fields, " fields to compare"))
        next
      }
      vals <- sapply(cols, function(f) {
        if (f %in% names(df)) {
          x <- trimws(df[[f]][rows]); x[blank(x) | x %in% missing_codes[[p]]] <- NA
          nx <- suppressWarnings(as.numeric(x)); ifelse(!is.na(nx), format(round(nx, 1), nsmall = 1, trim = TRUE), x)
        } else {
          cc <- grep(paste0("^", f, "___"), names(df), value = TRUE)
          cc <- cc[!toupper(sub("^.*___", "", cc)) %in% toupper(missing_codes[[p]])]
          x <- apply(df[rows, cc, drop = FALSE], 1, function(r) paste(sub("^.*___", "", cc)[r %in% "1"], collapse = ","))
          x[x == ""] <- NA; x
        }
      })
      if (!is.matrix(vals)) vals <- matrix(vals, nrow = length(rows))
      n <- length(rows); comp <- matrix(0L, n, n); same <- matrix(0L, n, n)
      for (j in seq_len(ncol(vals))) {
        ok <- !is.na(vals[, j])
        cmp <- outer(ok, ok, "&")
        comp <- comp + cmp
        same <- same + (cmp & outer(vals[, j], vals[, j], "=="))
      }
      rec <- P[[p]]$record[rows]
      ij <- which(upper.tri(comp) & comp >= min_comparable_fields & outer(rec, rec, "!=") & same / pmax(comp, 1) >= similarity_threshold, arr.ind = TRUE)
      for (r in seq_len(nrow(ij))) {
        a <- ij[r, 1]; b <- ij[r, 2]
        okb <- !is.na(vals[a, ]) & !is.na(vals[b, ]); m <- okb & vals[a, ] == vals[b, ]
        sim <- rbind(sim, data.frame(project = p, event = e, record_1 = rec[a], instance_1 = P[[p]]$instance[rows][a],
                                     site_1 = P[[p]]$site[rows][a], record_2 = rec[b], instance_2 = P[[p]]$instance[rows][b],
                                     site_2 = P[[p]]$site[rows][b], comparable_fields = comp[a, b], matching_fields = same[a, b],
                                     similarity = round(same[a, b] / comp[a, b], 3),
                                     matching_field_names = paste(cols[m], collapse = "; "),
                                     non_matching_field_names = paste(cols[okb & !m], collapse = "; "), stringsAsFactors = FALSE))
      }
    }
  }
  if (!is.null(sim)) sim <- sim[order(-sim$similarity, -sim$comparable_fields), ]

  # ---- 11. Digit preference: decimal places and last digit, each site against the rest ----
  digits <- do.call(rbind, lapply(units, function(u) {
    id <- uid(u); us <- u[u$site != no_site, ]; out <- NULL
    dp <- ifelse(grepl("\\.", us$text), nchar(sub("^[^.]*\\.", "", us$text)), 0L)
    last <- substr(us$text, nchar(us$text), nchar(us$text))
    for (s in real_sites(us$site)) {
      w <- us$site == s
      if (n_rec(us$record[w]) < min_site_n) next
      if (n_rec(us$record[!w]) < min_site_n) {
        na_add("Digit preference", id[["project"]], id[["event"]], id[["field"]], s, "Too few records at the other sites to compare with"); next
      }
      for (pat in c("decimal places", "last digit")) {
        x <- if (pat == "decimal places") dp else last
        lv <- sort(unique(x)); tab <- rbind(site = table(factor(x[w], lv)), rest = table(factor(x[!w], lv)))
        pv <- if (length(lv) < 2) 1 else if (length(lv) == 2) stats::fisher.test(tab)$p.value else {
          set.seed(seed); stats::fisher.test(tab, simulate.p.value = TRUE, B = 10000)$p.value
        }
        top <- as.character(lv[which.max(tab["site", ])])
        dist <- function(r) paste(paste0(lv, ": ", round(100 * tab[r, ] / sum(tab[r, ])), "%"), collapse = "; ")
        out <- rbind(out, data.frame(project = id[["project"]], event = id[["event"]], variable = id[["field"]], site = s,
                                     pattern = pat, n_site = sum(w), n_rest = sum(!w), site_distribution = dist("site"),
                                     rest_distribution = dist("rest"), most_common_at_site = top,
                                     share_at_site = round(max(tab["site", ]) / sum(w), 3), p_value = pv, stringsAsFactors = FALSE))
      }
    }
    out
  }))

  # ---- 12. Multiple testing correction and flags ----
  adjust <- function(df) {
    if (is.null(df) || nrow(df) == 0) return(df)
    df$p_adjusted <- stats::p.adjust(df$p_value, method = p_adjust)
    df$flag <- df$p_adjusted < p_threshold
    df$p_value <- signif(df$p_value, 4); df$p_adjusted <- signif(df$p_adjusted, 4)
    df[order(df$p_adjusted), ]
  }
  bf <- adjust(bf); ad <- adjust(ad); digits <- adjust(digits)

  empty <- function(cols) as.data.frame(stats::setNames(replicate(length(cols), character(0), simplify = FALSE), cols))
  fl <- list()
  addf <- function(df, analysis, item_col, site_fun, detail_fun) {
    if (is.null(df) || nrow(df) == 0) return()
    fl[[length(fl) + 1]] <<- data.frame(analysis = analysis, project = df$project, event = df$event, item = df[[item_col]],
                                        site = site_fun(df), detail = detail_fun(df), stringsAsFactors = FALSE)
  }
  pick <- function(df, keep) if (is.null(df) || nrow(df) == 0) NULL else df[keep(df), , drop = FALSE]
  addf(pick(site_summary, function(d) d$no_variation), "Site distribution", "variable", function(d) d$site,
       function(d) paste0("All ", d$n, " values are the same (", d$mean, ")"))
  addf(pick(z_scores, function(d) d$flag != ""), "Z-scores", "variable", function(d) d$site,
       function(d) paste0(d$flag, ": z = ", d$z_score, " (site mean ", signif(d$mean_site, 4), ", rest ", signif(d$mean_rest, 4), ")"))
  addf(pick(bf, function(d) d$flag), "Brown-Forsythe", "variable", function(d) d$sites,
       function(d) paste0("Variability differs between sites (adjusted p = ", d$p_adjusted, ")"))
  addf(pick(ad, function(d) d$flag), "Anderson-Darling", "variable", function(d) d$site,
       function(d) paste0("Distribution differs from the rest (adjusted p = ", d$p_adjusted, ")"))
  addf(pick(maha, function(d) d$flag), "Mahalanobis", "group", function(d) d$site,
       function(d) paste0("Record ", d$record, ifelse(blank(d$instance), "", paste0(" (instance ", d$instance, ")")),
                          ": unusual combination of values (p = ", d$p_value, ")"))
  addf(pick(km_site, function(d) d$flag), "k-means", "group", function(d) d$site,
       function(d) paste0(round(100 * d$prop), "% of the site's records in cluster ", d$cluster))
  addf(pick(miss, function(d) d$flag), "Missing data", "field", function(d) d$site,
       function(d) ifelse(d$has_branching_logic,
                          paste0(d$n_missing_code, " of ", d$n_expected, " are missing data codes (",
                                 round(100 * d$n_missing_code / d$n_expected), "%); ", d$n_blank, " blank not counted (branching logic)"),
                          paste0(round(100 * d$prop_missing), "% missing (", d$n_blank, " blank, ", d$n_missing_code,
                                 " missing data code) of ", d$n_expected)))
  addf(sim, "Similar records", "event", function(d) paste(d$site_1, d$site_2, sep = " / "),
       function(d) paste0("Records ", d$record_1, " and ", d$record_2, ": ", d$matching_fields, " of ", d$comparable_fields, " values match"))
  addf(pick(digits, function(d) d$flag), "Digit preference", "variable", function(d) d$site,
       function(d) paste0(d$pattern, ": ", round(100 * d$share_at_site), "% are '", d$most_common_at_site, "' (adjusted p = ", d$p_adjusted, ")"))
  flags <- if (length(fl)) do.call(rbind, fl) else empty(c("analysis", "project", "event", "item", "site", "detail"))
  if (!is.null(sim) && nrow(sim)) flags$item[flags$analysis == "Similar records"] <- ""
  not_assessed <- if (length(not_assessed)) unique(do.call(rbind, not_assessed)) else empty(c("analysis", "project", "event", "item", "site", "reason"))
  rownames(flags) <- NULL; rownames(not_assessed) <- NULL

  nz <- function(x, cols) if (is.null(x)) empty(cols) else { rownames(x) <- NULL; x }
  res <- list(
    flags = flags, not_assessed = not_assessed,
    overall = nz(overall, c("project", "event", "variable")), site_summary = nz(site_summary, c("project", "event", "variable", "site")),
    z_scores = nz(z_scores, c("project", "event", "variable", "site", "z_score", "flag")),
    brown_forsythe = nz(bf, c("project", "event", "variable", "p_value", "p_adjusted", "flag")),
    anderson_darling = nz(ad, c("project", "event", "variable", "site", "p_value", "p_adjusted", "flag")),
    mahalanobis = nz(maha, c("project", "event", "group", "record", "site", "p_value", "flag")),
    kmeans = nz(km_site, c("project", "event", "group", "site", "cluster", "prop", "flag")),
    missing_data = nz(miss, c("project", "event", "field", "site", "prop_missing", "flag")),
    missing_ranking = nz(ranking, c("project", "field", "n_flags")),
    similar_records = nz(sim, c("project", "event", "record_1", "record_2", "similarity")),
    digit_preference = nz(digits, c("project", "event", "variable", "site", "pattern", "p_value", "p_adjusted", "flag")))

  # ---- 13. Workbook ----
  if (is.null(export_time)) export_time_txt <- "(not given)" else export_time_txt <- format(export_time, "%d %b %Y %H:%M")
  pkg_v <- function(p) tryCatch(as.character(utils::packageVersion(p)), error = function(e) "not installed")
  wb <- openxlsx::createWorkbook()
  title <- openxlsx::createStyle(fontSize = 14, textDecoration = "bold", fgFill = "#83CCEB", border = "TopBottomLeftRight")
  head <- openxlsx::createStyle(textDecoration = "bold", fgFill = "#D9D9D9", border = "TopBottomLeftRight", valign = "top", wrapText = TRUE)
  cell <- openxlsx::createStyle(border = "TopBottomLeftRight", valign = "top", wrapText = TRUE)
  example <- openxlsx::createStyle(fontColour = "#FF0000", border = "TopBottomLeftRight", valign = "top", wrapText = TRUE)
  red <- openxlsx::createStyle(fgFill = "#FFC7CE", fontColour = "#9C0006")
  amber <- openxlsx::createStyle(fgFill = "#FFEB9C", fontColour = "#9C6500")
  codes_txt <- paste(vapply(projects, function(p) paste0(p, ": ", if (length(missing_codes[[p]])) paste(missing_codes[[p]], collapse = ", ") else "none"), ""), collapse = "; ")
  cover <- data.frame(Item = c(
    "Report", "Document ID", "Study", "Project(s)", "Data exported", "Report saved by", "Script", "Function", "R and packages",
    "Critical data items", "Items used", "Missing data codes", "Minimum records per site", "Z-score flags",
    "p-value flags", "Mahalanobis flag", "k-means", "Similar records", "Missing data flag", "Random seed",
    "Flags for review", "Not assessed"),
    Value = c(
      "Central statistical monitoring (CSM)", if (is.null(document_id)) "" else document_id, study_name,
      paste(projects, collapse = "; "), export_time_txt, Sys.info()[["user"]], if (is.null(script)) "(not given)" else script,
      paste0("csm_review V", version),
      paste0("R ", R.version$major, ".", R.version$minor, "; car ", pkg_v("car"), "; kSamples ", pkg_v("kSamples"), "; openxlsx ", pkg_v("openxlsx")),
      paste0(meta_source, ": ", nrow(metadata), " item(s)"),
      paste0(nrow(req), " with required_yn = y; each analysed separately for each of its events"),
      codes_txt, paste0(min_site_n, " (sites with fewer are 'too few to assess')"),
      paste0("Each site against the rest of the study, using the standard error. Moderate |z| > ", z_moderate, "; large |z| > ", z_large),
      paste0("Brown-Forsythe, Anderson-Darling and digit preference: adjusted p < ", p_threshold, " (", p_adjust, " correction within each analysis)"),
      paste0("p < ", mahalanobis_p, " (chi-squared, degrees of freedom = number of fields; at least ", min_multivariate_n, " complete records)"),
      paste0("k = ", k, ", ", nstart, " starts; flag when ", round(100 * cluster_prop_threshold), "% or more of a site's records are in one cluster"),
      paste0("Same event, different records; flag when ", round(100 * similarity_threshold), "% or more of at least ", min_comparable_fields, " shared fields match"),
      paste0(round(100 * missing_threshold), "% or more missing (blank or missing data code), on forms that have been started; blanks in fields with branching logic are shown but not flagged"),
      as.character(seed), as.character(nrow(flags)), as.character(nrow(not_assessed))),
    stringsAsFactors = FALSE)
  openxlsx::addWorksheet(wb, "Cover")
  openxlsx::writeData(wb, "Cover", paste0(study_name, " central statistical monitoring"), startRow = 1)
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
    a = c("e.g. Site 003 had higher haemoglobin values than the other sites at baseline (z-score, large).",
          "e.g. Records 01002 and 01018 were 96% similar at baseline (similar records)."),
    b = c("e.g. Flag referred to the statistics team. Source data verification for the affected records found no transcription errors.",
          "e.g. The data team checked source documents with the site and confirmed they are two different participants."),
    c = c("e.g. Haemoglobin at the site will be looked at again at the next CSM review.",
          "e.g. No further action.")), startRow = fr + 2, colNames = FALSE)
  openxlsx::addStyle(wb, "Cover", example, rows = (fr + 2):(fr + 3), cols = 1:3, gridExpand = TRUE)
  openxlsx::addStyle(wb, "Cover", cell, rows = (fr + 4):(fr + 8), cols = 1:3, gridExpand = TRUE)
  openxlsx::setColWidths(wb, "Cover", cols = 1:3, widths = c(30, 80, 50))

  sheet <- function(name, df, flag_col = NULL, note = "None found.") {
    openxlsx::addWorksheet(wb, name)
    if (nrow(df) == 0) {
      openxlsx::writeData(wb, name, note, startRow = 1)
      openxlsx::writeData(wb, name, df, startRow = 3, headerStyle = head)
      return(invisible())
    }
    openxlsx::writeData(wb, name, df, headerStyle = head)
    openxlsx::freezePane(wb, name, firstRow = TRUE)
    openxlsx::addFilter(wb, name, rows = 1, cols = seq_along(df))
    openxlsx::setColWidths(wb, name, cols = seq_along(df), widths = "auto")
    if (!is.null(flag_col) && flag_col %in% names(df)) {
      v <- df[[flag_col]]
      r <- which(v %in% c(TRUE, "large", "red")) + 1; a <- which(v %in% c("moderate", "amber")) + 1
      if (length(r)) openxlsx::addStyle(wb, name, red, rows = r, cols = seq_along(df), gridExpand = TRUE)
      if (length(a)) openxlsx::addStyle(wb, name, amber, rows = a, cols = seq_along(df), gridExpand = TRUE)
    }
  }
  sheet("Flags for Review", res$flags, note = "Nothing was flagged.")
  sheet("Not Assessed", res$not_assessed, note = "Everything was assessed.")
  sheet("Overall Distribution", res$overall)
  if (nrow(res$overall)) {
    for (cc in c("spread_check", "skew_check")) {
      col <- which(names(res$overall) == cc)
      r <- which(res$overall[[cc]] == "red") + 1; a <- which(res$overall[[cc]] == "amber") + 1
      if (length(r)) openxlsx::addStyle(wb, "Overall Distribution", red, rows = r, cols = col)
      if (length(a)) openxlsx::addStyle(wb, "Overall Distribution", amber, rows = a, cols = col)
    }
  }
  sheet("Site Distribution", res$site_summary, "no_variation")
  sheet("Z-Scores", res$z_scores, "flag")
  sheet("Brown-Forsythe", res$brown_forsythe, "flag")
  sheet("Anderson-Darling", res$anderson_darling, "flag")
  sheet("Mahalanobis", res$mahalanobis, "flag")
  sheet("K-Means", res$kmeans, "flag")
  sheet("Missing Data", res$missing_data, "flag")
  sheet("Missing Data Ranking", res$missing_ranking)
  sheet("Similar Records", res$similar_records)
  sheet("Digit Preference", res$digit_preference, "flag")

  stamp <- if (is.null(export_time)) Sys.time() else export_time
  file <- file.path(out_abs, paste0(study_name, "_CSMOutput_", format(stamp, "%Y-%m-%d_%H%M"), ".xlsx"))
  openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
  message("CSM review saved: ", file, "\n", nrow(flags), " flag(s) for review; ", nrow(not_assessed), " item(s) not assessed.")
  res$file <- file
  invisible(res)
}

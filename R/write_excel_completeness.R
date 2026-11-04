#Title: write_excel_completeness (with completeness_thresholds and completeness_ratings)
#Version & Date: V2.0.0 03NOV2026
#Author: Paigan Aspinall
#R version: 4.4.3

#' Red/amber/green thresholds for the data completeness report
#'
#' Proportions (0 to 1). Complete: green at or above \code{complete_green}, amber
#' at or above \code{complete_amber}, otherwise red. Partially complete and
#' incomplete: green at or below \code{*_green}, amber at or below \code{*_amber},
#' otherwise red.
#' @export
completeness_thresholds <- function(complete_green = 0.85, complete_amber = 0.75,
                                    partial_green = 0.15, partial_amber = 0.25,
                                    incomplete_green = 0.15, incomplete_amber = 0.25) {
  v <- c(complete_green = complete_green, complete_amber = complete_amber, partial_green = partial_green,
         partial_amber = partial_amber, incomplete_green = incomplete_green, incomplete_amber = incomplete_amber)
  if (any(!is.finite(v)) || any(v < 0 | v > 1)) stop("Thresholds must be proportions between 0 and 1 (e.g. 0.85 for 85%).")
  if (complete_amber > complete_green) stop("complete_amber must not be higher than complete_green.")
  if (partial_green > partial_amber) stop("partial_green must not be higher than partial_amber.")
  if (incomplete_green > incomplete_amber) stop("incomplete_green must not be higher than incomplete_amber.")
  structure(as.list(v), class = "completeness_thresholds")
}

#' Counts, percentages and ratings for each form at each site
#'
#' One row per event, form and site, plus an "All forms" row per event and site.
#' Percentages use the exact proportion (not rounded) when rated.
#' @export
completeness_ratings <- function(completeness, thresholds = completeness_thresholds()) {
  if (!inherits(completeness, "completeness_data")) stop("completeness must come from completeness_tracker_data_prep().")
  if (!inherits(thresholds, "completeness_thresholds")) stop("thresholds must come from completeness_thresholds().")
  tol <- 1e-9
  rate_high <- function(p, green, amber) ifelse(is.na(p), NA, ifelse(p >= green - tol, "Green", ifelse(p >= amber - tol, "Amber", "Red")))
  rate_low  <- function(p, green, amber) ifelse(is.na(p), NA, ifelse(p <= green + tol, "Green", ifelse(p <= amber + tol, "Amber", "Red")))
  x <- completeness$counts
  if (nrow(x) == 0) return(data.frame())
  key <- paste(x$event, x$form, x$site, sep = "\r")
  w <- data.frame(event = tapply(x$event, key, `[`, 1), form = tapply(x$form, key, `[`, 1),
                  site = tapply(x$site, key, `[`, 1),
                  complete = tapply(x$n * (x$status == "Complete"), key, sum),
                  partial = tapply(x$n * (x$status == "Partially complete"), key, sum),
                  incomplete = tapply(x$n * (x$status == "Incomplete"), key, sum), stringsAsFactors = FALSE)
  allf <- stats::aggregate(cbind(complete, partial, incomplete) ~ event + site, data = w, FUN = sum)
  allf$form <- "All forms"
  w <- rbind(w[, names(allf)], allf[, names(allf)])
  w$instances <- w$complete + w$partial + w$incomplete
  pct <- function(n) ifelse(w$instances > 0, n / w$instances, NA)
  w$pct_complete <- pct(w$complete); w$pct_partial <- pct(w$partial); w$pct_incomplete <- pct(w$incomplete)
  t <- thresholds
  w$rating_complete <- rate_high(w$pct_complete, t$complete_green, t$complete_amber)
  w$rating_partial <- rate_low(w$pct_partial, t$partial_green, t$partial_amber)
  w$rating_incomplete <- rate_low(w$pct_incomplete, t$incomplete_green, t$incomplete_amber)
  # order: events, then forms in the report's order with "All forms" last, then sites
  ev_ord <- match(w$event, completeness$settings$events)
  f_ord <- mapply(function(e, f) if (f == "All forms") 1e6 else match(f, completeness$forms_by_event[[e]]), w$event, w$form)
  w <- w[order(ev_ord, f_ord, match(w$site, completeness$sites)), ]
  rownames(w) <- NULL
  w[, c("event", "form", "site", "instances", "complete", "partial", "incomplete",
        "pct_complete", "pct_partial", "pct_incomplete", "rating_complete", "rating_partial", "rating_incomplete")]
}

#' Write the data completeness workbook
#'
#' Saves one Excel workbook: a cover sheet, then one sheet per event showing, for
#' each form and site, the number of instances and the number and percentage
#' complete, partially complete and incomplete, coloured red/amber/green.
#'
#' @param completeness Output of \code{completeness_tracker_data_prep()}.
#' @param study_name Study name for the file name and cover sheet (letters,
#'   numbers, _ and - only).
#' @param output_dir Folder to save in (created if needed). Must not be inside a
#'   Git repository.
#' @param thresholds Output of \code{completeness_thresholds()}.
#' @param script Name and version of the script that ran this, shown on the cover
#'   sheet (e.g. "TEMPLATE_DataCompleteness V2.0.0").
#' @return Invisibly, a list with the file path and the ratings table.
#' @export
write_excel_completeness <- function(completeness, study_name, output_dir,
                                     thresholds = completeness_thresholds(), script = "") {
  version <- "2.0.0"
  if (!inherits(completeness, "completeness_data")) stop("completeness must come from completeness_tracker_data_prep().")
  if (length(study_name) != 1 || is.na(study_name) || !grepl("^[A-Za-z0-9_-]+$", study_name)) {
    stop("study_name must contain only letters, numbers, _ and -.")
  }
  if (missing(output_dir) || length(output_dir) != 1 || is.na(output_dir) || trimws(output_dir) == "") {
    stop("output_dir is needed: the folder to save the report in (outside any GitHub repository).")
  }
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(output_dir)) stop("Could not create output_dir: ", output_dir)
  out_abs <- normalizePath(output_dir, winslash = "/")
  d <- out_abs
  repeat {
    if (file.exists(file.path(d, ".git"))) {
      stop("output_dir is inside a Git repository (", d, "). Save the report outside any repository.")
    }
    parent <- dirname(d); if (parent == d) break; d <- parent
  }
  r <- completeness_ratings(completeness, thresholds)
  s <- completeness$settings
  t <- thresholds
  # percentages shown to 1 decimal place, rounding halves up (e.g. 81.25% shows as 81.3%)
  pc <- function(p) ifelse(is.na(p), "", sprintf("%.1f%%", floor(1000 * p + 0.5 + 1e-9) / 10))

  wb <- openxlsx::createWorkbook()
  bold <- openxlsx::createStyle(textDecoration = "bold")
  head <- openxlsx::createStyle(textDecoration = "bold", fgFill = "#D9D9D9", border = "TopBottomLeftRight",
                                halign = "center", valign = "center", wrapText = TRUE)
  cell <- openxlsx::createStyle(border = "TopBottomLeftRight", halign = "center")
  left <- openxlsx::createStyle(border = "TopBottomLeftRight", wrapText = TRUE, valign = "top")
  fills <- list(Green = openxlsx::createStyle(fgFill = "#C6EFCE", fontColour = "#006100", border = "TopBottomLeftRight", halign = "center"),
                Amber = openxlsx::createStyle(fgFill = "#FFEB9C", fontColour = "#9C5700", border = "TopBottomLeftRight", halign = "center"),
                Red   = openxlsx::createStyle(fgFill = "#FFC7CE", fontColour = "#9C0006", border = "TopBottomLeftRight", halign = "center"))

  # ---- Cover sheet ----
  openxlsx::addWorksheet(wb, "Cover")
  ev_lab <- paste0(completeness$events$event_name, " (", completeness$events$unique_event_name, ")")
  cover <- data.frame(Item = c(
    "Report", "Study", "REDCap project", "Environment", "Data exported", "Report saved by",
    "Script", "Functions", "Events included", "Forms left out", "Forms counted only where started",
    "Timepoints file (timepoints not taken place left out)", "Thresholds: complete",
    "Thresholds: partially complete", "Thresholds: incomplete", "How forms are counted"),
    Value = c(
      "Data completeness report", study_name, s$project_title, s$environment,
      format(s$export_time, "%d %b %Y %H:%M"), Sys.info()[["user"]],
      if (nzchar(script)) script else "(not given)",
      paste0("completeness_tracker_data_prep V", s$version, "; write_excel_completeness V", version),
      paste(ev_lab, collapse = "; "),
      if (length(s$exclude_forms)) paste(s$exclude_forms, collapse = ", ") else "None",
      if (length(s$as_needed_forms)) paste(s$as_needed_forms, collapse = ", ") else "None",
      if (s$timepoints == "none") "Not used" else s$timepoints,
      sprintf("Green %s or more; amber %s to under %s; red under %s", pc(t$complete_green), pc(t$complete_amber), pc(t$complete_green), pc(t$complete_amber)),
      sprintf("Green %s or less; amber over %s to %s; red over %s", pc(t$partial_green), pc(t$partial_green), pc(t$partial_amber), pc(t$partial_amber)),
      sprintf("Green %s or less; amber over %s to %s; red over %s", pc(t$incomplete_green), pc(t$incomplete_green), pc(t$incomplete_amber), pc(t$incomplete_amber)),
      paste("Only participants with data at an event are counted. Every instance of a repeating form or event is counted.",
            "Partially complete = REDCap 'Unverified'. A form with no status counts as incomplete,",
            "except forms counted only where started. Records with no data access group are shown as 'Unknown';",
            "a project with no data access groups is shown as one group, 'All participants'.")),
    stringsAsFactors = FALSE)
  openxlsx::writeData(wb, "Cover", cover, headerStyle = head)
  openxlsx::addStyle(wb, "Cover", left, rows = 2:(nrow(cover) + 1), cols = 1:2, gridExpand = TRUE)
  openxlsx::addStyle(wb, "Cover", openxlsx::createStyle(textDecoration = "bold", border = "TopBottomLeftRight", valign = "top", wrapText = TRUE),
                     rows = 2:(nrow(cover) + 1), cols = 1)
  openxlsx::setColWidths(wb, "Cover", cols = 1:2, widths = c(40, 100))

  # ---- One sheet per event ----
  used <- "Cover"
  sites <- completeness$sites
  for (i in seq_len(nrow(completeness$events))) {
    e <- completeness$events$unique_event_name[i]
    nm <- substr(gsub("[][*?/\\\\:]", "-", completeness$events$event_name[i]), 1, 28)
    base <- nm; k <- 2
    while (tolower(nm) %in% tolower(used)) { nm <- paste0(substr(base, 1, 25), " (", k, ")"); k <- k + 1 }
    used <- c(used, nm)
    openxlsx::addWorksheet(wb, nm)
    openxlsx::writeData(wb, nm, paste0(completeness$events$event_name[i], " (", e, ")"), startRow = 1)
    openxlsx::addStyle(wb, nm, openxlsx::createStyle(textDecoration = "bold", fontSize = 13), rows = 1, cols = 1)
    p <- completeness$participants[completeness$participants$event == e, ]
    openxlsx::writeData(wb, nm, "Participants counted:", startRow = 3)
    openxlsx::writeData(wb, nm, "Timepoints left out (not taken place):", startRow = 4)
    openxlsx::addStyle(wb, nm, bold, rows = 3:4, cols = 1)
    # header
    openxlsx::writeData(wb, nm, "Form", startRow = 6, startCol = 1)
    openxlsx::mergeCells(wb, nm, cols = 1, rows = 6:7)
    for (j in seq_along(sites)) {
      c0 <- 2 + (j - 1) * 4
      openxlsx::writeData(wb, nm, sites[j], startRow = 6, startCol = c0)
      openxlsx::mergeCells(wb, nm, cols = c0:(c0 + 3), rows = 6)
      openxlsx::writeData(wb, nm, t(c("Instances", "Complete", "Partially complete", "Incomplete")),
                          startRow = 7, startCol = c0, colNames = FALSE)
      openxlsx::writeData(wb, nm, p$participants[p$site == sites[j]], startRow = 3, startCol = c0)
      openxlsx::writeData(wb, nm, p$left_out[p$site == sites[j]], startRow = 4, startCol = c0)
      openxlsx::mergeCells(wb, nm, cols = c0:(c0 + 3), rows = 3)
      openxlsx::mergeCells(wb, nm, cols = c0:(c0 + 3), rows = 4)
      openxlsx::addStyle(wb, nm, cell, rows = 3:4, cols = c0:(c0 + 3), gridExpand = TRUE)
    }
    last_col <- 1 + 4 * length(sites)
    openxlsx::addStyle(wb, nm, head, rows = 6:7, cols = 1:last_col, gridExpand = TRUE)
    # body
    forms <- c(completeness$forms_by_event[[e]], "All forms")
    labels <- completeness$forms$label[match(forms, completeness$forms$form)]
    labels[forms == "All forms"] <- "All forms"
    for (fi in seq_along(forms)) {
      row <- 7 + fi
      openxlsx::writeData(wb, nm, labels[fi], startRow = row, startCol = 1)
      openxlsx::addStyle(wb, nm, if (forms[fi] == "All forms") openxlsx::createStyle(textDecoration = "bold", border = "TopBottomLeftRight") else left,
                         rows = row, cols = 1)
      for (j in seq_along(sites)) {
        c0 <- 2 + (j - 1) * 4
        x <- r[r$event == e & r$form == forms[fi] & r$site == sites[j], ]
        vals <- c(x$instances,
                  if (x$instances > 0) sprintf("%d (%s)", c(x$complete, x$partial, x$incomplete),
                                               pc(c(x$pct_complete, x$pct_partial, x$pct_incomplete))) else rep("-", 3))
        openxlsx::writeData(wb, nm, t(vals), startRow = row, startCol = c0, colNames = FALSE)
        openxlsx::addStyle(wb, nm, cell, rows = row, cols = c0)
        rat <- c(x$rating_complete, x$rating_partial, x$rating_incomplete)
        for (q in 1:3) {
          openxlsx::addStyle(wb, nm, if (is.na(rat[q])) cell else fills[[rat[q]]], rows = row, cols = c0 + q)
        }
      }
    }
    openxlsx::setColWidths(wb, nm, cols = 1, widths = 34)
    openxlsx::setColWidths(wb, nm, cols = 2:last_col, widths = 13)
    openxlsx::freezePane(wb, nm, firstActiveRow = 8, firstActiveCol = 2)
    openxlsx::pageSetup(wb, nm, orientation = "landscape", fitToWidth = TRUE, fitToHeight = FALSE)
  }

  f <- file.path(out_abs, paste0(study_name, "_DataCompleteness_", format(Sys.time(), "%Y-%m-%d_%H%M"), ".xlsx"))
  openxlsx::saveWorkbook(wb, f, overwrite = TRUE)
  message("Data completeness report saved: ", f)
  invisible(list(file = f, ratings = r))
}

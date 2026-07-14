#' Write Data Completeness Worksheet
#'
#' Adds a formatted data-completeness worksheet to an existing Excel workbook.
#' The worksheet summarises form completion status by site and presents both
#' record-level counts and site-level percentages for each completion status.
#'
#' The worksheet is structured dynamically according to the number of forms,
#' sites, and completion statuses supplied. Site headings are merged across the
#' associated status columns, with participant or form-instance counts displayed
#' beneath each site heading.
#'
#' The function writes:
#'
#' \itemize{
#'   \item The REDCap timepoint or worksheet name
#'   \item The total number of forms included in the summary
#'   \item The number of completed form instances recorded for each site
#'   \item Form-level counts for each site and completion status
#'   \item The percentage distribution of completion statuses within each site
#' }
#'
#' Participant or instance counts are calculated directly from
#' \code{raw_data}. Records without a Data Access Group are assigned to an
#' \code{"Unknown"} site category. Sites without any records are retained and
#' assigned a count of zero.
#'
#' Percentage calculations are written to Excel as formulas. For each site, the
#' denominator is the sum of all completion-status counts across all included
#' forms. The numerator is the sum of the selected status column across those
#' forms. This produces the percentage of form instances classified under each
#' status for that site.
#'
#' Conditional formatting is applied to the percentage row using the following
#' thresholds:
#'
#' \describe{
#'   \item{Complete}{
#'     Green where completion is at least 85 percent; amber where completion is
#'     at least 75 percent but below 85 percent; red where completion is below
#'     75 percent.
#'   }
#'   \item{Partially complete}{
#'     Green where the percentage is no more than 15 percent; amber where the
#'     percentage is above 15 percent and no more than 25 percent; red where the
#'     percentage is above 25 percent.
#'   }
#'   \item{Incomplete}{
#'     Green where the percentage is no more than 15 percent; amber where the
#'     percentage is above 15 percent and no more than 25 percent; red where the
#'     percentage is above 25 percent.
#'   }
#' }
#'
#' The function modifies the supplied workbook by reference and does not return
#' a separate worksheet object.
#'
#' @param wb An \code{openxlsx} workbook object to which the worksheet will be
#'   added.
#'
#' @param sheet_name Character string specifying the worksheet name. This value
#'   is also displayed as the worksheet timepoint.
#'
#' @param summary_table A data frame or matrix containing one row per form and
#'   one column for each site-status combination. The first column should
#'   contain the form name or form label.
#'
#' @param raw_data A data frame containing the underlying REDCap records used
#'   to calculate the number of form instances completed by site. The dataset
#'   must contain a \code{redcap_data_access_group} column.
#'
#' @param sites A character vector defining the sites to display and their
#'   ordering within the worksheet. The value \code{"Unknown"} may be included
#'   where records without a Data Access Group should be displayed explicitly.
#'
#' @param statuses A character vector defining the completion-status columns
#'   displayed for each site. Expected values include \code{"Complete"},
#'   \code{"Partially complete"}, and \code{"Incomplete"}.
#'
#' @return Invisibly modifies \code{wb} by adding and formatting the requested
#'   worksheet. No explicit value is returned.
#'
#' @examples
#' wb <- openxlsx::createWorkbook()
#'
#' write_excel_completeness(
#'   wb = wb,
#'   sheet_name = "Week 12",
#'   summary_table = week_12_summary,
#'   raw_data = week_12_data,
#'   sites = c("Site 1", "Site 2", "Unknown"),
#'   statuses = c(
#'     "Complete",
#'     "Partially complete",
#'     "Incomplete"
#'   )
#' )
#'
#' openxlsx::saveWorkbook(
#'   wb,
#'   "Data_Completeness_Report.xlsx",
#'   overwrite = TRUE
#' )
#'
#' @export
#' 
write_excel_completeness <- function(wb, sheet_name, summary_table, raw_data, sites, statuses){
  
  openxlsx::addWorksheet(wb, sheet_name)
  
  # ---------- Styles ----------
  hs1 <- openxlsx::createStyle(
    fgFill = "#FFDAB9", halign = "CENTER", textDecoration = "Bold",
    border = "TopLeftRightBottom"
  )
  hs2 <- openxlsx::createStyle(
    halign = "CENTER", border = "TopLeftRightBottom"
  )
  percent_style <- openxlsx::createStyle(
    numFmt = "0.0%", halign = "CENTER", border = "TopLeftRightBottom"
  )
  
  # ---------- Dynamic layout ----------
  n_forms <- nrow(summary_table)
  
  header_row_site   <- 3
  participant_row   <- 4
  header_row_status <- 5
  
  data_start_row <- 6
  data_end_row   <- data_start_row + n_forms - 1
  
  # Percentage row immediately after the data (no gap)
  percent_row <- data_end_row + 1
  
  # Table width (Forms + site*status columns)
  last_col <- 1 + length(sites) * length(statuses)
  
  # ---------- Header rows ----------
  header_row1 <- c("Forms", rep(sites, each = length(statuses)))
  header_row2 <- c("", rep(statuses, times = length(sites)))
  
  # ---------- Top info ----------
  openxlsx::writeData(wb, sheet_name, "Timepoint:", startRow = 1, startCol = 1, colNames = FALSE)
  openxlsx::writeData(wb, sheet_name, sheet_name, startRow = 1, startCol = 2, colNames = FALSE)
  
  openxlsx::writeData(wb, sheet_name, "Number of forms:", startRow = 2, startCol = 1, colNames = FALSE)
  openxlsx::writeData(wb, sheet_name, n_forms, startRow = 2, startCol = 2, colNames = FALSE)
  
  openxlsx::writeData(wb, sheet_name, "Number of instances completed:", startRow = participant_row, startCol = 1, colNames = FALSE)
  
  # Merge top cells across the table width
  openxlsx::mergeCells(wb, sheet_name, cols = 2:last_col, rows = 1)
  openxlsx::mergeCells(wb, sheet_name, cols = 2:last_col, rows = 2)
  
  # ---------- Participant counts (from RAW DATA) ----------
  participant_counts <- raw_data %>%
    dplyr::mutate(redcap_data_access_group = dplyr::if_else(
      is.na(redcap_data_access_group), "Unknown", as.character(redcap_data_access_group)
    )) %>%
    dplyr::count(redcap_data_access_group, name = "n") %>%
    tidyr::complete(redcap_data_access_group = sites, fill = list(n = 0)) %>%
    dplyr::arrange(factor(redcap_data_access_group, levels = sites))
  
  # Write participant counts to row 4, merged across each site's status columns
  col_index <- 2
  for (site in sites) {
    n_site <- participant_counts$n[participant_counts$redcap_data_access_group == site]
    if (length(n_site) == 0) n_site <- 0
    
    openxlsx::mergeCells(
      wb, sheet_name,
      cols = col_index:(col_index + length(statuses) - 1),
      rows = participant_row
    )
    openxlsx::writeData(wb, sheet_name, x = n_site, startCol = col_index, startRow = participant_row, colNames = FALSE)
    openxlsx::addStyle(
      wb, sheet_name, style = hs2, rows = participant_row,
      cols = col_index:(col_index + length(statuses) - 1),
      gridExpand = TRUE
    )
    
    col_index <- col_index + length(statuses)
  }
  
  # ---------- Write header rows ----------
  openxlsx::writeData(
    wb, sheet_name, t(as.data.frame(header_row1)),
    startRow = header_row_site, startCol = 1, colNames = FALSE
  )
  openxlsx::writeData(
    wb, sheet_name, t(as.data.frame(header_row2)),
    startRow = header_row_status, startCol = 1, colNames = FALSE
  )
  
  # ---------- Write summary table ----------
  openxlsx::writeData(
    wb, sheet_name, summary_table,
    startRow = data_start_row, startCol = 1, colNames = FALSE
  )
  
  # Merge site headers across statuses on row 3
  col_index <- 2
  for (site in sites) {
    openxlsx::mergeCells(
      wb, sheet_name,
      cols = col_index:(col_index + length(statuses) - 1),
      rows = header_row_site
    )
    col_index <- col_index + length(statuses)
  }
  
  # ---------- Apply styles ----------
  openxlsx::addStyle(wb, sheet_name, hs1, rows = 1, cols = 1:last_col, gridExpand = TRUE)
  openxlsx::addStyle(wb, sheet_name, hs1, rows = 2, cols = 1, gridExpand = TRUE)
  openxlsx::addStyle(wb, sheet_name, hs2, rows = 2, cols = 2:last_col, gridExpand = TRUE)
  
  openxlsx::addStyle(wb, sheet_name, hs1, rows = header_row_site, cols = 1:last_col, gridExpand = TRUE)
  openxlsx::addStyle(wb, sheet_name, hs1, rows = participant_row, cols = 1, gridExpand = TRUE)
  openxlsx::addStyle(wb, sheet_name, hs1, rows = header_row_status, cols = 1:last_col, gridExpand = TRUE)
  
  openxlsx::setColWidths(wb, sheet_name, cols = 1, widths = "auto")
  
  # ---------- Percentage row ----------
  openxlsx::writeData(
    wb, sheet_name, "Percentage of forms by status:",
    startRow = percent_row, startCol = 1, colNames = FALSE
  )
  openxlsx::addStyle(wb, sheet_name, style = hs1, rows = percent_row, cols = 1, gridExpand = TRUE)
  
  # New denominator logic:
  # % for each status = SUM(status column over forms) / SUM(all status columns over forms for that site)
  for (site_index in seq_along(sites)) {
    
    site_start_col <- 2 + (site_index - 1) * length(statuses)
    site_end_col   <- site_start_col + length(statuses) - 1
    
    site_start_letter <- openxlsx::int2col(site_start_col)
    site_end_letter   <- openxlsx::int2col(site_end_col)
    
    # Denominator: sum all statuses for this site across all form rows (a rectangle sum)
    denom_range <- paste0(site_start_letter, data_start_row, ":", site_end_letter, data_end_row)
    
    for (status_index in seq_along(statuses)) {
      current_col        <- site_start_col + (status_index - 1)
      current_col_letter <- openxlsx::int2col(current_col)
      
      numer_range <- paste0(current_col_letter, data_start_row, ":", current_col_letter, data_end_row)
      
      formula <- paste0(
        "IFERROR(ROUND(SUM(", numer_range, ")/SUM(", denom_range, "), 2), 0)"
      )
      
      openxlsx::writeFormula(wb, sheet = sheet_name, x = formula, startCol = current_col, startRow = percent_row)
      openxlsx::addStyle(wb, sheet_name, style = percent_style, rows = percent_row, cols = current_col, gridExpand = TRUE)
    }
  }
  
  # ---------- Conditional formatting on the percentage row ----------
  col_index <- 2
  for (site_index in seq_along(sites)) {
    for (status_index in seq_along(statuses)) {
      current_col <- col_index + (status_index - 1)
      col_letter  <- openxlsx::int2col(current_col)
      
      if (statuses[status_index] == "Complete") {
        openxlsx::conditionalFormatting(
          wb, sheet_name, cols = current_col, rows = percent_row,
          rule = "<0.75",
          style = openxlsx::createStyle(fontColour = "#9C0006", bgFill = "#FFC7CE")
        )
        openxlsx::conditionalFormatting(
          wb, sheet_name, cols = current_col, rows = percent_row,
          rule = paste0("AND(", col_letter, percent_row, ">=0.75,", col_letter, percent_row, "<0.85)"),
          type = "expression",
          style = openxlsx::createStyle(fontColour = "#9C6500", bgFill = "#FFEB9C")
        )
        openxlsx::conditionalFormatting(
          wb, sheet_name, cols = current_col, rows = percent_row,
          rule = ">=0.85",
          style = openxlsx::createStyle(fontColour = "#006600", bgFill = "#acd7b5")
        )
      }
      
      if (statuses[status_index] == "Partially complete") {
        openxlsx::conditionalFormatting(
          wb, sheet_name, cols = current_col, rows = percent_row,
          rule = ">0.25",
          style = openxlsx::createStyle(fontColour = "#9C0006", bgFill = "#FFC7CE")
        )
        openxlsx::conditionalFormatting(
          wb, sheet_name, cols = current_col, rows = percent_row,
          rule = paste0("AND(", col_letter, percent_row, ">0.15,", col_letter, percent_row, "<=0.25)"),
          type = "expression",
          style = openxlsx::createStyle(fontColour = "#9C6500", bgFill = "#FFEB9C")
        )
        openxlsx::conditionalFormatting(
          wb, sheet_name, cols = current_col, rows = percent_row,
          rule = "<=0.15",
          style = openxlsx::createStyle(fontColour = "#006600", bgFill = "#acd7b5")
        )
      }
      
      if (statuses[status_index] == "Incomplete") {
        openxlsx::conditionalFormatting(
          wb, sheet_name, cols = current_col, rows = percent_row,
          rule = paste0("=", col_letter, percent_row, ">0.25"),
          type = "expression",
          style = openxlsx::createStyle(fontColour = "#9C0006", bgFill = "#FFC7CE")
        )
        openxlsx::conditionalFormatting(
          wb, sheet_name, cols = current_col, rows = percent_row,
          rule = paste0("=AND(", col_letter, percent_row, ">0.15,", col_letter, percent_row, "<=0.25)"),
          type = "expression",
          style = openxlsx::createStyle(fontColour = "#9C6500", bgFill = "#FFEB9C")
        )
        openxlsx::conditionalFormatting(
          wb, sheet_name, cols = current_col, rows = percent_row,
          rule = paste0("=", col_letter, percent_row, "<=0.15"),
          type = "expression",
          style = openxlsx::createStyle(fontColour = "#006600", bgFill = "#acd7b5")
        )
      }
    }
    col_index <- col_index + length(statuses)
  }
  
  # ---------- Column widths ----------
  openxlsx::setColWidths(wb, sheet = sheet_name, cols = 1:last_col, widths = "auto")
}
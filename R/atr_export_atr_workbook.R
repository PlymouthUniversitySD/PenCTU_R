#Title: ATR - Export ATR workbook
#Author: Paigan Aspinall
#Version & Date: 1.0.0 02JUN2026
#R version: 4.4.3

#' Export ATR Workbook
#'
#' Creates a formatted Excel workbook containing audit trail review outputs.
#' The workbook always includes a cover sheet and can optionally include any
#' combination of audit output sheets.
#'
#' Optional outputs include:
#' \itemize{
#'   \item Critical data item changes
#'   \item Changes per user
#'   \item Monthly changes per user
#'   \item Unstable fields
#'   \item Time between changes
#'   \item Out-of-hours changes
#' }
#'
#' Conditional formatting is applied where relevant:
#' \itemize{
#'   \item \code{n_changes}: low values green, high values red
#'   \item \code{days_between_changes}: low values green, high values red
#'   \item Monthly user change columns: row-wise low values green, high values red
#' }
#'
#' @param critical_data_changes Optional data frame of critical data item changes.
#' @param user_data_changes Optional data frame of total changes per user.
#' @param monthly_user_changes Optional data frame of monthly changes per user.
#' @param unstable_fields Optional data frame of unstable fields.
#' @param time_to_change Optional data frame of time between changes.
#' @param out_of_hours_changes Optional data frame of out-of-hours changes.
#' @param document_id Character string containing the document ID.
#' @param study_name Character string containing the study name.
#' @param file Output Excel file name.
#'
#' @return Saves an Excel workbook to the specified file path.
#'
#' @examples
#' export_atr_workbook(
#'   critical_data_changes = critical_data_changes,
#'   user_data_changes = user_data_changes,
#'   monthly_user_changes = monthly_user_changes,
#'   unstable_fields = unstable_fields,
#'   time_to_change = time_to_change,
#'   out_of_hours_changes = out_of_hours_changes,
#'   document_id = document_id,
#'   study_name = study_name,
#'   file = "ATR_review_output.xlsx"
#' )
#'
#' @export
export_atr_workbook <- function(
    critical_data_changes = NULL,
    user_data_changes = NULL,
    monthly_user_changes = NULL,
    unstable_fields = NULL,
    time_to_change = NULL,
    out_of_hours_changes = NULL,
    document_id,
    study_name,
    file = "ATR_review_output.xlsx"
) {
  
  script_version <- "V1.0.0 02JUN2026"
  
  requireNamespace("openxlsx", quietly = TRUE)
  requireNamespace("dplyr", quietly = TRUE)
  
  wb <- openxlsx::createWorkbook()
  
  title_style <- openxlsx::createStyle(
    fontSize = 16,
    textDecoration = "bold",
    halign = "center",
    valign = "center",
    fgFill = "#83CCEB",
    border = "TopBottomLeftRight"
  )
  
  subtitle_style <- openxlsx::createStyle(
    fontSize = 14,
    textDecoration = "bold",
    halign = "center",
    fgFill = "#D9D9D9",
    border = "TopBottomLeftRight"
  )
  
  label_style <- openxlsx::createStyle(
    textDecoration = "bold",
    border = "TopBottomLeftRight",
    valign = "top",
    wrapText = TRUE
  )
  
  value_style <- openxlsx::createStyle(
    border = "TopBottomLeftRight",
    valign = "top",
    wrapText = TRUE
  )
  
  header_style <- openxlsx::createStyle(
    textDecoration = "bold",
    fgFill = "#D9D9D9",
    border = "TopBottomLeftRight",
    valign = "top",
    wrapText = TRUE
  )
  
  sheet_header_style <- openxlsx::createStyle(
    textDecoration = "bold",
    fgFill = "#D9EAF7",
    border = "Bottom"
  )
  
  add_basic_sheet <- function(wb, sheet_name, df) {
    
    openxlsx::addWorksheet(wb, sheet_name)
    openxlsx::writeData(wb, sheet_name, df)
    openxlsx::freezePane(wb, sheet_name, firstRow = TRUE)
    openxlsx::addFilter(wb, sheet_name, rows = 1, cols = seq_along(df))
    openxlsx::setColWidths(wb, sheet_name, cols = seq_along(df), widths = "auto")
    
    openxlsx::addStyle(
      wb,
      sheet_name,
      sheet_header_style,
      rows = 1,
      cols = seq_along(df),
      gridExpand = TRUE
    )
  }
  
  add_colour_scale <- function(wb, sheet_name, df, column_name) {
    
    if (column_name %in% names(df) && nrow(df) > 0) {
      
      col_num <- which(names(df) == column_name)
      
      openxlsx::conditionalFormatting(
        wb,
        sheet = sheet_name,
        cols = col_num,
        rows = 2:(nrow(df) + 1),
        type = "colourScale",
        style = c("#63BE7B", "#FFEB84", "#F8696B")
      )
    }
  }
  
  # Cover sheet
  openxlsx::addWorksheet(wb, "Cover Sheet")
  
  openxlsx::mergeCells(wb, "Cover Sheet", cols = 1:3, rows = 1)
  openxlsx::writeData(wb, "Cover Sheet", document_id, startCol = 1, startRow = 1)
  openxlsx::addStyle(wb, "Cover Sheet", title_style, rows = 1, cols = 1:3, gridExpand = TRUE)
  
  openxlsx::mergeCells(wb, "Cover Sheet", cols = 1:3, rows = 2)
  openxlsx::writeData(wb, "Cover Sheet", "AUDIT TRAIL REVIEW (ATR)", startCol = 1, startRow = 2)
  openxlsx::addStyle(wb, "Cover Sheet", title_style, rows = 2, cols = 1:3, gridExpand = TRUE)
  
  openxlsx::mergeCells(wb, "Cover Sheet", cols = 1:3, rows = 3)
  openxlsx::writeData(wb, "Cover Sheet", paste0(study_name, " Audit Trail Review Output"), startCol = 1, startRow = 3)
  openxlsx::addStyle(wb, "Cover Sheet", subtitle_style, rows = 3, cols = 1:3, gridExpand = TRUE)
  
  version_text <- paste0('Template version & date: ', script_version)
  
  openxlsx::mergeCells(wb, "Cover Sheet", cols = 1:3, rows = 4)
  openxlsx::writeData(wb, "Cover Sheet", version_text, startCol = 1, startRow = 4)
  openxlsx::addStyle(wb, "Cover Sheet", value_style, rows = 4, cols = 1:3, gridExpand = TRUE)
  
  cover_rows <- data.frame(
    label = c(
      "Document author:",
      "Study name:",
      "System(s) being reviewed:",
      "Date of audit log snapshot:",
      "Date of review:"
    ),
    value = c(
      "Your Name",
      study_name,
      "",
      format(Sys.Date(), "%d%b%Y"),
      ""
    )
  )
  
  openxlsx::writeData(wb, "Cover Sheet", cover_rows, startCol = 1, startRow = 6, colNames = FALSE)
  openxlsx::addStyle(wb, "Cover Sheet", label_style, rows = 6:10, cols = 1, gridExpand = TRUE)
  openxlsx::addStyle(wb, "Cover Sheet", value_style, rows = 6:10, cols = 2:3, gridExpand = TRUE)
  
  for (i in 6:10) {
    openxlsx::mergeCells(wb, "Cover Sheet", cols = 2:3, rows = i)
  }
  
  openxlsx::mergeCells(wb, "Cover Sheet", cols = 1:3, rows = 11)
  openxlsx::writeData(wb, "Cover Sheet", "Summary of findings and actions from the review:", startCol = 1, startRow = 11)
  openxlsx::addStyle(wb, "Cover Sheet", header_style, rows = 11, cols = 1:3, gridExpand = TRUE)
  
  openxlsx::writeData(
    wb,
    "Cover Sheet",
    data.frame(
      description = "Description of finding:",
      corrective = "Corrective action(s) taken:",
      preventative = "Preventative action(s) taken:"
    ),
    startCol = 1,
    startRow = 12,
    colNames = FALSE
  )
  
  example_rows <- data.frame(
    description = c(
      "e.g. primary outcome field 'field_name' was identified as having been subject to 18 data changes in the past month.",
      "e.g. user, 'user_name' was identified as having made 52 changes in the past month, when their typical changes are under 10 per month."
    ),
    corrective = c(
      paste(
        "e.g. the reasons for the changes were reviewed and it was identified",
        "that a new site had not known how to complete the outcome and had",
        "done it wrong for their first participants. The TM and QA manager",
        "were notified of the problem. Other sites were contacted to determine",
        "if the same mistake had been made there. Additional training on the",
        "outcome was offered to sites."
      ),
      paste(
        "e.g. the reasons for the changes were reviewed as well as the names",
        "of fields reviewed, a pattern was established of the temperature",
        "field being changed, however the reason for review was entered as '-'",
        "in many of the cases. The person was contacted to query why these",
        "changes had been made and what the reason was, they notified us",
        "that they had been entering the value to 1 decimal places when it",
        "should have been entered to 2 and so had changed the values.",
        "The user was reminded that they must enter a reason for changes."
      )
    ),
    preventative = c(
      paste(
        "e.g. additional training will be offered to all new sites to ensure",
        "the mistake is not repeated. A note in the system was added offering",
        "additional instructions to users about completing the outcome.",
        "The outcome will be monitored for upcoming participants to identify",
        "any further issues."
      ),
      paste(
        "e.g. the TM and QA manager were notified and conducted SDV for the",
        "values that had been changed to ensure that the values entered to",
        "2 d.p. were present in the SD. A reminder was sent out to sites",
        "that they must enter a sufficient reason for change in the reason",
        "for change box. Additionally, it was suggested to sites that if",
        "mass data changes are to be undertaken that they notify the data",
        "team in advance."
      )
    )
  )
  
  openxlsx::writeData(
    wb,
    "Cover Sheet",
    example_rows,
    startCol = 1,
    startRow = 13,
    colNames = FALSE
  )
  
  example_style <- openxlsx::createStyle(
    fontColour = "#FF0000",
    valign = "top",
    wrapText = TRUE,
    border = "TopBottomLeftRight"
  )
  
  openxlsx::addStyle(
    wb,
    "Cover Sheet",
    example_style,
    rows = 13:14,
    cols = 1:3,
    gridExpand = TRUE
  )
  
  openxlsx::setRowHeights(
    wb,
    "Cover Sheet",
    rows = 13:14,
    heights = c(140, 180)
  )
  
  openxlsx::addStyle(wb, "Cover Sheet", header_style, rows = 12, cols = 1:3, gridExpand = TRUE)
  
  openxlsx::setColWidths(wb, "Cover Sheet", cols = 1:3, widths = c(35, 50, 50))
  openxlsx::setRowHeights(wb, "Cover Sheet", rows = 1:4, heights = c(22, 28, 24, 18))
  
  # Optional sheets
  if (!is.null(critical_data_changes)) {
    add_basic_sheet(wb, "Critical Data Items", critical_data_changes)
    add_colour_scale(wb, "Critical Data Items", critical_data_changes, "n_changes")
  }
  
  if (!is.null(user_data_changes) && !is.null(monthly_user_changes)) {
    
    changes_per_user <- dplyr::left_join(
      user_data_changes,
      monthly_user_changes,
      by = "username"
    )
    
    add_basic_sheet(wb, "Changes per User", changes_per_user)
    
    month_cols <- setdiff(names(changes_per_user), c("username", "n_changes"))
    
    if (length(month_cols) > 0 && nrow(changes_per_user) > 0) {
      
      month_col_nums <- which(names(changes_per_user) %in% month_cols)
      
      for (i in seq_len(nrow(changes_per_user))) {
        openxlsx::conditionalFormatting(
          wb,
          sheet = "Changes per User",
          cols = month_col_nums,
          rows = i + 1,
          type = "colourScale",
          style = c("#63BE7B", "#FFEB84", "#F8696B")
        )
      }
    }
  }
  
  if (!is.null(unstable_fields)) {
    add_basic_sheet(wb, "Unstable Fields", unstable_fields)
    add_colour_scale(wb, "Unstable Fields", unstable_fields, "n_changes")
  }
  
  if (!is.null(time_to_change)) {
    add_basic_sheet(wb, "Time to Change", time_to_change)
    add_colour_scale(wb, "Time to Change", time_to_change, "days_between_changes")
  }
  
  if (!is.null(out_of_hours_changes)) {
    add_basic_sheet(wb, "Out of Hours Changes", out_of_hours_changes)
  }
  
  openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
}

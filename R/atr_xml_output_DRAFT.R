library(openxlsx)
library(dplyr)

export_atr_workbook <- function(
    critical_data_changes,
    user_data_changes,
    monthly_user_changes,
    unstable_fields,
    document_id,
    study_name,
    file = "ATR_review_output.xlsx"
) {
  
  wb <- createWorkbook()
  
  # -----------------------------
  # 1. Cover sheet
  # -----------------------------
  
  addWorksheet(wb, "Cover Sheet")
  
  title_style <- createStyle(
    fontSize = 16,
    textDecoration = "bold",
    halign = "center",
    valign = "center",
    fgFill = "#83CCEB",
    border = "TopBottomLeftRight"
  )
  
  subtitle_style <- createStyle(
    fontSize = 14,
    textDecoration = "bold",
    halign = "center",
    fgFill = "#D9D9D9",
    border = "TopBottomLeftRight"
  )
  
  label_style <- createStyle(
    textDecoration = "bold",
    border = "TopBottomLeftRight",
    valign = "top"
  )
  
  value_style <- createStyle(
    border = "TopBottomLeftRight",
    valign = "top"
  )
  
  header_style <- createStyle(
    textDecoration = "bold",
    fgFill = "#D9D9D9",
    border = "TopBottomLeftRight",
    valign = "top"
  )
  
  # Document headers
  mergeCells(wb, "Cover Sheet", cols = 1:3, rows = 1)
  writeData(wb, "Cover Sheet", document_id, startCol = 1, startRow = 1)
  addStyle(wb, "Cover Sheet", title_style, rows = 1, cols = 1:3, gridExpand = TRUE)
  
  mergeCells(wb, "Cover Sheet", cols = 1:3, rows = 2)
  writeData(wb, "Cover Sheet", "AUDIT TRAIL REVIEW (ATR)", startCol = 1, startRow = 2)
  addStyle(wb, "Cover Sheet", title_style, rows = 2, cols = 1:3, gridExpand = TRUE)
  
  mergeCells(wb, "Cover Sheet", cols = 1:3, rows = 3)
  writeData(wb, "Cover Sheet", paste0(study_name, " Audit Trail Review Output"), startCol = 1, startRow = 3)
  addStyle(wb, "Cover Sheet", subtitle_style, rows = 3, cols = 1:3, gridExpand = TRUE)
  
  mergeCells(wb, "Cover Sheet", cols = 1:3, rows = 4)
  writeData(wb, "Cover Sheet", "Template version & date: V1.0 24MAR2026", startCol = 1, startRow = 4)
  addStyle(wb, "Cover Sheet", value_style, rows = 4, cols = 1:3, gridExpand = TRUE)
  
  # Metadata section
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
  
  writeData(wb, "Cover Sheet", cover_rows, startCol = 1, startRow = 6, colNames = FALSE)
  addStyle(wb, "Cover Sheet", label_style, rows = 6:10, cols = 1, gridExpand = TRUE)
  addStyle(wb, "Cover Sheet", value_style, rows = 6:10, cols = 2:3, gridExpand = TRUE)
  
  mergeCells(wb, "Cover Sheet", cols = 2:3, rows = 6)
  mergeCells(wb, "Cover Sheet", cols = 2:3, rows = 7)
  mergeCells(wb, "Cover Sheet", cols = 2:3, rows = 8)
  mergeCells(wb, "Cover Sheet", cols = 2:3, rows = 9)
  mergeCells(wb, "Cover Sheet", cols = 2:3, rows = 10)
  
  # Findings section
  mergeCells(wb, "Cover Sheet", cols = 1:3, rows = 11)
  writeData(wb, "Cover Sheet", "Summary of findings and actions from the review:", startCol = 1, startRow = 11)
  addStyle(wb, "Cover Sheet", header_style, rows = 11, cols = 1:3, gridExpand = TRUE)
  
  writeData(
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
  addStyle(wb, "Cover Sheet", header_style, rows = 12, cols = 1:3, gridExpand = TRUE)
  
  # Example findings/actions rows
  
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
  
  writeData(
    wb,
    "Cover Sheet",
    example_rows,
    startCol = 1,
    startRow = 13,
    colNames = FALSE
  )
  
  example_style <- createStyle(
    fontColour = "#FF0000",
    valign = "top",
    wrapText = TRUE,
    border = "TopBottomLeftRight"
  )
  
  addStyle(
    wb,
    "Cover Sheet",
    example_style,
    rows = 13:14,
    cols = 1:3,
    gridExpand = TRUE
  )
  
  setRowHeights(
    wb,
    "Cover Sheet",
    rows = 13:14,
    heights = c(140, 180)
  )
  
  setColWidths(wb, "Cover Sheet", cols = 1:3, widths = c(35, 50, 50))
  setRowHeights(wb, "Cover Sheet", rows = 1:4, heights = c(22, 28, 24, 18))
  setRowHeights(wb, "Cover Sheet", rows = 13:25, heights = 35)
  
  # -----------------------------
  # 2. Critical data items sheet
  # -----------------------------
  
  addWorksheet(wb, "Critical Data Items")
  writeData(wb, "Critical Data Items", critical_data_changes)
  
  freezePane(wb, "Critical Data Items", firstRow = TRUE)
  addFilter(
    wb,
    "Critical Data Items",
    rows = 1,
    cols = seq_along(critical_data_changes)
  )
  setColWidths(
    wb,
    "Critical Data Items",
    cols = seq_along(critical_data_changes),
    widths = "auto"
  )
  
  sheet_header_style <- createStyle(
    textDecoration = "bold",
    fgFill = "#D9EAF7",
    border = "Bottom"
  )
  
  addStyle(
    wb,
    "Critical Data Items",
    sheet_header_style,
    rows = 1,
    cols = seq_along(critical_data_changes),
    gridExpand = TRUE
  )
  
  if ("n_changes" %in% names(critical_data_changes) && nrow(critical_data_changes) > 0) {
    
    n_col <- which(names(critical_data_changes) == "n_changes")
    
    conditionalFormatting(
      wb,
      sheet = "Critical Data Items",
      cols = n_col,
      rows = 2:(nrow(critical_data_changes) + 1),
      type = "colourScale",
      style = c("#63BE7B", "#FFEB84", "#F8696B")
    )
  }
  
  # -----------------------------
  # 3. Changes per user sheet
  # -----------------------------
  
  changes_per_user <- user_data_changes %>%
    left_join(monthly_user_changes, by = "username")
  
  addWorksheet(wb, "Changes per User")
  writeData(wb, "Changes per User", changes_per_user)
  
  freezePane(wb, "Changes per User", firstRow = TRUE)
  addFilter(wb, "Changes per User", rows = 1, cols = seq_along(changes_per_user))
  setColWidths(wb, "Changes per User", cols = seq_along(changes_per_user), widths = "auto")
  
  addStyle(
    wb,
    "Changes per User",
    sheet_header_style,
    rows = 1,
    cols = seq_along(changes_per_user),
    gridExpand = TRUE
  )
  
  month_cols <- setdiff(
    names(changes_per_user),
    c("username", "n_changes")
  )
  
  if (length(month_cols) > 0 && nrow(changes_per_user) > 0) {
    
    month_col_nums <- which(names(changes_per_user) %in% month_cols)
    
    for (i in seq_len(nrow(changes_per_user))) {
      conditionalFormatting(
        wb,
        sheet = "Changes per User",
        cols = month_col_nums,
        rows = i + 1,
        type = "colourScale",
        style = c("#63BE7B", "#FFEB84", "#F8696B")
      )
    }
  }
  
  # -----------------------------
  # 4. Unstable fields sheet
  # -----------------------------
  
  addWorksheet(wb, "Unstable Fields")
  writeData(wb, "Unstable Fields", unstable_fields)
  
  freezePane(wb, "Unstable Fields", firstRow = TRUE)
  addFilter(wb, "Unstable Fields", rows = 1, cols = seq_along(unstable_fields))
  setColWidths(wb, "Unstable Fields", cols = seq_along(unstable_fields), widths = "auto")
  
  addStyle(
    wb,
    "Unstable Fields",
    sheet_header_style,
    rows = 1,
    cols = seq_along(unstable_fields),
    gridExpand = TRUE
  )
  
  if ("n_changes" %in% names(unstable_fields) && nrow(unstable_fields) > 0) {
    
    n_col <- which(names(unstable_fields) == "n_changes")
    
    conditionalFormatting(
      wb,
      sheet = "Unstable Fields",
      cols = n_col,
      rows = 2:(nrow(unstable_fields) + 1),
      type = "colourScale",
      style = c("#63BE7B", "#FFEB84", "#F8696B")
    )
  }
  
  saveWorkbook(wb, file, overwrite = TRUE)
}

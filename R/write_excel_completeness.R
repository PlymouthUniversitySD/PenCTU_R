#' Write Data Completeness Worksheet
#'
#' Adds a formatted data-completeness worksheet to an existing Excel workbook.
#' The worksheet summarises form-completion status by site and presents both
#' form-level counts and site-level percentages for each completion status.
#'
#' The worksheet is structured dynamically according to the number of forms,
#' sites, and completion statuses supplied. Site headings are merged across the
#' associated status columns, with participant or form-instance counts displayed
#' beneath each site heading.
#'
#' The \code{statuses} argument must contain exactly three values in the
#' following order:
#'
#' \enumerate{
#'   \item The label used for incomplete forms
#'   \item The label used for unverified or partially complete forms
#'   \item The label used for complete forms
#' }
#'
#' The function writes:
#'
#' \itemize{
#'   \item The REDCap timepoint or worksheet name
#'   \item The total number of forms included in the summary
#'   \item The number of records or non-repeating instances for each site
#'   \item Form-level counts for each site and completion status
#'   \item The percentage distribution of completion statuses within each site
#' }
#'
#' Site-level record or instance counts are calculated directly from
#' \code{raw_data}. Repeat-instance rows can optionally be excluded from this
#' calculation. When \code{exclude_repeat_instances = TRUE}, rows where
#' \code{redcap_repeat_instance} is equal to or greater than \code{1} are
#' removed before the site counts are calculated.
#'
#' Rows where \code{redcap_repeat_instance} is missing, blank, or equal to
#' \code{0} are retained. If repeat instances are to be excluded,
#' \code{raw_data} must contain a \code{redcap_repeat_instance} column.
#'
#' Records without a REDCap Data Access Group are assigned to the
#' \code{"Unknown"} site category. Sites without any records are retained and
#' assigned a count of zero.
#'
#' Percentage calculations are written to Excel as formulas. For each site, the
#' denominator is the sum of all completion-status counts across all included
#' forms. The numerator is the sum of the selected status column across those
#' forms. This produces the percentage of form instances classified under each
#' completion status for that site.
#'
#' Conditional formatting is applied according to the position of each status
#' within \code{statuses}, rather than the text of the status label. This allows
#' customised display labels to be used.
#'
#' The following thresholds are applied:
#'
#' \describe{
#'   \item{Third status: complete}{
#'     Green where completion is at least 85 percent; amber where completion is
#'     at least 75 percent but below 85 percent; and red where completion is
#'     below 75 percent.
#'   }
#'   \item{Second status: unverified or partially complete}{
#'     Green where the percentage is no more than 15 percent; amber where the
#'     percentage is above 15 percent and no more than 25 percent; and red where
#'     the percentage is above 25 percent.
#'   }
#'   \item{First status: incomplete}{
#'     Green where the percentage is no more than 15 percent; amber where the
#'     percentage is above 15 percent and no more than 25 percent; and red where
#'     the percentage is above 25 percent.
#'   }
#' }
#'
#' The function modifies the supplied workbook by reference and does not return
#' a separate worksheet object.
#'
#' @param wb An \code{openxlsx} workbook object to which the worksheet will be
#'   added.
#'
#' @param sheet_name A character string specifying the worksheet name. This
#'   value is also displayed as the worksheet timepoint.
#'
#' @param summary_table A data frame or matrix containing one row per form and
#'   one column for each site-status combination. The first column should
#'   contain the form name or form label.
#'
#' @param raw_data A data frame containing the underlying REDCap records used
#'   to calculate the number of records or instances by site. The dataset must
#'   contain a \code{redcap_data_access_group} column. When
#'   \code{exclude_repeat_instances = TRUE}, it must also contain a
#'   \code{redcap_repeat_instance} column.
#'
#' @param sites A character vector defining the sites to display and their
#'   ordering within the worksheet. Include \code{"Unknown"} where records
#'   without a Data Access Group should be represented explicitly.
#'
#' @param statuses A character vector containing exactly three completion-status
#'   display labels. Values must be supplied in the following order:
#'   incomplete, unverified or partially complete, and complete.
#'
#' @param exclude_repeat_instances A logical value indicating whether repeating
#'   rows should be excluded from the site-level record or instance counts.
#'   When \code{TRUE}, rows with a \code{redcap_repeat_instance} value of
#'   \code{1} or greater are removed. Defaults to \code{FALSE}.
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
#'   sites = c(
#'     "Site 1",
#'     "Site 2",
#'     "Unknown"
#'   ),
#'   statuses = c(
#'     "Incomplete",
#'     "Partially complete",
#'     "Complete"
#'   ),
#'   exclude_repeat_instances = TRUE
#' )
#'
#' openxlsx::saveWorkbook(
#'   wb,
#'   "Data_Completeness_Report.xlsx",
#'   overwrite = TRUE
#' )
#'
#' @export

write_excel_completeness <- function(
    wb,
    sheet_name,
    summary_table,
    raw_data,
    sites,
    statuses,
    exclude_repeat_instances = FALSE
) {
  
  # ---------- Validate arguments ----------
  
  if (length(statuses) != 3) {
    stop(
      "`statuses` must contain exactly three values in this order: ",
      "Incomplete, Unverified, Complete."
    )
  }
  
  if (
    !is.logical(exclude_repeat_instances) ||
    length(exclude_repeat_instances) != 1 ||
    is.na(exclude_repeat_instances)
  ) {
    stop(
      "`exclude_repeat_instances` must be a single TRUE or FALSE value."
    )
  }
  
  if (!"redcap_data_access_group" %in% names(raw_data)) {
    stop(
      "`raw_data` must contain a `redcap_data_access_group` column."
    )
  }
  
  if (
    exclude_repeat_instances &&
    !"redcap_repeat_instance" %in% names(raw_data)
  ) {
    stop(
      "`exclude_repeat_instances` is TRUE, but `raw_data` does not ",
      "contain a `redcap_repeat_instance` column."
    )
  }
  
  # ---------- Add worksheet ----------
  
  openxlsx::addWorksheet(
    wb,
    sheet_name
  )
  
  # ---------- Styles ----------
  
  hs1 <- openxlsx::createStyle(
    fgFill = "#FFDAB9",
    halign = "CENTER",
    textDecoration = "Bold",
    border = "TopLeftRightBottom"
  )
  
  hs2 <- openxlsx::createStyle(
    halign = "CENTER",
    border = "TopLeftRightBottom"
  )
  
  percent_style <- openxlsx::createStyle(
    numFmt = "0.0%",
    halign = "CENTER",
    border = "TopLeftRightBottom"
  )
  
  red_style <- openxlsx::createStyle(
    fontColour = "#9C0006",
    bgFill = "#FFC7CE"
  )
  
  amber_style <- openxlsx::createStyle(
    fontColour = "#9C6500",
    bgFill = "#FFEB9C"
  )
  
  green_style <- openxlsx::createStyle(
    fontColour = "#006600",
    bgFill = "#acd7b5"
  )
  
  # ---------- Dynamic layout ----------
  
  n_forms <- nrow(summary_table)
  
  header_row_site   <- 3
  participant_row   <- 4
  header_row_status <- 5
  
  data_start_row <- 6
  data_end_row   <- data_start_row + n_forms - 1
  
  # Percentage row immediately after the form data
  percent_row <- data_end_row + 1
  
  # Table width: form column plus one column per site-status combination
  last_col <- 1 + length(sites) * length(statuses)
  
  # ---------- Header content ----------
  
  header_row1 <- c(
    "Forms",
    rep(
      sites,
      each = length(statuses)
    )
  )
  
  header_row2 <- c(
    "",
    rep(
      statuses,
      times = length(sites)
    )
  )
  
  # ---------- Top information ----------
  
  openxlsx::writeData(
    wb,
    sheet_name,
    "Timepoint:",
    startRow = 1,
    startCol = 1,
    colNames = FALSE
  )
  
  openxlsx::writeData(
    wb,
    sheet_name,
    sheet_name,
    startRow = 1,
    startCol = 2,
    colNames = FALSE
  )
  
  openxlsx::writeData(
    wb,
    sheet_name,
    "Number of forms:",
    startRow = 2,
    startCol = 1,
    colNames = FALSE
  )
  
  openxlsx::writeData(
    wb,
    sheet_name,
    n_forms,
    startRow = 2,
    startCol = 2,
    colNames = FALSE
  )
  
  openxlsx::writeData(
    wb,
    sheet_name,
    "Number of instances completed:",
    startRow = participant_row,
    startCol = 1,
    colNames = FALSE
  )
  
  # Merge the top information cells across the table width
  openxlsx::mergeCells(
    wb,
    sheet_name,
    cols = 2:last_col,
    rows = 1
  )
  
  openxlsx::mergeCells(
    wb,
    sheet_name,
    cols = 2:last_col,
    rows = 2
  )
  
  # ---------- Prepare data used for site counts ----------
  
  count_data <- raw_data
  
  if (exclude_repeat_instances) {
    
    count_data <- count_data %>%
      dplyr::mutate(
        .repeat_instance = suppressWarnings(
          as.numeric(
            stringr::str_trim(
              as.character(redcap_repeat_instance)
            )
          )
        )
      ) %>%
      dplyr::filter(
        is.na(.repeat_instance) |
          .repeat_instance < 1
      ) %>%
      dplyr::select(-.repeat_instance)
  }
  
  # ---------- Calculate site-level counts ----------
  
  participant_counts <- count_data %>%
    dplyr::mutate(
      redcap_data_access_group = dplyr::if_else(
        is.na(redcap_data_access_group) |
          stringr::str_trim(
            as.character(redcap_data_access_group)
          ) == "",
        "Unknown",
        as.character(redcap_data_access_group)
      )
    ) %>%
    dplyr::count(
      redcap_data_access_group,
      name = "n"
    ) %>%
    tidyr::complete(
      redcap_data_access_group = sites,
      fill = list(n = 0)
    ) %>%
    dplyr::arrange(
      factor(
        redcap_data_access_group,
        levels = sites
      )
    )
  
  # ---------- Write site-level counts ----------
  
  col_index <- 2
  
  for (site in sites) {
    
    n_site <- participant_counts$n[
      participant_counts$redcap_data_access_group == site
    ]
    
    if (length(n_site) == 0) {
      n_site <- 0
    }
    
    openxlsx::mergeCells(
      wb,
      sheet_name,
      cols = col_index:(col_index + length(statuses) - 1),
      rows = participant_row
    )
    
    openxlsx::writeData(
      wb,
      sheet_name,
      x = n_site,
      startCol = col_index,
      startRow = participant_row,
      colNames = FALSE
    )
    
    openxlsx::addStyle(
      wb,
      sheet_name,
      style = hs2,
      rows = participant_row,
      cols = col_index:(col_index + length(statuses) - 1),
      gridExpand = TRUE
    )
    
    col_index <- col_index + length(statuses)
  }
  
  # ---------- Write header rows ----------
  
  openxlsx::writeData(
    wb,
    sheet_name,
    t(as.data.frame(header_row1)),
    startRow = header_row_site,
    startCol = 1,
    colNames = FALSE
  )
  
  openxlsx::writeData(
    wb,
    sheet_name,
    t(as.data.frame(header_row2)),
    startRow = header_row_status,
    startCol = 1,
    colNames = FALSE
  )
  
  # ---------- Write summary table ----------
  
  openxlsx::writeData(
    wb,
    sheet_name,
    summary_table,
    startRow = data_start_row,
    startCol = 1,
    colNames = FALSE
  )
  
  # ---------- Merge site headings ----------
  
  col_index <- 2
  
  for (site in sites) {
    
    openxlsx::mergeCells(
      wb,
      sheet_name,
      cols = col_index:(col_index + length(statuses) - 1),
      rows = header_row_site
    )
    
    col_index <- col_index + length(statuses)
  }
  
  # ---------- Apply worksheet styles ----------
  
  openxlsx::addStyle(
    wb,
    sheet_name,
    hs1,
    rows = 1,
    cols = 1:last_col,
    gridExpand = TRUE
  )
  
  openxlsx::addStyle(
    wb,
    sheet_name,
    hs1,
    rows = 2,
    cols = 1,
    gridExpand = TRUE
  )
  
  openxlsx::addStyle(
    wb,
    sheet_name,
    hs2,
    rows = 2,
    cols = 2:last_col,
    gridExpand = TRUE
  )
  
  openxlsx::addStyle(
    wb,
    sheet_name,
    hs1,
    rows = header_row_site,
    cols = 1:last_col,
    gridExpand = TRUE
  )
  
  openxlsx::addStyle(
    wb,
    sheet_name,
    hs1,
    rows = participant_row,
    cols = 1,
    gridExpand = TRUE
  )
  
  openxlsx::addStyle(
    wb,
    sheet_name,
    hs1,
    rows = header_row_status,
    cols = 1:last_col,
    gridExpand = TRUE
  )
  
  # ---------- Percentage row ----------
  
  openxlsx::writeData(
    wb,
    sheet_name,
    "Percentage of forms by status:",
    startRow = percent_row,
    startCol = 1,
    colNames = FALSE
  )
  
  openxlsx::addStyle(
    wb,
    sheet_name,
    style = hs1,
    rows = percent_row,
    cols = 1,
    gridExpand = TRUE
  )
  
  # Percentage for each status:
  # sum of status column divided by sum of all status columns for that site
  for (site_index in seq_along(sites)) {
    
    site_start_col <- 2 +
      (site_index - 1) * length(statuses)
    
    site_end_col <- site_start_col +
      length(statuses) - 1
    
    site_start_letter <- openxlsx::int2col(
      site_start_col
    )
    
    site_end_letter <- openxlsx::int2col(
      site_end_col
    )
    
    denominator_range <- paste0(
      site_start_letter,
      data_start_row,
      ":",
      site_end_letter,
      data_end_row
    )
    
    for (status_index in seq_along(statuses)) {
      
      current_col <- site_start_col +
        status_index - 1
      
      current_col_letter <- openxlsx::int2col(
        current_col
      )
      
      numerator_range <- paste0(
        current_col_letter,
        data_start_row,
        ":",
        current_col_letter,
        data_end_row
      )
      
      formula <- paste0(
        "IFERROR(ROUND(SUM(",
        numerator_range,
        ")/SUM(",
        denominator_range,
        "),2),0)"
      )
      
      openxlsx::writeFormula(
        wb,
        sheet = sheet_name,
        x = formula,
        startCol = current_col,
        startRow = percent_row
      )
      
      openxlsx::addStyle(
        wb,
        sheet_name,
        style = percent_style,
        rows = percent_row,
        cols = current_col,
        gridExpand = TRUE
      )
    }
  }
  
  # ---------- Conditional formatting ----------
  
  col_index <- 2
  
  for (site_index in seq_along(sites)) {
    
    for (status_index in seq_along(statuses)) {
      
      current_col <- col_index +
        status_index - 1
      
      col_letter <- openxlsx::int2col(
        current_col
      )
      
      # First status: incomplete
      if (status_index == 1) {
        
        openxlsx::conditionalFormatting(
          wb,
          sheet_name,
          cols = current_col,
          rows = percent_row,
          rule = paste0(
            "=",
            col_letter,
            percent_row,
            ">0.25"
          ),
          type = "expression",
          style = red_style
        )
        
        openxlsx::conditionalFormatting(
          wb,
          sheet_name,
          cols = current_col,
          rows = percent_row,
          rule = paste0(
            "=AND(",
            col_letter,
            percent_row,
            ">0.15,",
            col_letter,
            percent_row,
            "<=0.25)"
          ),
          type = "expression",
          style = amber_style
        )
        
        openxlsx::conditionalFormatting(
          wb,
          sheet_name,
          cols = current_col,
          rows = percent_row,
          rule = paste0(
            "=",
            col_letter,
            percent_row,
            "<=0.15"
          ),
          type = "expression",
          style = green_style
        )
      }
      
      # Second status: unverified or partially complete
      if (status_index == 2) {
        
        openxlsx::conditionalFormatting(
          wb,
          sheet_name,
          cols = current_col,
          rows = percent_row,
          rule = paste0(
            "=",
            col_letter,
            percent_row,
            ">0.25"
          ),
          type = "expression",
          style = red_style
        )
        
        openxlsx::conditionalFormatting(
          wb,
          sheet_name,
          cols = current_col,
          rows = percent_row,
          rule = paste0(
            "=AND(",
            col_letter,
            percent_row,
            ">0.15,",
            col_letter,
            percent_row,
            "<=0.25)"
          ),
          type = "expression",
          style = amber_style
        )
        
        openxlsx::conditionalFormatting(
          wb,
          sheet_name,
          cols = current_col,
          rows = percent_row,
          rule = paste0(
            "=",
            col_letter,
            percent_row,
            "<=0.15"
          ),
          type = "expression",
          style = green_style
        )
      }
      
      # Third status: complete
      if (status_index == 3) {
        
        openxlsx::conditionalFormatting(
          wb,
          sheet_name,
          cols = current_col,
          rows = percent_row,
          rule = paste0(
            "=",
            col_letter,
            percent_row,
            "<0.75"
          ),
          type = "expression",
          style = red_style
        )
        
        openxlsx::conditionalFormatting(
          wb,
          sheet_name,
          cols = current_col,
          rows = percent_row,
          rule = paste0(
            "=AND(",
            col_letter,
            percent_row,
            ">=0.75,",
            col_letter,
            percent_row,
            "<0.85)"
          ),
          type = "expression",
          style = amber_style
        )
        
        openxlsx::conditionalFormatting(
          wb,
          sheet_name,
          cols = current_col,
          rows = percent_row,
          rule = paste0(
            "=",
            col_letter,
            percent_row,
            ">=0.85"
          ),
          type = "expression",
          style = green_style
        )
      }
    }
    
    col_index <- col_index + length(statuses)
  }
  
  # ---------- Column widths ----------
  
  openxlsx::setColWidths(
    wb,
    sheet = sheet_name,
    cols = 1:last_col,
    widths = "auto"
  )
  
  invisible(wb)
}

#Title: Export Central Statistical Monitoring Workbook
#Author: Paigan Aspinall
#Version & Date: V1.0.0 02JUN2026
#R version: 4.4.3

#' Export Central Statistical Monitoring Results Workbook
#'
#' Creates a formatted Excel workbook containing outputs from the Central
#' Statistical Monitoring (CSM) analyses.
#'
#' The workbook includes a standardised cover sheet and automatically generates
#' separate worksheets for each supplied analysis output. Worksheets are only
#' created when the corresponding dataset is provided.
#'
#' Built-in conditional formatting is applied to highlight potentially
#' important findings requiring review. Formatting thresholds are based on the
#' rules defined within each analysis section.
#'
#' Included analyses may comprise:
#' \itemize{
#'   \item Overall distribution summaries
#'   \item Site-level distribution summaries
#'   \item Site-level Z-score analyses
#'   \item Mahalanobis distance analyses
#'   \item Brown-Forsythe variance analyses
#'   \item Anderson-Darling distribution analyses
#'   \item K-means clustering analyses
#'   \item Missing data analyses
#'   \item Duplicate/similar record detection
#'   \item Decimal preference analyses
#' }
#'
#' The cover sheet includes:
#' \itemize{
#'   \item Document identifier
#'   \item Study name
#'   \item Date of data snapshot
#'   \item Review documentation fields
#'   \item Summary of findings and actions section
#' }
#'
#' @param document_id Character string containing the document identifier.
#'
#' @param study_name Character string containing the study name.
#'
#' @param overall_distribution_summary Optional dataframe returned by
#'   \code{overall_distribution_summary()}.
#'
#' @param site_distribution_summary Optional dataframe returned by
#'   \code{site_distribution_summary()}.
#'
#' @param z_scores Optional dataframe returned by
#'   \code{z_score_analysis()}.
#'
#' @param z_flags Optional dataframe containing flagged Z-score results.
#'   Currently retained for backwards compatibility.
#'
#' @param mahalanobis_results Optional dataframe returned by
#'   \code{mahalanobis_analysis()}.
#'
#' @param variance_results Optional dataframe returned by
#'   \code{variance_analysis()}.
#'
#' @param ad_results Optional dataframe returned by
#'   \code{ad_distribution_analysis()}.
#'
#' @param cluster_results Optional dataframe returned by
#'   clustering analyses.
#'
#' @param site_cluster_summary Optional site-level clustering summary
#'   dataframe.
#'
#' @param cluster_flags Optional clustering flag dataframe.
#'
#' @param missing_data_results Optional dataframe returned by
#'   \code{missing_data_analysis()}.
#'
#' @param similarity_results Optional dataframe returned by
#'   \code{similarity_detection()}.
#'
#' @param decimal_preference_results Optional dataframe returned by
#'   \code{decimal_preference_analysis()}.
#'
#' @param file Output filename for the Excel workbook.
#'
#' @return An Excel workbook is written to disk. The function returns the
#'   workbook file invisibly.
#'
#' @details
#' Each analysis output is written to a separate worksheet with:
#' \itemize{
#'   \item Frozen header rows
#'   \item Excel filters
#'   \item Automatic column width adjustment
#'   \item Conditional formatting to highlight potential data quality issues
#' }
#'
#' Conditional formatting rules include:
#' \itemize{
#'   \item Extreme Z-scores
#'   \item Significant variance test results
#'   \item Significant Anderson-Darling test results
#'   \item High Mahalanobis distances
#'   \item High site-level missing data rates
#'   \item Potential duplicate records
#'   \item Decimal preference patterns
#'   \item Unusual site-level distributions
#' }
#'
#' @examples
#' export_csm_workbook(
#'   document_id = "CSM001",
#'   study_name = "Example Study",
#'   overall_distribution_summary = overall_summary,
#'   site_distribution_summary = site_summary,
#'   z_scores = z_scores,
#'   file = "CSM_review_output.xlsx"
#' )
#'
#' @export
#' 
export_csm_workbook <- function(
    document_id,
    study_name,
    overall_distribution_summary = NULL,
    site_distribution_summary = NULL,
    z_scores = NULL,
    z_flags = NULL,
    mahalanobis_results = NULL,
    variance_results = NULL,
    ad_results = NULL,
    cluster_results = NULL,
    site_cluster_summary = NULL,
    cluster_flags = NULL,
    missing_data_results = NULL,
    similarity_results = NULL,
    decimal_preference_results = NULL,
    file = "CSM_review_output.xlsx"
) {
  
  script_version <- 'V1.0.0 02JUN2026'
  
  requireNamespace("openxlsx", quietly = TRUE)
  requireNamespace("dplyr", quietly = TRUE)
  
  wb <- openxlsx::createWorkbook()
  
  # -----------------------------
  # Styles
  # -----------------------------
  
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
  
  example_style <- openxlsx::createStyle(
    fontColour = "#FF0000",
    valign = "top",
    wrapText = TRUE,
    border = "TopBottomLeftRight"
  )
  
  sheet_header_style <- openxlsx::createStyle(
    textDecoration = "bold",
    fgFill = "#D9EAF7",
    border = "Bottom"
  )
  
  red_fill <- openxlsx::createStyle(
    fgFill = "#FFC7CE",
    fontColour = "#9C0006"
  )
  
  amber_fill <- openxlsx::createStyle(
    fgFill = "#FFEB9C",
    fontColour = "#9C6500"
  )
  
  # -----------------------------
  # Cover Sheet
  # -----------------------------
  
  openxlsx::addWorksheet(wb, "Cover Sheet")
  
  openxlsx::mergeCells(wb, "Cover Sheet", cols = 1:3, rows = 1)
  openxlsx::writeData(wb, "Cover Sheet", document_id, startCol = 1, startRow = 1)
  openxlsx::addStyle(wb, "Cover Sheet", title_style, rows = 1, cols = 1:3, gridExpand = TRUE)
  
  openxlsx::mergeCells(wb, "Cover Sheet", cols = 1:3, rows = 2)
  openxlsx::writeData(wb, "Cover Sheet", "CENTRAL STATISTICAL MONITORING (CSM)", startCol = 1, startRow = 2)
  openxlsx::addStyle(wb, "Cover Sheet", title_style, rows = 2, cols = 1:3, gridExpand = TRUE)
  
  openxlsx::mergeCells(wb, "Cover Sheet", cols = 1:3, rows = 3)
  openxlsx::writeData(
    wb,
    "Cover Sheet",
    paste0(study_name, " Central Statistical Monitoring Output"),
    startCol = 1,
    startRow = 3
  )
  openxlsx::addStyle(wb, "Cover Sheet", subtitle_style, rows = 3, cols = 1:3, gridExpand = TRUE)
  
  version_text <- paste0("Template version & date: ", script_version)
  
  openxlsx::mergeCells(wb, "Cover Sheet", cols = 1:3, rows = 4)
  openxlsx::writeData(wb, "Cover Sheet", version_text, startCol = 1, startRow = 4)
  openxlsx::addStyle(wb, "Cover Sheet", value_style, rows = 4, cols = 1:3, gridExpand = TRUE)
  
  cover_rows <- data.frame(
    label = c(
      "Document author:",
      "Study name:",
      "System(s) being reviewed:",
      "Date of data snapshot:",
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
  openxlsx::writeData(
    wb,
    "Cover Sheet",
    "Summary of findings and actions from the review:",
    startCol = 1,
    startRow = 11
  )
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
  openxlsx::addStyle(wb, "Cover Sheet", header_style, rows = 12, cols = 1:3, gridExpand = TRUE)
  
  example_rows <- data.frame(
    `Description of finding:` = c(
      "e.g. Site 003 was identified as having significantly higher haemoglobin values than other sites based on Z-score analysis.",
      "e.g. Site 007 demonstrated significantly different variability in bilirubin values compared with other sites based on Brown-Forsythe testing.",
      "e.g. Participant 02004 demonstrated an extreme multivariate profile within the liver function assessment group based on Mahalanobis distance analysis.",
      "e.g. Site 005 was identified as having 32% missing data for a required critical outcome measure.",
      "e.g. Records 01002 and 01018 were identified as having 96% similarity across required fields suggesting possible duplicate data entry.",
      "e.g. Site 004 demonstrated a strong preference for entering weight values as whole numbers, suggesting potential rounding behaviour."
    ),
    `Corrective action(s) taken:` = c(
      "e.g. Source data verification was undertaken for affected records and no transcription errors were identified.",
      "e.g. The site was contacted and a review of local measurement procedures was undertaken.",
      "e.g. Source documentation was reviewed and the data were confirmed to be correct.",
      "e.g. Missing data reports were issued to the site and outstanding data queries were raised.",
      "e.g. The records were reviewed by the data management team and source documentation was checked to confirm participant uniqueness.",
      "e.g. The site was contacted to determine whether measurements were being rounded prior to entry."
    ),
    `Preventative action(s) taken:` = c(
      "e.g. Enhanced monitoring of haemoglobin data will be undertaken for subsequent participants.",
      "e.g. Additional training was provided to site staff regarding sample collection and laboratory procedures.",
      "e.g. The participant will be monitored in future CSM reviews to ensure no further unusual patterns emerge.",
      "e.g. Missing data rates will be reviewed monthly until the site returns to expected levels.",
      "e.g. Additional duplicate record checks will be incorporated into routine monitoring activities.",
      "e.g. Refresher training was provided regarding the required recording precision for study measurements."
    ),
    check.names = FALSE
  )
  
  openxlsx::writeData(
    wb,
    "Cover Sheet",
    example_rows,
    startCol = 1,
    startRow = 13,
    colNames = FALSE
  )
  
  openxlsx::addStyle(
    wb,
    "Cover Sheet",
    example_style,
    rows = 13:(12 + nrow(example_rows)),
    cols = 1:3,
    gridExpand = TRUE,
    stack = TRUE
  )
  
  openxlsx::setColWidths(
    wb,
    "Cover Sheet",
    cols = 1:3,
    widths = c(45, 60, 60)
  )
  
  openxlsx::setRowHeights(
    wb,
    "Cover Sheet",
    rows = 13:(12 + nrow(example_rows)),
    heights = 90
  )
  
  # -----------------------------
  # Overall Distribution Summary
  # -----------------------------
  
  if (!is.null(overall_distribution_summary)) {
    
    openxlsx::addWorksheet(wb, "Overall Distribution")
    openxlsx::writeData(wb, "Overall Distribution", overall_distribution_summary)
    
    openxlsx::addStyle(
      wb,
      "Overall Distribution",
      sheet_header_style,
      rows = 1,
      cols = seq_along(overall_distribution_summary),
      gridExpand = TRUE
    )
    
    openxlsx::freezePane(wb, "Overall Distribution", firstRow = TRUE)
    openxlsx::addFilter(wb, "Overall Distribution", rows = 1, cols = seq_along(overall_distribution_summary))
    openxlsx::setColWidths(wb, "Overall Distribution", cols = seq_along(overall_distribution_summary), widths = "auto")
    
    if (all(c("mean", "sd", "median") %in% names(overall_distribution_summary)) &&
        nrow(overall_distribution_summary) > 0) {
      
      summary_flags <- dplyr::mutate(
        overall_distribution_summary,
        sd_mean_ratio = dplyr::if_else(
          abs(mean) > 0,
          sd / abs(mean),
          NA_real_
        ),
        median_mean_difference = dplyr::if_else(
          abs(mean) > 0,
          abs(median - mean) / abs(mean),
          NA_real_
        )
      )
      
      sd_col <- which(names(overall_distribution_summary) == "sd")
      median_col <- which(names(overall_distribution_summary) == "median")
      
      for (i in seq_len(nrow(summary_flags))) {
        
        excel_row <- i + 1
        
        if (!is.na(summary_flags$sd_mean_ratio[i])) {
          
          ratio <- summary_flags$sd_mean_ratio[i]
          
          if (ratio < 0.05 || ratio > 1) {
            openxlsx::addStyle(
              wb,
              "Overall Distribution",
              red_fill,
              rows = excel_row,
              cols = sd_col,
              stack = TRUE
            )
          } else if (ratio < 0.10 || ratio > 0.75) {
            openxlsx::addStyle(
              wb,
              "Overall Distribution",
              amber_fill,
              rows = excel_row,
              cols = sd_col,
              stack = TRUE
            )
          }
        }
        
        if (!is.na(summary_flags$median_mean_difference[i])) {
          
          diff_ratio <- summary_flags$median_mean_difference[i]
          
          if (diff_ratio > 0.50) {
            openxlsx::addStyle(
              wb,
              "Overall Distribution",
              red_fill,
              rows = excel_row,
              cols = median_col,
              stack = TRUE
            )
          } else if (diff_ratio > 0.25) {
            openxlsx::addStyle(
              wb,
              "Overall Distribution",
              amber_fill,
              rows = excel_row,
              cols = median_col,
              stack = TRUE
            )
          }
        }
      }
    }
  }
  
  # -----------------------------
  # Site Distribution Summary
  # -----------------------------
  
  if (!is.null(site_distribution_summary)) {
    
    openxlsx::addWorksheet(wb, "Site Distribution")
    
    openxlsx::writeData(
      wb,
      "Site Distribution",
      site_distribution_summary
    )
    
    openxlsx::addStyle(
      wb,
      "Site Distribution",
      sheet_header_style,
      rows = 1,
      cols = seq_along(site_distribution_summary),
      gridExpand = TRUE
    )
    
    openxlsx::freezePane(
      wb,
      "Site Distribution",
      firstRow = TRUE
    )
    
    openxlsx::addFilter(
      wb,
      "Site Distribution",
      rows = 1,
      cols = seq_along(site_distribution_summary)
    )
    
    openxlsx::setColWidths(
      wb,
      "Site Distribution",
      cols = seq_along(site_distribution_summary),
      widths = "auto"
    )
    
    if (all(c("variable", "mean", "median", "sd", "IQR", "n") %in%
            names(site_distribution_summary)) &&
        nrow(site_distribution_summary) > 0) {
      
      site_flags <- site_distribution_summary %>%
        dplyr::group_by(variable) %>%
        dplyr::mutate(
          mean_group_sd = stats::sd(mean, na.rm = TRUE),
          median_group_sd = stats::sd(median, na.rm = TRUE),
          sd_group_sd = stats::sd(sd, na.rm = TRUE),
          
          mean_site_z = dplyr::if_else(
            !is.na(mean_group_sd) & mean_group_sd > 0,
            (mean - mean(mean, na.rm = TRUE)) / mean_group_sd,
            NA_real_
          ),
          
          median_site_z = dplyr::if_else(
            !is.na(median_group_sd) & median_group_sd > 0,
            (median - mean(median, na.rm = TRUE)) / median_group_sd,
            NA_real_
          ),
          
          sd_site_z = dplyr::if_else(
            !is.na(sd_group_sd) & sd_group_sd > 0,
            (sd - mean(sd, na.rm = TRUE)) / sd_group_sd,
            NA_real_
          ),
          
          mean_median_difference = dplyr::if_else(
            !is.na(mean) & mean != 0,
            abs(mean - median) / abs(mean),
            NA_real_
          )
        ) %>%
        dplyr::ungroup()
      
      mean_col <- which(names(site_distribution_summary) == "mean")
      median_col <- which(names(site_distribution_summary) == "median")
      sd_col <- which(names(site_distribution_summary) == "sd")
      iqr_col <- which(names(site_distribution_summary) == "IQR")
      
      for (i in seq_len(nrow(site_flags))) {
        
        excel_row <- i + 1
        
        # Mean much higher/lower at one site compared to others
        if (!is.na(site_flags$mean_site_z[i])) {
          if (abs(site_flags$mean_site_z[i]) > 3) {
            openxlsx::addStyle(
              wb, "Site Distribution", red_fill,
              rows = excel_row, cols = mean_col, stack = TRUE
            )
          } else if (abs(site_flags$mean_site_z[i]) > 2) {
            openxlsx::addStyle(
              wb, "Site Distribution", amber_fill,
              rows = excel_row, cols = mean_col, stack = TRUE
            )
          }
        }
        
        # Median much higher/lower at one site compared to others
        if (!is.na(site_flags$median_site_z[i])) {
          if (abs(site_flags$median_site_z[i]) > 3) {
            openxlsx::addStyle(
              wb, "Site Distribution", red_fill,
              rows = excel_row, cols = median_col, stack = TRUE
            )
          } else if (abs(site_flags$median_site_z[i]) > 2) {
            openxlsx::addStyle(
              wb, "Site Distribution", amber_fill,
              rows = excel_row, cols = median_col, stack = TRUE
            )
          }
        }
        
        # SD = 0 with n > 1
        if (!is.na(site_flags$sd[i]) &&
            !is.na(site_flags$n[i]) &&
            site_flags$sd[i] == 0 &&
            site_flags$n[i] > 1) {
          
          openxlsx::addStyle(
            wb, "Site Distribution", red_fill,
            rows = excel_row, cols = sd_col, stack = TRUE
          )
        }
        
        # IQR = 0 with n > 1
        if (!is.na(site_flags$IQR[i]) &&
            !is.na(site_flags$n[i]) &&
            site_flags$IQR[i] == 0 &&
            site_flags$n[i] > 1) {
          
          openxlsx::addStyle(
            wb, "Site Distribution", red_fill,
            rows = excel_row, cols = iqr_col, stack = TRUE
          )
        }
        
        # SD much higher than other sites
        if (!is.na(site_flags$sd_site_z[i])) {
          if (site_flags$sd_site_z[i] > 3) {
            openxlsx::addStyle(
              wb, "Site Distribution", red_fill,
              rows = excel_row, cols = sd_col, stack = TRUE
            )
          } else if (site_flags$sd_site_z[i] > 2) {
            openxlsx::addStyle(
              wb, "Site Distribution", amber_fill,
              rows = excel_row, cols = sd_col, stack = TRUE
            )
          }
        }
        
        # Large difference between mean and median at one site
        if (!is.na(site_flags$mean_median_difference[i])) {
          if (site_flags$mean_median_difference[i] > 0.50) {
            openxlsx::addStyle(
              wb, "Site Distribution", red_fill,
              rows = excel_row, cols = median_col, stack = TRUE
            )
          } else if (site_flags$mean_median_difference[i] > 0.25) {
            openxlsx::addStyle(
              wb, "Site Distribution", amber_fill,
              rows = excel_row, cols = median_col, stack = TRUE
            )
          }
        }
      }
    }
  }
  
  # -----------------------------
  # Z-Score Analysis
  # -----------------------------
  
  if (!is.null(z_scores)) {
    
    z_scores_for_report <- z_scores %>%
      dplyr::filter(
        !small_n_flag,
        !invalid_flag
      )
    
    openxlsx::addWorksheet(wb, "Z-Scores")
    
    openxlsx::writeData(
      wb,
      "Z-Scores",
      z_scores_for_report
    )
    
    openxlsx::addStyle(
      wb,
      "Z-Scores",
      sheet_header_style,
      rows = 1,
      cols = seq_along(z_scores_for_report),
      gridExpand = TRUE
    )
    
    openxlsx::freezePane(
      wb,
      "Z-Scores",
      firstRow = TRUE
    )
    
    openxlsx::addFilter(
      wb,
      "Z-Scores",
      rows = 1,
      cols = seq_along(z_scores_for_report)
    )
    
    openxlsx::setColWidths(
      wb,
      "Z-Scores",
      cols = seq_along(z_scores_for_report),
      widths = "auto"
    )
    
    if ("z_score" %in% names(z_scores_for_report) &&
        nrow(z_scores_for_report) > 0) {
      
      for (i in seq_len(nrow(z_scores_for_report))) {
        
        excel_row <- i + 1
        z_value <- z_scores_for_report$z_score[i]
        
        if (!is.na(z_value)) {
          
          if (abs(z_value) > 3) {
            
            openxlsx::addStyle(
              wb,
              "Z-Scores",
              red_fill,
              rows = excel_row,
              cols = seq_along(z_scores_for_report),
              gridExpand = TRUE,
              stack = TRUE
            )
            
          } else if (abs(z_value) > 2) {
            
            openxlsx::addStyle(
              wb,
              "Z-Scores",
              amber_fill,
              rows = excel_row,
              cols = seq_along(z_scores_for_report),
              gridExpand = TRUE,
              stack = TRUE
            )
          }
        }
      }
    }
  }
  
  # -----------------------------
  # Mahalanobis Analysis
  # -----------------------------
  
  if (!is.null(mahalanobis_results)) {
    
    openxlsx::addWorksheet(wb, "Mahalanobis")
    
    openxlsx::writeData(
      wb,
      "Mahalanobis",
      mahalanobis_results
    )
    
    openxlsx::addStyle(
      wb,
      "Mahalanobis",
      sheet_header_style,
      rows = 1,
      cols = seq_along(mahalanobis_results),
      gridExpand = TRUE
    )
    
    openxlsx::freezePane(
      wb,
      "Mahalanobis",
      firstRow = TRUE
    )
    
    openxlsx::addFilter(
      wb,
      "Mahalanobis",
      rows = 1,
      cols = seq_along(mahalanobis_results)
    )
    
    openxlsx::setColWidths(
      wb,
      "Mahalanobis",
      cols = seq_along(mahalanobis_results),
      widths = "auto"
    )
    
    if ("flag" %in% names(mahalanobis_results) &&
        nrow(mahalanobis_results) > 0) {
      
      for (i in seq_len(nrow(mahalanobis_results))) {
        
        excel_row <- i + 1
        
        if (isTRUE(mahalanobis_results$flag[i])) {
          
          openxlsx::addStyle(
            wb,
            "Mahalanobis",
            red_fill,
            rows = excel_row,
            cols = seq_along(mahalanobis_results),
            gridExpand = TRUE,
            stack = TRUE
          )
        }
      }
    }
  }
  
  # -----------------------------
  # Variance Analysis
  # -----------------------------
  
  if (!is.null(variance_results)) {
    
    openxlsx::addWorksheet(wb, "Variance Analysis")
    
    openxlsx::writeData(
      wb,
      "Variance Analysis",
      variance_results
    )
    
    openxlsx::addStyle(
      wb,
      "Variance Analysis",
      sheet_header_style,
      rows = 1,
      cols = seq_along(variance_results),
      gridExpand = TRUE
    )
    
    openxlsx::freezePane(
      wb,
      "Variance Analysis",
      firstRow = TRUE
    )
    
    openxlsx::addFilter(
      wb,
      "Variance Analysis",
      rows = 1,
      cols = seq_along(variance_results)
    )
    
    openxlsx::setColWidths(
      wb,
      "Variance Analysis",
      cols = seq_along(variance_results),
      widths = "auto"
    )
    
    if ("p_value" %in% names(variance_results) &&
        nrow(variance_results) > 0) {
      
      for (i in seq_len(nrow(variance_results))) {
        
        excel_row <- i + 1
        p_value <- variance_results$p_value[i]
        
        if (!is.na(p_value)) {
          
          if (p_value < 0.01) {
            
            openxlsx::addStyle(
              wb,
              "Variance Analysis",
              red_fill,
              rows = excel_row,
              cols = seq_along(variance_results),
              gridExpand = TRUE,
              stack = TRUE
            )
            
          } else if (p_value < 0.05) {
            
            openxlsx::addStyle(
              wb,
              "Variance Analysis",
              amber_fill,
              rows = excel_row,
              cols = seq_along(variance_results),
              gridExpand = TRUE,
              stack = TRUE
            )
          }
        }
      }
    }
  }
  # -----------------------------
  # Anderson-Darling Analysis
  # -----------------------------
  
  if (!is.null(ad_results)) {
    
    openxlsx::addWorksheet(wb, "Anderson-Darling")
    
    openxlsx::writeData(
      wb,
      "Anderson-Darling",
      ad_results
    )
    
    openxlsx::addStyle(
      wb,
      "Anderson-Darling",
      sheet_header_style,
      rows = 1,
      cols = seq_along(ad_results),
      gridExpand = TRUE
    )
    
    openxlsx::freezePane(
      wb,
      "Anderson-Darling",
      firstRow = TRUE
    )
    
    openxlsx::addFilter(
      wb,
      "Anderson-Darling",
      rows = 1,
      cols = seq_along(ad_results)
    )
    
    openxlsx::setColWidths(
      wb,
      "Anderson-Darling",
      cols = seq_along(ad_results),
      widths = "auto"
    )
    
    if ("p_value" %in% names(ad_results) &&
        nrow(ad_results) > 0) {
      
      for (i in seq_len(nrow(ad_results))) {
        
        excel_row <- i + 1
        p_value <- ad_results$p_value[i]
        
        if (!is.na(p_value)) {
          
          if (p_value < 0.01) {
            
            openxlsx::addStyle(
              wb,
              "Anderson-Darling",
              red_fill,
              rows = excel_row,
              cols = seq_along(ad_results),
              gridExpand = TRUE,
              stack = TRUE
            )
            
          } else if (p_value < 0.05) {
            
            openxlsx::addStyle(
              wb,
              "Anderson-Darling",
              amber_fill,
              rows = excel_row,
              cols = seq_along(ad_results),
              gridExpand = TRUE,
              stack = TRUE
            )
          }
        }
      }
    }
  }
  
  # -----------------------------
  # K-Means Clustering Analysis
  # -----------------------------
  
  if (!is.null(site_cluster_summary)) {
    
    openxlsx::addWorksheet(wb, "K-Means Clustering")
    
    openxlsx::writeData(
      wb,
      "K-Means Clustering",
      site_cluster_summary
    )
    
    openxlsx::addStyle(
      wb,
      "K-Means Clustering",
      sheet_header_style,
      rows = 1,
      cols = seq_along(site_cluster_summary),
      gridExpand = TRUE
    )
    
    openxlsx::freezePane(
      wb,
      "K-Means Clustering",
      firstRow = TRUE
    )
    
    openxlsx::addFilter(
      wb,
      "K-Means Clustering",
      rows = 1,
      cols = seq_along(site_cluster_summary)
    )
    
    openxlsx::setColWidths(
      wb,
      "K-Means Clustering",
      cols = seq_along(site_cluster_summary),
      widths = "auto"
    )
    
    if ("prop" %in% names(site_cluster_summary) &&
        nrow(site_cluster_summary) > 0) {
      
      for (i in seq_len(nrow(site_cluster_summary))) {
        
        excel_row <- i + 1
        prop_value <- site_cluster_summary$prop[i]
        
        if (!is.na(prop_value)) {
          
          if (prop_value > 0.90) {
            
            openxlsx::addStyle(
              wb,
              "K-Means Clustering",
              red_fill,
              rows = excel_row,
              cols = seq_along(site_cluster_summary),
              gridExpand = TRUE,
              stack = TRUE
            )
            
          } else if (prop_value > 0.80) {
            
            openxlsx::addStyle(
              wb,
              "K-Means Clustering",
              amber_fill,
              rows = excel_row,
              cols = seq_along(site_cluster_summary),
              gridExpand = TRUE,
              stack = TRUE
            )
          }
        }
      }
    }
  }
  # -----------------------------
  # Missing Data Analysis
  # -----------------------------
  
  if (!is.null(missing_data_results)) {
    
    openxlsx::addWorksheet(wb, "Missing Data")
    
    openxlsx::writeData(
      wb,
      "Missing Data",
      missing_data_results
    )
    
    openxlsx::addStyle(
      wb,
      "Missing Data",
      sheet_header_style,
      rows = 1,
      cols = seq_along(missing_data_results),
      gridExpand = TRUE
    )
    
    openxlsx::freezePane(
      wb,
      "Missing Data",
      firstRow = TRUE
    )
    
    openxlsx::addFilter(
      wb,
      "Missing Data",
      rows = 1,
      cols = seq_along(missing_data_results)
    )
    
    openxlsx::setColWidths(
      wb,
      "Missing Data",
      cols = seq_along(missing_data_results),
      widths = "auto"
    )
    
    if ("missing_prop" %in% names(missing_data_results) &&
        nrow(missing_data_results) > 0) {
      
      for (i in seq_len(nrow(missing_data_results))) {
        
        excel_row <- i + 1
        missing_prop <- missing_data_results$missing_prop[i]
        
        if (!is.na(missing_prop)) {
          
          if (missing_prop >= 0.20) {
            
            openxlsx::addStyle(
              wb,
              "Missing Data",
              red_fill,
              rows = excel_row,
              cols = seq_along(missing_data_results),
              gridExpand = TRUE,
              stack = TRUE
            )
            
          } else if (missing_prop >= 0.10) {
            
            openxlsx::addStyle(
              wb,
              "Missing Data",
              amber_fill,
              rows = excel_row,
              cols = seq_along(missing_data_results),
              gridExpand = TRUE,
              stack = TRUE
            )
          }
        }
      }
    }
  }
  # -----------------------------
  # Duplicate / Similarity Detection
  # -----------------------------
  
  if (!is.null(similarity_results)) {
    
    openxlsx::addWorksheet(wb, "Similar Records")
    
    openxlsx::writeData(
      wb,
      "Similar Records",
      similarity_results
    )
    
    openxlsx::addStyle(
      wb,
      "Similar Records",
      sheet_header_style,
      rows = 1,
      cols = seq_along(similarity_results),
      gridExpand = TRUE
    )
    
    openxlsx::freezePane(
      wb,
      "Similar Records",
      firstRow = TRUE
    )
    
    openxlsx::addFilter(
      wb,
      "Similar Records",
      rows = 1,
      cols = seq_along(similarity_results)
    )
    
    openxlsx::setColWidths(
      wb,
      "Similar Records",
      cols = seq_along(similarity_results),
      widths = "auto"
    )
    
    if ("similarity" %in% names(similarity_results) &&
        nrow(similarity_results) > 0) {
      
      for (i in seq_len(nrow(similarity_results))) {
        
        excel_row <- i + 1
        similarity <- similarity_results$similarity[i]
        
        if (!is.na(similarity)) {
          
          if (similarity >= 0.95) {
            
            openxlsx::addStyle(
              wb,
              "Similar Records",
              red_fill,
              rows = excel_row,
              cols = seq_along(similarity_results),
              gridExpand = TRUE,
              stack = TRUE
            )
            
          } else if (similarity >= 0.80) {
            
            openxlsx::addStyle(
              wb,
              "Similar Records",
              amber_fill,
              rows = excel_row,
              cols = seq_along(similarity_results),
              gridExpand = TRUE,
              stack = TRUE
            )
          }
        }
      }
    }
  }
  
  # -----------------------------
  # Decimal Preference Analysis
  # -----------------------------
  
  if (!is.null(decimal_preference_results)) {
    
    openxlsx::addWorksheet(wb, "Decimal Preference")
    
    openxlsx::writeData(
      wb,
      "Decimal Preference",
      decimal_preference_results
    )
    
    openxlsx::addStyle(
      wb,
      "Decimal Preference",
      sheet_header_style,
      rows = 1,
      cols = seq_along(decimal_preference_results),
      gridExpand = TRUE
    )
    
    openxlsx::freezePane(
      wb,
      "Decimal Preference",
      firstRow = TRUE
    )
    
    openxlsx::addFilter(
      wb,
      "Decimal Preference",
      rows = 1,
      cols = seq_along(decimal_preference_results)
    )
    
    openxlsx::setColWidths(
      wb,
      "Decimal Preference",
      cols = seq_along(decimal_preference_results),
      widths = "auto"
    )
    
    if ("flag_decimal_preference" %in% names(decimal_preference_results) &&
        nrow(decimal_preference_results) > 0) {
      
      for (i in seq_len(nrow(decimal_preference_results))) {
        
        excel_row <- i + 1
        
        if (isTRUE(decimal_preference_results$flag_decimal_preference[i])) {
          
          openxlsx::addStyle(
            wb,
            "Decimal Preference",
            red_fill,
            rows = excel_row,
            cols = seq_along(decimal_preference_results),
            gridExpand = TRUE,
            stack = TRUE
          )
        }
      }
    }
  }

  openxlsx::saveWorkbook(
    wb,
    file,
    overwrite = TRUE
  )
}

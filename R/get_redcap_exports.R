#Title: Get REDCap Exports Function
#Author: Paigan Aspinall
#Version & Date: V1.0.0 27APR2026
#R version: 4.4.3

#' Export multiple REDCap reports using metadata
#'
#' This function iterates over a metadata table of REDCap report definitions
#' and programmatically exports each report via the REDCap API. Export
#' parameters such as report ID and output formatting are derived from the
#' supplied metadata. Each exported dataset is written to disk as a CSV file
#' with a standardised filename incorporating the study name, report name,
#' and execution date.
#'
#' The function supports different REDCap environments (e.g. test vs live)
#' and automatically maps metadata-defined export formats to the appropriate
#' API parameters (\code{rawOrLabel} and \code{rawOrLabelHeaders}).
#'
#' @param export_metadata A data frame containing export definitions. Must
#' include the following columns:
#' \itemize{
#'   \item \code{export_id}: REDCap report ID
#'   \item \code{export_name}: descriptive name for the export
#'   \item \code{format}: export format specification (\code{"raw"},
#'   \code{"labelled"}, or \code{"label_headers"})
#' }
#'
#' @param token A character string containing the REDCap API token with
#' sufficient privileges to export the specified reports.
#'
#' @param environment A character string specifying the target REDCap
#' environment. Must be either \code{"live"} or \code{"test"}. Determines
#' the API endpoint used for the export.
#'
#' @param study_name A character string used as a prefix for output file
#' names to identify the study.
#'
#' @return Invisibly returns \code{NULL}. The primary side effect is the
#' creation of CSV files in the working directory.
#'
#' @details
#' For each row in \code{export_metadata}, the function constructs an API
#' request using \code{httr::POST()} and retrieves the corresponding report
#' in CSV format. The \code{format} column is used to determine how raw
#' values and labels are handled:
#' \itemize{
#'   \item \code{"raw"}: raw values for both data and headers
#'   \item \code{"labelled"}: labelled values for both data and headers
#'   \item \code{"label_headers"}: raw data with labelled headers
#' }
#'
#' Output files are named using the convention:
#' \preformatted{
#' study_name_export_name_YYYY-MM-DD.csv
#' }
#'
#' If an API request fails (non-200 HTTP status), a warning is issued and
#' processing continues with the next export.
#'
#' @examples
#' \dontrun{
#' get_redcap_exports(
#'   export_metadata = export_metadata,
#'   token = token,
#'   environment = "live",
#'   study_name = "MyStudy"
#' )
#' }
#'
#' @importFrom httr POST content status_code
#' @export

get_redcap_exports <- function(export_metadata,
                               token,
                               environment,
                               study_name) {
  if(environment=="live"){
    url <- "https://clinicaltrials.plymouth.ac.uk/api/"
  } else {
    if(environment=="test"){
      url <- "https://clinicaltrials.pre-plymouth.ac.uk/api/"
    }}
  
  today <- Sys.Date()
  
  for (i in seq_len(nrow(export_metadata))) {
    
    export_id <- export_metadata$export_id[i]
    export_name <- export_metadata$export_name[i]
    export_format <- export_metadata$format[i]
    
    raw_or_label <- ifelse(
      export_format %in% c("raw", "label_headers"),
      "raw",
      "label"
    )
    
    raw_or_label_headers <- ifelse(
      export_format == "raw",
      "raw",
      "label"
    )
    
    formData <- list(
      token = token,
      content = "report",
      format = "csv",
      report_id = export_id,
      csvDelimiter = "",
      rawOrLabel = raw_or_label,
      rawOrLabelHeaders = raw_or_label_headers,
      exportCheckboxLabel = "false",
      returnFormat = "json"
    )
    
    response <- httr::POST(url, body = formData, encode = "form")
    
    if (httr::status_code(response) != 200) {
      warning(paste("Export failed for", export_name, "- status code:", httr::status_code(response)))
      next
    }
    
    result <- httr::content(response, as = "text", encoding = "UTF-8")
    
    file_name <- paste0(
      study_name, "_",
      export_name, "_",
      today,
      ".csv"
    )
    
    writeLines(result, file_name)
    
    message("Saved: ", file_name)
  }
}
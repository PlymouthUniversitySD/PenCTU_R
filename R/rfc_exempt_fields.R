#Title: Reason for change - fields that do not need a reason
#Author: Paigan Aspinall 
#Version & Date: V1.0.0 04NOV2026

#' Fields that don't need a reason for change
#'
#' Reads a REDCap data dictionary and lists the fields that PenCTU_ReasonForChange.js should not ask a
#' reason for: calculated fields, @CALCTEXT and @CALCDATE fields, and fields whose annotation contains
#' #DataManagement, #TrialManagement or #SystemFunctionality (anywhere in the annotation). Prints the
#' list as a line to paste over the STUDY_EXEMPT_FIELDS setting at the top of the script.
#'
#' @param metadata The data dictionary: a data frame from the API metadata export (field_name, field_type,
#'   field_annotation) or from the Data Dictionary download ("Variable / Field Name", "Field Type",
#'   "Field Annotation"), or the path to the downloaded CSV.
#' @return Invisibly, the field names.
#' @export
rfc_exempt_fields <- function(metadata) {
  if (is.character(metadata) && length(metadata) == 1) {
    metadata <- utils::read.csv(metadata, colClasses = "character", check.names = FALSE, fileEncoding = "UTF-8-BOM")
  }
  names(metadata)[names(metadata) == "Variable / Field Name"] <- "field_name"
  names(metadata)[names(metadata) == "Field Type"] <- "field_type"
  names(metadata)[names(metadata) == "Field Annotation"] <- "field_annotation"
  if (!all(c("field_name", "field_type", "field_annotation") %in% names(metadata))) {
    stop("metadata must be a REDCap data dictionary, with field name, field type and field annotation.")
  }
  ann <- ifelse(is.na(metadata$field_annotation), "", metadata$field_annotation)
  exempt <- metadata$field_type %in% "calc" |
    grepl("@CALCTEXT|@CALCDATE", ann) |
    grepl("#DataManagement|#TrialManagement|#SystemFunctionality", ann)
  fields <- metadata$field_name[exempt]
  cat("    const STUDY_EXEMPT_FIELDS = [", paste0("'", fields, "'", collapse = ", "), "];\n", sep = "")
  invisible(fields)
}

# Author: Matthew Bailey and Lexy Sorrell
# Date: 24MAR2023
# R version: 4.2.2

#' Check Field Completeness of a Data Frame
#'
#' This function calculates the completeness of each field in a given data frame.
#'
#' @param data A data frame to check the completeness of its fields.
#' @param by The column name as a string to group the data by before checking field completeness (optional).
#' @param full A logical value that determines whether to return completeness information for missing values and a set of predefined codes (default: FALSE).
#' @return A data frame with the completeness of each field.
#' @import dplyr
#' @importFrom dplyr group_by summarize across mutate everything sym
#' @export
check_field_completeness <- function(data, by = NULL, full = FALSE) {
  
  if(is.null(data)){
    stop("Data frame not provided!")
  } else if(!is.null(data)){
    stopifnot(is.data.frame(data))
    
    if (!is.null(by)) {
      data <- data %>% 
        dplyr::mutate(!!by := as.character(!!dplyr::sym(by))) %>%
        dplyr::group_by(!!dplyr::sym(by))
    }
    
    NAs <- data %>%
      dplyr::summarize(across(dplyr::everything(), ~ paste0(sum(is.na(.)), "/", n(), " (", round(sum(is.na(.)) / n() * 100), "%)"))) %>%
      dplyr::mutate(Value = c('NA'), .before = dplyr::everything())
    
    
    if (full) {
      codes <- c(444, 555, 666, 777, 888, 999)
      values <- c('444 - Not applicable', '555 - Other', '666 - Prefer not to say', '777 - Missing', '888 - Spoiled', '999 - Unknown')
      lst <- lapply(codes, function(code) {
        data %>%
          summarise(across(everything(), ~paste0(sum(. == code, na.rm = TRUE), "/", n(), " (", round(sum(is.na(.)) / n() * 100), "%)"))) %>%
          mutate(Value = c(values[which(codes == code)]), .before = everything())
      })
      
      return(do.call(rbind, c(list(NAs), lst)))
    }
    
    return(NAs)
  }
}
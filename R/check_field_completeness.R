# Author: Matthew Bailey and Lexy Sorrell
# Date: 24MAR2023
# R version: 4.2.2

library(dplyr) #1.1.0
library(purrr) #1.0.1

#' Check Field Completeness of a Data Frame
#'
#' This function calculates the completeness of each field in a given data frame.
#'
#' @param data A data frame to check the completeness of its fields.
#' @param by The column name as a string to group the data by before checking field completeness. This argument is optional.
#' @param full A boolean that determines whether to return completeness information for missing values and a set of predefined codes. This argument is optional, and its default value is \code{FALSE}.
#'
#' @return A data frame with the completeness of each field.
#'
#'
#' @importFrom dplyr "%>%"
#'
#' @export
check_field_completeness <- function(data, by = NULL, full = FALSE) {
  if (!is.null(by)) {
    data <- data %>% 
      mutate(!!by := as.character(!!sym(by))) %>%
      group_by(!!sym(by))
  }
  
  NAs <- data %>%
    summarise(across(everything(), ~sum(is.na(.)))) %>%
    mutate(across(where(is.numeric), ~paste0(.x, '/', nrow(data), ' (', round(.x * 100 / nrow(data)), '%)'))) %>%
    mutate(Value = c('NA'), .before = everything())
  
  if (full) {
    codes <- c(444, 555, 666, 777, 888, 999)
    values <- c('444 - Not applicable', '555 - Other', '666 - Prefer not to say', '777 - Missing', '888 - Spoiled', '999 - Unknown')
    lst <- lapply(codes, function(code) {
      data %>%
        summarise(across(everything(), ~sum(. == code, na.rm = TRUE))) %>%
        mutate(across(where(is.numeric), ~paste0(.x, '/', nrow(data), ' (', round(.x * 100 / nrow(data)), '%)'))) %>%
        mutate(Value = c(values[which(codes == code)]), .before = everything())
    })
    
    return(do.call(rbind, c(list(NAs), lst)))
  }
  
  return(NAs)
}
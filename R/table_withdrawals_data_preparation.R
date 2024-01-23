# Author: Paigan Aspinall
# Date: 23JAN2024
# R version: 4.2.2
#' Prepare a dataset from which to plot a withdrawals table.
#'
#' This function removed early discontinuations (if required) and selects only the required withdrawal data.
#'
#' @param dataset The dataset where the withdrawal data is being pulled from.
#' @param withdrawal_date_column The column in the dataset where the date of the withdrawal is specified.
#' @param withdrawal_reason_column The column in the dataset where the reason for the withdrawal is specified.
#' @param withdrawal_event_name The name of the event where withdrawals are recorded.
#' @param allocation Logical, indicating whether the allocation data should be included in the output. Default is FALSE.
#' @param early_discontinuation_column Name of the column where whether the withdrawal is an early discontinuation is specified. Default is NULL.
#' @param full_withdrawal_option The response option for early_discontinuation_column that is selected for a full withdrawal only. Default is NULL.

#'
#' @return A data frame containing withdrawal data.
#'
#' @details This function removes early discontinuation data (if required) and selects only the required data
#' from the dataset to produce a withdrawal table.
#'
#' @import dplyr

#' @examples
#' # Example usage:
#' withdrawal_table_data <- table_withdrawals_data_preparation(dataset, "wiwithdrawdat", "wireason", "withdraw_arm_1", allocation = TRUE,
#'                           "wiearlydiscon", "Entire study")
#' Example downstream usage: 
#'withdrawals_table <- flextable(withdrawal_table_data) %>%
#'  set_table_properties(width = .75, layout = "autofit")
#'  
#'  @export

table_withdrawals_data_preparation <- function(dataset, withdrawal_date_column, withdrawal_reason_column, withdrawal_event_name, allocation = FALSE,
                                              early_discontinuation_column = NULL, full_withdrawal_option = NULL){
  
  processdataset <- subset(dataset, redcap_event_name == withdrawal_event_name)
  
  if (!is.null(early_discontinuation_column) && !is.null(full_withdrawal_option)) {
    subset_condition <- processdataset[[early_discontinuation_column]] == full_withdrawal_option
    processdataset <- processdataset[subset_condition, ]
  }
  
  selected_columns <- c("record_id", "Site", withdrawal_date_column, withdrawal_reason_column)  
  if(allocation){
    selected_columns <- c(selected_columns[1:2], "Allocation", selected_columns[3:4]) 
  }
  processdataset <- select(processdataset, all_of(selected_columns))
  
  names(processdataset)[which(names(processdataset) == withdrawal_date_column)] <- "Withdrawal date"  
  names(processdataset)[which(names(processdataset) == withdrawal_reason_column)] <- "Withdrawal reason" 
  names(processdataset)[names(processdataset) == "record_id"] <- "Participant ID"

  return(processdataset)
  
}


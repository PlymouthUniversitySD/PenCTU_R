# Author: Matthew Bailey
# Date: 08MAR2023
# R version: 4.2.2

library(tidyverse) #1.3.2
library(expss) #0.11.4

#' Add variable labels to data from a data dictionary
#'
#' This function checks the data dictionary (dictionary_df) for any variable names which match those in the data (data_df). It then assigns the label from dictionary_df to the variables in data_df
#' @param data_df The data frame to add labels to
#' @param dictionary_df The data dictionary containing variable names and data labels
#' @param var_names_str A string containing the name of the column in dictionary_df which contains the variable names
#' @param var_labels_str A string containing the name of the column in dictionary_df which contains the corresponding data labels
#' @return The original data frame with labels assigned from the data dictionary provided
#' @examples
#' from_rcc <- labels_from_dictionary(data_df = export, dictionary_df = data_dictionary, var_names_str= "Variable...Field.Name", var_labels_str = "Field.Label")
#' @export
labels_from_dictionary <- function(data_df, dictionary_df, var_names_str, var_labels_str) {
  tryCatch({
    
    for(i in 1:nrow(dictionary_df)) {
      if(!is.null(data_df[[dictionary_df[[var_names_str]][i]]])){
        
        var_lab(data_df[[dictionary_df[[var_names_str]][i]]]) = dictionary_df[[var_labels_str]][i]
      }
    }
  },
  error = function(e) {
    warning("Please ensure var_names_str and var_labels_str match column names in dictionary_df")
    message("error:\n", e)
  },
  finally = {
    return(data_df)
  })
}
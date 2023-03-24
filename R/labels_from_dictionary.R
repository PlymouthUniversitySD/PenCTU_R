# Author: Matthew Bailey
# Date: 17MAR2023
# R version: 4.2.2

library(dplyr) #1.1.0
library(expss) #0.11.4
library(stringr) #1.5.0
library(haven) #2.5.1

#' Add variable labels to data from a data dictionary
#'
#' This function checks the data dictionary (dictionary_df) for any variable names which match those in the data (data_df). It then assigns the label from dictionary_df to the variables in data_df
#' @param data_df The data frame to add labels to
#' @param dictionary_df The data dictionary containing variable names and data labels
#' @param var_names_str A string containing the name of the column in dictionary_df which contains the variable names
#' @param var_labels_str A string containing the name of the column in dictionary_df which contains the corresponding data labels
#' @param var_choices_str (Optional) A string containing the name of the column in dictionary_df which contains the response options
#' @param split_by (Optional, default = ' \\| ') The string to separate response options
#' @param stata_path (Optional) A string containing the file path of the location where the stata file should be saved
#' @return The original data frame with labels assigned from the data dictionary provided
#' @usage 
#' labels_from_dictionary(data_df = export, dictionary_df = data_dictionary, var_names_str= "Variable...Field.Name", var_labels_str = "Field.Label", split_by= ' \\| ', stata_path = NULL)
#' @importFrom dplyr "%>%"
#' @export
labels_from_dictionary <- function(data_df, dictionary_df, var_names_str, var_labels_str, var_choices_str = NULL, split_by = ' \\| ', stata_path = NULL) {
  tryCatch({
    
    for(i in 1:nrow(dictionary_df)) {
      if(!is.null(data_df[[dictionary_df[[var_names_str]][i]]])){
        
        if(!is.null(var_choices_str) & !is.na(dictionary_df[[var_choices_str]][i])){
          #String split 
          value <- stringr::str_remove_all(dictionary_df[[var_choices_str]][i], '\"') %>% #Remove any quote marks
            stringr::str_remove_all('\"') %>% 
            stringr::str_split_1(split_by) %>%
            stringr::str_trim() %>% 
            stringr::str_split(", ", n=2)
          
          data_df[[dictionary_df[[var_names_str]][i]]] <- factor(data_df[[dictionary_df[[var_names_str]][i]]], 
                                                                 levels = sapply(value,"[[",1), 
                                                                 labels = sapply(value,"[[",2)) #Assign value labels
        }
        
        data_df[data_df == "NULL"] <- NA
        
        expss::var_lab(data_df[[dictionary_df[[var_names_str]][i]]]) = dictionary_df[[var_labels_str]][i] #Assign variable label
        
        if(!is.null(stata_path)){
          haven::write_dta(data_df, stata_path)
        }
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
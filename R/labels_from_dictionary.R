# Author: Matthew Bailey
# Date: 24MAY2023
# R version: 4.2.2

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
#' @import dplyr
#' @importFrom stringr str_remove_all str_split_1 str_trim
#' @importFrom expss var_lab
#' @importFrom haven write_dta
#' @export
labels_from_dictionary <- function(data_df, dictionary_df, var_names_str, var_labels_str, var_choices_str = NULL, split_by = ' \\| ', stata_path = NULL) {
 
  if(is.null(data_df)) {
    stop("Dataset is null!")
  }
  
  if(is.null(dictionary_df)) {
    stop("Data dictionary is null")
  }
  
  stopifnot(is.data.frame(data_df))
  stopifnot(is.data.frame(dictionary_df))
  
  if(is.null(var_names_str)) {
    stop("Name of variable names column not provided!")
  }
  
  if(is.null(var_labels_str)) {
    stop("Name of labels column not provided!")
  }
  
  dictionary_df <- dplyr::mutate(dictionary_df, across(everything(), ~ ifelse(. == '', NA, .))) # Ensure blank dictionary items are marked as NA
  
  for (i in 1:nrow(dictionary_df)) {
    var_name <- dictionary_df[[var_names_str]][i]
    
    if (!is.null(data_df[[var_name]])) {
      if (!is.null(var_choices_str) && !is.na(dictionary_df[[var_choices_str]][i])) {
        
          value <- stringr::str_remove_all(dictionary_df[[var_choices_str]][i], '\"') %>% 
            stringr::str_remove_all('\"') %>% 
            stringr::str_split_1(split_by) %>% 
            stringr::str_trim() %>% 
            stringr::str_split(", ", n = 2)
          
          levels <- sapply(value, `[[`, 1)
          labels <- sapply(value, `[[`, 2)
          
          data_df[[var_name]] <- factor(data_df[[var_name]], levels = levels, labels = labels)
      }
      
      data_df[data_df == "NULL"] <- NA
      print(colnames(data_df)[i])
      if(
          (colnames(data_df)[i] != "redcap_event_name") && 
          (colnames(data_df)[i] != "redcap_repeat_instrument") &&
          (colnames(data_df)[i] != "redcap_repeat_instance") &&
          (colnames(data_df)[i] != "redcap_data_access_group")
      ) {
        if (colnames(data_df)[i] %in% dictionary_df[[var_names_str]] == TRUE) {
          expss::var_lab(data_df[[var_name]]) <- dictionary_df[[var_labels_str]][i] # Assign variable label
          ##print(data_df[[var_name]])
        } else {
          stop(paste0(colnames(data_df)[i], " is not present in data dictionary!"))
        }
      }

      if (!is.null(stata_path)) {
        haven::write_dta(data_df, stata_path)
      }
    }
  }
  
  return(data_df)
  
}
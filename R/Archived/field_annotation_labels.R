# Author: Paigan Aspinall
# Date: 20SEP2024
# R version: 4.2.2
#'
#' Add variable labels to data from field annotations column
#'
#' @param dataset The dataset to which the labels are being added
#' @param data_dictionary The dataset which contains the field_annotation column
#' 
#' @return A dataframe with labelled columns
#' 
#' @details This function formats the field annotation column to remove action tags then loops through the data dictionary to assign these field annotations as labels to the dataset.
#' 
#' @import Hmisc
#' 
#' @examples labelled_dataset <- field_annotation_labels(your_dataset, your_data_dictionary)
#' 
#' @export

field_annotation_labels <- function(dataset, data_dictionary){
  if(is.null(dataset)) {
    stop("Dataset not provided!")
  }
  
  if(is.null(data_dictionary)) {
    stop("Data dictionary not provided!")
  }
  
  #Remove any action tags from the field annotation column
  data_dictionary$field_annotation <- sub(" @.*", "", data_dictionary$field_annotation)
  
  #Loop through the data dictionary field_annotation column to add the labels to the dataset
  for (i in seq_along(names(dataset))) {
    column_name <- names(dataset)[i]
    
    annotation <- data_dictionary$field_annotation[data_dictionary$field_name == column_name]
    
    if (length(annotation) > 0 && !is.na(annotation)) {
      var_label(dataset[[column_name]]) <- as.character(annotation)
    }
  }
  
  #Return the labelled dataset
  return(dataset)
}


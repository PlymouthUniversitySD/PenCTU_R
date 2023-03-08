# Author: Matthew Bailey
# Date: 24FEB2023
# R version: 4.2.2

library(dplyr) #1.1.0
library(tidyr) #1.3.0
library(tidyverse) #1.3.2
library(purrr) #1.0.1

#' Extract json from data frame column
#'
#' This function takes a data frame and extracts all json properties, from all JSON objects in a specified column, creates a new column for each property and populates the correct value of the property for each row. Also allows user to specify which JSON properties they are interested in - all others will be discarded.
#' @param data The data frame which contains a column of json data
#' @param col_name The name of the column containing the json data e.g. "redcap_record_metadata"
#' @param prop_list A vector containing JSON properties that the user would like to extract from the json column (col_name) e.g. c("column_name_1", "another_column_name"), if not specified returns all extracted json properties. 
#' @return The original data frame with each JSON property becoming its own column and the original json column (col_nam) being removed
#' @examples
#' example1 <- extract_json_from_column(data = df, col_name = "JSON")
#' example2 <- extract_json_from_column(data = df, col_name = "JSON", prop_list = c("event.name", "event.status"))
#' @export
extract_json_from_column <- function(data, col_name, prop_list = NULL) {
  tryCatch({
    temp <- data %>%
      mutate(json_data = map(data[[col_name]], ~ unlist(jsonlite::fromJSON(.))) %>% bind_rows) %>% #Creates a new column (json_data) with unlisted JSON data
      unnest(json_data) %>% #Un-nests json_data so that each JSON object becomes its own column
      select(-col_name) #Removes original JSON column (col_name)

    if (is.null(prop_list)) {
      result <- temp
    } else {
      result <- cbind(data, temp[, prop_list]) %>%
        dplyr::select(-col_name) #Removes original JSON column (col_name)
    }
  },
  error = function(e) {
    warning("PenCTU error: Ensure col_name column contains only JSON objects")
    message("error:\n", e)
  },
  finally = {
    return(result)
  })
}
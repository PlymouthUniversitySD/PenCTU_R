# Author: Matthew Bailey
# Date: 21FEB2023
# R version: 4.2.2

library(tidyverse) #1.3.2

#' Extract JSON from dataframe column
#'
#' Takes a dataframe with a JSON object in one of its columns and unnests the JSON so that each JSON property becomes its own unique column. Also extracts values corresponding to each row.
#' @param data A dataset with a column that contains only JSON objects
#' @return The input dataset with the JSON column unnested and each JSON property
#' @examples
#' example1 <- extract_JSON_from_column(data, "column_name_as_string");
#' @export
extract_JSON_from_column <- function(data, col_name) {
  tryCatch({
    result <- data %>%
      mutate(json_data = map(data[[col_name]], ~ unlist(jsonlite::fromJSON(.))) %>% bind_rows) %>% #Creates a new column (json_data) with unlisted JSON data
      unnest(json_data) %>% #Unnests json_data so that each JSON object becomes its own column
      select(-col_name) #Removes original JSON column (col_name)
  },
  error = function(e) {
    warning("PenCTU error: Ensure col_name column contains only JSON objects")
    message("error:\n", e)
  },
  finally = {
    return(result)
  })
}
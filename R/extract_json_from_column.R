# Author: Matthew Bailey
# Date: 24FEB2023
# R version: 4.2.2

#' Extract JSON properties from a data frame column
#'
#' This function takes a data frame and extracts specified JSON properties from a column of JSON objects. Each property becomes its own column in the resulting data frame.
#'
#' @param data The data frame containing a column of JSON data
#' @param col_name The name of the column containing the JSON data
#' @param prop_list A vector of JSON properties to extract (optional). If not specified, all properties will be extracted.
#' @return The data frame with each JSON property as a separate column, and the original JSON column removed
#' @import dplyr
#' @import tidyr
#' @import purrr
#' @import jsonlite
#' @importFrom jsonlite fromJSON
#' @export
extract_json_from_column <- function(data, col_name, prop_list = NULL) {
  stopifnot(is.data.frame(data), col_name %in% names(data))
  
  tryCatch({
    temp <- data %>%
      dplyr::mutate(json_data = purrr::map(data[[col_name]], ~ unlist(jsonlite::fromJSON(.))) %>% bind_rows) %>% #Creates a new column (json_data) with unlisted JSON data
      tidyr::unnest(json_data) %>% #Un-nests json_data so that each JSON object becomes its own column
      dplyr::select(-col_name) #Removes original JSON column (col_name)

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
# Author: Paigan Aspinall, Matthew Bailey
# Date: 06JUN2023
# R version: 4.2.2

#' Export CRF Data from RCC
#'
#' This function exports data from the RedCap Cloud (RCC) system for a specific
#' CRF (Case Report Form) using the provided CRF name and access token.
#'
#' @param crf_name The name of the CRF to export data from.
#' @param token The access token for authentication with RCC.
#' @return A dataframe containing the exported CRF data.
#' @export
#' @examples
#' export_crf_data_from_RCC("CRF_Name", "access_token")
#' 
#' @import dplyr
#' @importFrom jsonlite fromJSON

#' @import httr
export_crf_data_from_RCC <- function(crf_name, token, env) {
  stopifnot(is.character(crf_name))
  stopifnot(is.character(token))
  
  tryCatch({
    
    rcc_url <- paste0("https://plymouth.", env, ".redcapcloud.com/rest/v2/")
    
    # Get CRF ID from crf_name
    crfs <- httr::GET(
      url = paste0(rcc_url, "crfs"),
      httr::add_headers(Token = token, Accept = "application/json")
    ) %>%
      httr::content(as = "text", encoding = "UTF-8") %>%
      jsonlite::fromJSON() %>%
      dplyr::filter(stringr::str_detect(name, crf_name)) %>%
      dplyr::pull(id) %>%
      dplyr::first()
    
    # Get all CRF items
    unpacknames <- httr::GET(
      url = paste0(rcc_url, "crf-items"),
      httr::add_headers(Token = token, Accept = "application/json")
    ) %>%
      httr::content(as = "text", encoding = "UTF-8") %>%
      jsonlite::fromJSON()
    
    # Subset using crf_id
    subset_unpacknames_nonrepeating <- unpacknames %>%
      dplyr::filter(item$crfId == crfs & isRemoved == FALSE & itemGroup$addLookupCodeByGroupType == "Non Repeating" & item$adLookupCodeByItemDataTypeId != "Checkboxes")
    
    subset_unpacknames_repeating <- unpacknames %>%
      dplyr::filter(item$crfId == crfs & isRemoved == FALSE & itemGroup$addLookupCodeByGroupType == "Repeating" & item$adLookupCodeByItemDataTypeId != "Checkboxes")
    
    if (nrow(subset_unpacknames_repeating) > 0) {
      subset_unpacknames_repeating$item$variableName <- paste0(subset_unpacknames_repeating$item$variableName, "(1)")
    }
    
    addComplete <- function(input_string) {
      lowercase_string <- stringr::str_to_lower(input_string)
      complete_string <- paste0(lowercase_string, "_complete")
      return(complete_string)
    }
    
    crfnames_nonrepeating <- subset_unpacknames_nonrepeating$item$variableName
    crfnames_repeating <- subset_unpacknames_repeating$item$variableName
    
    expected_columns <- c(
      "participantId", "eventName", "siteName", "startedDate", "completedDate",
      "eventDate", crfnames_nonrepeating, crfnames_repeating, addComplete(crf_name)
    )
    
    res <- httr::POST(
      url = paste0(rcc_url, "export/records"),
      body = list(crfForms = list(crf_name)),
      encode = "json",
      httr::add_headers(Token = token, Accept = "application/json")
    )
    
    df <- httr::content(res, as = "text", encoding = "UTF-8") %>%
      jsonlite::fromJSON() %>%
      tidyr::unnest(df$items) %>%
      tidyr::pivot_wider(
        id_cols = c(participantId, eventName, siteName, startedDate, completedDate, eventDate),
        names_from = itemName,
        values_from = itemValue
      )
    
    current_columns <- colnames(df)
    missing_columns <- setdiff(expected_columns, current_columns)
    df[missing_columns] <- NA
    
    result <- df %>%
      dplyr::group_by(participantId, eventName) %>%
      dplyr::summarise(across(everything(), ~na.omit(.)[1])) %>%
      dplyr::mutate(
        eventDate = as.Date(eventDate),
        completedDate = as.Date(completedDate),
        startedDate = as.Date(startedDate)
      )
    
    return(result)
  },
  error = function(e) {
    message("PenCTU error:", e$message)
    return(NULL)
  })
}
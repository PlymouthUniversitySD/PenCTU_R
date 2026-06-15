#Title: ANON - Recode site identifiers
#Author: Paigan Aspinall
#Version & Date: V1.0.0 01JUN2026
#R version: 4.4.3

#' Recode Site Identifiers
#'
#' Replaces site names or REDCap Data Access Group values with randomly
#' generated anonymised site identifiers.
#'
#' This function is designed for REDCap datasets where site information is
#' commonly stored in \code{redcap_data_access_group}. The function can also use
#' an existing linkage file so that site identifiers are recoded consistently
#' across multiple datasets.
#'
#' If a site already exists in the linkage file, the existing anonymised site ID
#' is reused. If a site is not present in the linkage file, a new random
#' anonymised site ID is generated.
#'
#' @param dataset A data frame containing the REDCap dataset.
#' @param site_field Character string specifying the site field in the dataset.
#'   Default is \code{"redcap_data_access_group"}.
#' @param prefix Character string used at the start of each anonymised site ID.
#'   Default is \code{"SITE"}.
#' @param id_length Integer specifying the length of the random part of the
#'   anonymised site ID. Default is \code{6}.
#' @param linkage_file Optional data frame containing an existing site linkage
#'   file. Expected columns are \code{original_site} and \code{anonymised_site}.
#' @param overwrite_original Logical. If \code{TRUE}, the original site field is
#'   overwritten. If \code{FALSE}, a new field is created. Default is
#'   \code{TRUE}.
#' @param new_site_field Character string specifying the new site field when
#'   \code{overwrite_original = FALSE}. Default is \code{"anonymised_site"}.
#'
#' @return A list containing:
#'   \describe{
#'     \item{dataset}{Dataset with recoded site identifiers}
#'     \item{linkage_file}{Site linkage file}
#'     \item{change_log}{Summary of site recoding}
#'   }
#'
#' @examples
#' result <- recode_site(
#'   dataset = erase_data,
#'   site_field = "redcap_data_access_group",
#'   prefix = "SITE",
#'   id_length = 6
#' )
#'
#' anonymised_data <- result$dataset
#' site_linkage <- result$linkage_file
#'
#' @export
recode_site <- function(dataset,
                        site_field = "redcap_data_access_group",
                        prefix = "SITE",
                        id_length = 6,
                        linkage_file = NULL,
                        overwrite_original = TRUE,
                        new_site_field = "anonymised_site") {
  
  generate_random_ids <- function(n, prefix, id_length, existing_ids = character()) {
    
    allowed_characters <- c(LETTERS, 0:9)
    generated_ids <- character()
    
    while (length(generated_ids) < n) {
      
      candidate <- paste0(
        prefix,
        paste0(
          sample(
            allowed_characters,
            size = id_length,
            replace = TRUE
          ),
          collapse = ""
        )
      )
      
      if (!candidate %in% c(existing_ids, generated_ids)) {
        generated_ids <- c(generated_ids, candidate)
      }
    }
    
    generated_ids
  }
  
  if (!is.data.frame(dataset)) {
    stop("dataset must be a data frame.", call. = FALSE)
  }
  
  if (!site_field %in% names(dataset)) {
    stop(
      paste0("site_field not found in dataset: ", site_field),
      call. = FALSE
    )
  }
  
  if (!is.numeric(id_length) || length(id_length) != 1 || id_length < 1) {
    stop("id_length must be a positive number.", call. = FALSE)
  }
  
  original_sites <- sort(unique(stats::na.omit(as.character(dataset[[site_field]]))))
  
  if (is.null(linkage_file)) {
    
    linkage_file <- tibble::tibble(
      original_site = character(),
      anonymised_site = character()
    )
    
  } else {
    
    if (!is.data.frame(linkage_file)) {
      stop("linkage_file must be a data frame if supplied.", call. = FALSE)
    }
    
    required_linkage_columns <- c(
      "original_site",
      "anonymised_site"
    )
    
    missing_linkage_columns <- setdiff(
      required_linkage_columns,
      names(linkage_file)
    )
    
    if (length(missing_linkage_columns) > 0) {
      stop(
        paste0(
          "linkage_file is missing required columns: ",
          paste(missing_linkage_columns, collapse = ", ")
        ),
        call. = FALSE
      )
    }
    
    linkage_file <- linkage_file |>
      dplyr::mutate(
        original_site = as.character(.data$original_site),
        anonymised_site = as.character(.data$anonymised_site)
      )
  }
  
  existing_original_sites <- linkage_file$original_site
  new_original_sites <- setdiff(original_sites, existing_original_sites)
  
  if (length(new_original_sites) > 0) {
    
    new_anonymised_sites <- generate_random_ids(
      n = length(new_original_sites),
      prefix = prefix,
      id_length = id_length,
      existing_ids = linkage_file$anonymised_site
    )
    
    new_linkage_rows <- tibble::tibble(
      original_site = new_original_sites,
      anonymised_site = new_anonymised_sites
    )
    
    linkage_file <- dplyr::bind_rows(
      linkage_file,
      new_linkage_rows
    )
  }
  
  site_lookup <- stats::setNames(
    linkage_file$anonymised_site,
    linkage_file$original_site
  )
  
  updated_dataset <- dataset
  
  recoded_sites <- unname(
    site_lookup[as.character(updated_dataset[[site_field]])]
  )
  
  if (overwrite_original) {
    updated_dataset[[site_field]] <- recoded_sites
    output_field <- site_field
  } else {
    updated_dataset[[new_site_field]] <- recoded_sites
    output_field <- new_site_field
  }
  
  change_log <- tibble::tibble(
    action = "recode_site",
    site_field = site_field,
    output_field = output_field,
    sites_in_dataset = length(original_sites),
    existing_linkages_reused = sum(original_sites %in% existing_original_sites),
    new_linkages_created = length(new_original_sites),
    linkage_rows_total = nrow(linkage_file)
  )
  
  list(
    dataset = updated_dataset,
    linkage_file = linkage_file,
    change_log = change_log
  )
}

#Title: QTL Monitoring
#Author: Paigan Aspinall
#Version & Date: V1.0.0 08JUN2026
#R version: 4.4.3

#' Run QTL Monitoring
#'
#' Calculates Quality Tolerance Limit (QTL) values using a metadata-driven
#' specification. Each row of the metadata dataset defines one QTL to be
#' evaluated, including the QTL identifier, source dataset, REDCap event,
#' field(s), positive condition, conditional denominator logic, and RAG
#' threshold bands.
#'
#' The function currently supports the following QTL identifiers:
#' \itemize{
#'   \item \code{"timely_recruitment"}
#'   \item \code{"participant_retention"}
#'   \item \code{"endpoint_completeness"}
#'   \item \code{"total_saes"}
#'   \item \code{"related_saes"}
#'   \item \code{"imp_compliance"}
#' }
#'
#' QTLs are only calculated where the relevant \code{qtl_id} is present in the
#' metadata dataset. Therefore, the metadata may contain all supported QTLs or
#' only a subset of QTLs relevant to a specific study or monitoring report.
#'
#' The \code{"timely_recruitment"} QTL must be present where other QTLs require
#' the total number of recruited participants as their denominator, including:
#' \itemize{
#'   \item \code{"participant_retention"}
#'   \item \code{"total_saes"}
#'   \item \code{"related_saes"}
#' }
#'
#' Source datasets are identified from the \code{export_name} column in the
#' metadata. The named datasets must exist in the supplied environment.
#'
#' @param metadata A data frame containing the QTL specification. Expected
#'   columns include:
#'   \describe{
#'     \item{qtl_description}{Description of the QTL}
#'     \item{qtl_id}{Unique QTL identifier used to determine calculation logic}
#'     \item{green_band}{Green RAG threshold}
#'     \item{amber_band}{Amber RAG threshold}
#'     \item{red_band}{Red RAG threshold}
#'     \item{field}{Field or fields used in the numerator calculation. Multiple
#'       fields should be separated by \code{";"}}
#'     \item{event_name}{REDCap event used for the numerator calculation}
#'     \item{export_name}{Name of the source dataset object}
#'     \item{positive_condition}{Condition defining a positive QTL event}
#'     \item{negative_condition}{Condition defining a negative QTL event}
#'     \item{further_condition}{Additional QTL-specific information, such as
#'       recruitment target or denominator notes}
#'     \item{conditional_event}{REDCap event used to define the denominator,
#'       where applicable}
#'     \item{conditional_field}{Field used to define the denominator or
#'       participant identifier, where applicable}
#'     \item{conditional_logic}{Logic used to define the denominator, where
#'       applicable}
#'   }
#'
#' @param env Environment in which source datasets named in \code{export_name}
#'   can be found. Defaults to \code{parent.frame()}.
#'
#' @return A data frame containing:
#'   \describe{
#'     \item{qtl_id}{Unique QTL identifier}
#'     \item{qtl_name}{QTL description}
#'     \item{qtl_value}{Calculated QTL percentage value}
#'     \item{flag}{RAG flag: \code{"green"}, \code{"amber"}, or \code{"red"}}
#'   }
#'
#' @examples
#' qtl_results <- run_qtl_monitoring(
#'   metadata = qtl_metadata
#' )
#'
#' @export
#' 
run_qtl_monitoring <- function(metadata, env = parent.frame()) {
  
  results <- list()
  recruitment_count <- NA
  
  add_result <- function(qtl_id, qtl_name, qtl_value, flag) {
    data.frame(
      qtl_id = qtl_id,
      qtl_name = qtl_name,
      qtl_value = round(qtl_value, 2),
      flag = flag
    )
  }
  
  get_meta <- function(id) {
    metadata[metadata$qtl_id == id, ][1, ]
  }
  
  has_qtl <- function(id) {
    any(metadata$qtl_id == id)
  }
  
  # timely recruitment
  if (has_qtl("timely_recruitment")) {
    
    meta_row <- get_meta("timely_recruitment")
    data <- get(meta_row$export_name, envir = env)
    
    recruitment_count <- data |>
      dplyr::filter(redcap_event_name == meta_row$event_name) |>
      dplyr::filter(!is.na(.data[[meta_row$field]])) |>
      nrow()
    
    recruitment_target <- as.numeric(
      stringr::str_extract(meta_row$further_condition, "\\d+")
    )
    
    recruitment_qtl_value <- recruitment_count / recruitment_target * 100
    
    recruitment_flag <- dplyr::case_when(
      recruitment_qtl_value >= 90 ~ "green",
      recruitment_qtl_value >= 75 ~ "amber",
      TRUE ~ "red"
    )
    
    results[["timely_recruitment"]] <- add_result(
      meta_row$qtl_id,
      meta_row$qtl_description,
      recruitment_qtl_value,
      recruitment_flag
    )
  }
  
  # participant retention
  if (has_qtl("participant_retention")) {
    
    if (is.na(recruitment_count)) {
      stop("participant_retention requires timely_recruitment to be present first.")
    }
    
    meta_row <- get_meta("participant_retention")
    data <- get(meta_row$export_name, envir = env)
    
    participant_id_col <- names(data)[1]
    
    positive_values <- stringr::str_extract_all(
      meta_row$positive_condition,
      "\\d+"
    )[[1]]
    
    retention_count <- data |>
      dplyr::filter(redcap_event_name == meta_row$event_name) |>
      dplyr::filter(as.character(.data[[meta_row$field]]) %in% positive_values) |>
      dplyr::distinct(.data[[participant_id_col]]) |>
      nrow()
    
    retention_qtl_value <- retention_count / recruitment_count * 100
    
    retention_flag <- dplyr::case_when(
      retention_qtl_value < 10 ~ "green",
      retention_qtl_value >= 10 & retention_qtl_value <= 24 ~ "amber",
      TRUE ~ "red"
    )
    
    results[["participant_retention"]] <- add_result(
      meta_row$qtl_id,
      meta_row$qtl_description,
      retention_qtl_value,
      retention_flag
    )
  }
  
  # endpoint completeness
  if (has_qtl("endpoint_completeness")) {
    
    meta_row <- get_meta("endpoint_completeness")
    data <- get(meta_row$export_name, envir = env)
    
    participant_id_col <- names(data)[1]
    
    endpoint_fields <- strsplit(meta_row$field, ";")[[1]]
    endpoint_fields <- trimws(endpoint_fields)
    
    conditional_value <- stringr::str_extract(
      meta_row$conditional_logic,
      "\\d+"
    )
    
    denominator_data <- data |>
      dplyr::filter(redcap_event_name == meta_row$conditional_event) |>
      dplyr::filter(as.character(.data[[meta_row$conditional_field]]) == conditional_value)
    
    endpoint_denominator <- denominator_data |>
      dplyr::distinct(.data[[participant_id_col]]) |>
      nrow()
    
    eligible_participants <- denominator_data |>
      dplyr::distinct(.data[[participant_id_col]])
    
    endpoint_count <- data |>
      dplyr::filter(redcap_event_name == meta_row$event_name) |>
      dplyr::semi_join(eligible_participants, by = participant_id_col) |>
      dplyr::filter(
        dplyr::if_all(
          dplyr::all_of(endpoint_fields),
          ~ !is.na(.x)
        )
      ) |>
      dplyr::distinct(.data[[participant_id_col]]) |>
      nrow()
    
    endpoint_qtl_value <- endpoint_count / endpoint_denominator * 100
    
    endpoint_flag <- dplyr::case_when(
      endpoint_qtl_value >= 90 ~ "green",
      endpoint_qtl_value >= 75 ~ "amber",
      TRUE ~ "red"
    )
    
    results[["endpoint_completeness"]] <- add_result(
      meta_row$qtl_id,
      meta_row$qtl_description,
      endpoint_qtl_value,
      endpoint_flag
    )
  }
  
  # total SAEs
  if (has_qtl("total_saes")) {
    
    if (is.na(recruitment_count)) {
      stop("total_saes requires timely_recruitment to be present first.")
    }
    
    meta_row <- get_meta("total_saes")
    data <- get(meta_row$export_name, envir = env)
    
    participant_id_col <- meta_row$conditional_field
    sae_value <- stringr::str_extract(meta_row$positive_condition, "\\d+")
    
    total_sae_count <- data |>
      dplyr::filter(redcap_event_name == meta_row$event_name) |>
      dplyr::filter(as.character(.data[[meta_row$field]]) == sae_value) |>
      dplyr::distinct(.data[[participant_id_col]]) |>
      nrow()
    
    total_sae_qtl_value <- total_sae_count / recruitment_count * 100
    
    total_sae_flag <- dplyr::case_when(
      total_sae_qtl_value < 10 ~ "green",
      total_sae_qtl_value >= 10 & total_sae_qtl_value < 25 ~ "amber",
      TRUE ~ "red"
    )
    
    results[["total_saes"]] <- add_result(
      meta_row$qtl_id,
      meta_row$qtl_description,
      total_sae_qtl_value,
      total_sae_flag
    )
  }
  
  # related SAEs
  if (has_qtl("related_saes")) {
    
    if (is.na(recruitment_count)) {
      stop("related_saes requires timely_recruitment to be present first.")
    }
    
    meta_row <- get_meta("related_saes")
    data <- get(meta_row$export_name, envir = env)
    
    participant_id_col <- meta_row$conditional_field
    
    related_sae_fields <- strsplit(meta_row$field, ";")[[1]]
    related_sae_fields <- trimws(related_sae_fields)
    
    positive_values <- stringr::str_extract_all(
      meta_row$positive_condition,
      "\\d+"
    )[[1]]
    
    related_sae_count <- data |>
      dplyr::filter(redcap_event_name == meta_row$event_name) |>
      dplyr::filter(
        as.character(.data[[related_sae_fields[1]]]) == positive_values[1],
        as.character(.data[[related_sae_fields[2]]]) == positive_values[2]
      ) |>
      dplyr::distinct(.data[[participant_id_col]]) |>
      nrow()
    
    related_sae_qtl_value <- related_sae_count / recruitment_count * 100
    
    related_sae_flag <- dplyr::case_when(
      related_sae_qtl_value < 10 ~ "green",
      related_sae_qtl_value >= 10 & related_sae_qtl_value < 25 ~ "amber",
      TRUE ~ "red"
    )
    
    results[["related_saes"]] <- add_result(
      meta_row$qtl_id,
      meta_row$qtl_description,
      related_sae_qtl_value,
      related_sae_flag
    )
  }
  
  # IMP compliance
  if (has_qtl("imp_compliance")) {
    
    meta_row <- get_meta("imp_compliance")
    data <- get(meta_row$export_name, envir = env)
    
    participant_id_col <- names(data)[1]
    
    positive_value <- stringr::str_extract(meta_row$positive_condition, "\\d+")
    conditional_value <- stringr::str_extract(meta_row$conditional_logic, "\\d+")
    
    imp_denominator_data <- data |>
      dplyr::filter(redcap_event_name == meta_row$conditional_event) |>
      dplyr::filter(as.character(.data[[meta_row$conditional_field]]) == conditional_value)
    
    imp_denominator <- imp_denominator_data |>
      dplyr::distinct(.data[[participant_id_col]]) |>
      nrow()
    
    eligible_participants <- imp_denominator_data |>
      dplyr::distinct(.data[[participant_id_col]])
    
    imp_count <- data |>
      dplyr::filter(redcap_event_name == meta_row$event_name) |>
      dplyr::semi_join(eligible_participants, by = participant_id_col) |>
      dplyr::filter(as.character(.data[[meta_row$field]]) == positive_value) |>
      dplyr::distinct(.data[[participant_id_col]]) |>
      nrow()
    
    imp_qtl_value <- imp_count / imp_denominator * 100
    
    imp_flag <- dplyr::case_when(
      imp_qtl_value >= 90 ~ "green",
      imp_qtl_value >= 75 ~ "amber",
      TRUE ~ "red"
    )
    
    results[["imp_compliance"]] <- add_result(
      meta_row$qtl_id,
      meta_row$qtl_description,
      imp_qtl_value,
      imp_flag
    )
  }
  
  dplyr::bind_rows(results)
}

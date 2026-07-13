# Title: Produce Key Risk Indicator Report
# Author: Paigan Aspinall
# Version & Date: V1.0.0 13JUL2026
# R version: 4.4.3


#' Produce a Key Risk Indicator Report
#'
#' Calculates site-level Key Risk Indicators using study-specific metadata and
#' exports the results to a formatted Excel workbook.
#'
#' Only KRIs included in the supplied metadata are calculated. Worksheets are
#' created only for KRI categories containing at least one calculated result.
#'
#' Each worksheet contains:
#'
#' \itemize{
#'   \item A category heading
#'   \item One section per KRI
#'   \item Site names displayed horizontally
#'   \item The calculated value for each site
#' }
#'
#' @param kri_metadata A data frame containing the KRI configuration.
#'
#' @param output_file Character string specifying the path and filename of the
#'   Excel workbook to create.
#'
#' @param data_environment Environment containing the datasets referenced in
#'   the KRI metadata. Defaults to the calling environment.
#'
#' @return Invisibly returns a named list containing the detailed calculated
#'   KRI datasets.
#'
#' @examples
#' produce_kri_report(
#'   kri_metadata = kri_metadata,
#'   output_file = "KRI_Report.xlsx"
#' )
#'
#' @export

produce_kri_report <- function(
    kri_metadata,
    output_file,
    data_environment = parent.frame()
) {
  
  # ==========================================================================
  # Validation and helper functions
  # ==========================================================================
  
  if (!is.data.frame(kri_metadata)) {
    stop("`kri_metadata` must be a data frame.", call. = FALSE)
  }
  
  if (length(output_file) != 1L ||
      is.na(output_file) ||
      trimws(output_file) == "") {
    stop("`output_file` must be a valid path ending in .xlsx.", call. = FALSE)
  }
  
  if (!grepl("\\.xlsx$", output_file, ignore.case = TRUE)) {
    output_file <- paste0(output_file, ".xlsx")
  }
  
  required_columns <- c(
    "kri_id", "kri_category", "kri_description", "export_name",
    "event_name", "field", "positive_condition",
    "denominator_field", "denominator_positive",
    "date_start_field", "date_end_field",
    "lookup_join_field_1", "lookup_join_event_1",
    "lookup_join_system_1", "lookup_value_field_1",
    "lookup_join_field_2", "lookup_join_event_2",
    "lookup_join_system_2"
  )
  
  missing_columns <- setdiff(required_columns, names(kri_metadata))
  
  if (length(missing_columns) > 0L) {
    stop(
      paste0(
        "The metadata is missing: ",
        paste(missing_columns, collapse = ", "),
        "."
      ),
      call. = FALSE
    )
  }
  
  site_col <- "redcap_data_access_group"
  
  metadata_value <- function(meta_row, column_name) {
    value <- meta_row[[column_name]][[1]]
    
    if (length(value) == 0L ||
        is.na(value) ||
        trimws(as.character(value)) == "" ||
        toupper(trimws(as.character(value))) == "NA") {
      return(NA_character_)
    }
    
    trimws(as.character(value))
  }
  
  split_metadata_value <- function(value) {
    if (length(value) == 0L ||
        is.na(value) ||
        trimws(as.character(value)) == "") {
      return(character())
    }
    
    trimws(strsplit(as.character(value), ";", fixed = TRUE)[[1]])
  }
  
  is_populated <- function(x) {
    !is.na(x) & trimws(as.character(x)) != ""
  }
  
  kri_exists <- function(kri_id) {
    kri_id %in% kri_metadata$kri_id
  }
  
  get_meta_row <- function(kri_id) {
    meta_row <- kri_metadata[
      kri_metadata$kri_id == kri_id,
      ,
      drop = FALSE
    ]
    
    if (nrow(meta_row) == 0L) {
      return(NULL)
    }
    
    if (nrow(meta_row) > 1L) {
      warning(
        paste0(
          "More than one metadata row was found for `",
          kri_id,
          "`. Only the first row will be used."
        ),
        call. = FALSE
      )
      
      meta_row <- meta_row[1, , drop = FALSE]
    }
    
    meta_row
  }
  
  get_dataset <- function(meta_row, metadata_column = "export_name") {
    dataset_name <- metadata_value(meta_row, metadata_column)
    
    if (is.na(dataset_name)) {
      stop(
        paste0(
          "No dataset was specified in `",
          metadata_column,
          "` for KRI `",
          metadata_value(meta_row, "kri_id"),
          "`."
        ),
        call. = FALSE
      )
    }
    
    if (!exists(
      dataset_name,
      envir = data_environment,
      inherits = TRUE
    )) {
      stop(
        paste0(
          "Dataset `",
          dataset_name,
          "` could not be found for KRI `",
          metadata_value(meta_row, "kri_id"),
          "`."
        ),
        call. = FALSE
      )
    }
    
    data <- get(
      dataset_name,
      envir = data_environment,
      inherits = TRUE
    )
    
    if (!is.data.frame(data)) {
      stop(
        paste0("`", dataset_name, "` is not a data frame."),
        call. = FALSE
      )
    }
    
    data
  }
  
  check_fields <- function(data, fields, kri_id) {
    fields <- unique(fields[
      !is.na(fields) & trimws(fields) != ""
    ])
    
    missing_fields <- setdiff(fields, names(data))
    
    if (length(missing_fields) > 0L) {
      stop(
        paste0(
          "Dataset for KRI `",
          kri_id,
          "` is missing: ",
          paste(missing_fields, collapse = ", "),
          "."
        ),
        call. = FALSE
      )
    }
  }
  
  parse_date <- function(x) {
    x <- trimws(as.character(x))
    x[x == ""] <- NA_character_
    suppressWarnings(as.Date(x))
  }
  
  get_all_sites <- function(data) {
    check_fields(data, site_col, "site list")
    
    data |>
      dplyr::filter(is_populated(.data[[site_col]])) |>
      dplyr::distinct(site = .data[[site_col]])
  }
  
  apply_condition <- function(data, fields, condition) {
    fields <- split_metadata_value(fields)
    
    if (length(fields) == 0L ||
        length(condition) == 0L ||
        is.na(condition) ||
        trimws(as.character(condition)) == "") {
      return(data)
    }
    
    condition <- trimws(as.character(condition))
    
    if (toupper(condition) == "NOT NULL") {
      return(
        data |>
          dplyr::filter(
            dplyr::if_all(
              dplyr::all_of(fields),
              is_populated
            )
          )
      )
    }
    
    if (grepl("\\bAND\\b", condition, ignore.case = TRUE)) {
      values <- trimws(
        strsplit(
          toupper(condition),
          "\\s+AND\\s+",
          perl = TRUE
        )[[1]]
      )
      
      if (length(fields) != length(values)) {
        stop(
          paste0(
            "The number of fields and AND values do not match for condition `",
            condition,
            "`."
          ),
          call. = FALSE
        )
      }
      
      for (i in seq_along(fields)) {
        selected_field <- fields[[i]]
        selected_value <- values[[i]]
        
        data <- data |>
          dplyr::filter(
            toupper(as.character(.data[[selected_field]])) ==
              selected_value
          )
      }
      
      return(data)
    }
    
    values <- trimws(
      strsplit(
        toupper(condition),
        "\\s+OR\\s+",
        perl = TRUE
      )[[1]]
    )
    
    if (length(fields) == 1L) {
      return(
        data |>
          dplyr::filter(
            toupper(as.character(.data[[fields[[1]]]])) %in%
              values
          )
      )
    }
    
    stop(
      paste0(
        "Unable to interpret condition `",
        condition,
        "` for multiple fields."
      ),
      call. = FALSE
    )
  }
  
  prepare_result <- function(
    result,
    meta_row,
    value_column,
    display_type = "integer"
  ) {
    result |>
      dplyr::mutate(
        kri_id = metadata_value(meta_row, "kri_id"),
        kri_category = metadata_value(meta_row, "kri_category"),
        kri_name = metadata_value(meta_row, "kri_description"),
        display_value = as.numeric(.data[[value_column]]),
        display_type = display_type
      ) |>
      dplyr::relocate(
        kri_id,
        kri_category,
        kri_name,
        site,
        display_value,
        display_type
      )
  }
  
  kri_results <- list()
  
  # ==========================================================================
  # Timely recruitment
  # ==========================================================================
  
  if (kri_exists("recruitment_vs_expected")) {
    meta_row <- get_meta_row("recruitment_vs_expected")
    data <- get_dataset(meta_row)
    lookup_data <- get_dataset(meta_row, "lookup_join_system_1")
    
    event_name <- metadata_value(meta_row, "event_name")
    field <- metadata_value(meta_row, "field")
    source_site_col <- metadata_value(meta_row, "lookup_join_field_2")
    lookup_site_col <- metadata_value(meta_row, "lookup_join_field_1")
    target_col <- metadata_value(meta_row, "lookup_value_field_1")
    
    check_fields(
      data,
      c("redcap_event_name", field, source_site_col),
      "recruitment_vs_expected"
    )
    check_fields(
      lookup_data,
      c(lookup_site_col, target_col),
      "recruitment_vs_expected"
    )
    
    actual_recruitment <- data |>
      dplyr::filter(.data$redcap_event_name == event_name) |>
      dplyr::filter(is_populated(.data[[field]])) |>
      dplyr::filter(is_populated(.data[[source_site_col]])) |>
      dplyr::count(
        site = .data[[source_site_col]],
        name = "actual_recruited"
      )
    
    expected_recruitment <- lookup_data |>
      dplyr::filter(is_populated(.data[[lookup_site_col]])) |>
      dplyr::transmute(
        site = .data[[lookup_site_col]],
        expected_recruitment =
          suppressWarnings(as.numeric(.data[[target_col]]))
      )
    
    result <- expected_recruitment |>
      dplyr::left_join(actual_recruitment, by = "site") |>
      dplyr::mutate(
        actual_recruited =
          tidyr::replace_na(.data$actual_recruited, 0L),
        kri_value = dplyr::if_else(
          !is.na(.data$expected_recruitment) &
            .data$expected_recruitment > 0,
          .data$actual_recruited /
            .data$expected_recruitment * 100,
          NA_real_
        )
      )
    
    kri_results[["recruitment_vs_expected"]] <- prepare_result(
      result,
      meta_row,
      "kri_value",
      "percentage"
    )
  }
  
  for (kri_id in c("identified_by_site", "screened_by_site")) {
    if (!kri_exists(kri_id)) {
      next
    }
    
    meta_row <- get_meta_row(kri_id)
    data <- get_dataset(meta_row)
    field <- metadata_value(meta_row, "field")
    
    output_column <- if (
      kri_id == "identified_by_site"
    ) {
      "identified_participants"
    } else {
      "screened_participants"
    }
    
    check_fields(data, c(site_col, field), kri_id)
    
    all_sites <- get_all_sites(data)
    
    counts <- data |>
      dplyr::filter(is_populated(.data[[field]])) |>
      dplyr::filter(is_populated(.data[[site_col]])) |>
      dplyr::count(
        site = .data[[site_col]],
        name = output_column
      )
    
    result <- all_sites |>
      dplyr::left_join(counts, by = "site")
    
    result[[output_column]] <- tidyr::replace_na(
      result[[output_column]],
      0L
    )
    
    kri_results[[kri_id]] <- prepare_result(
      result,
      meta_row,
      output_column,
      "integer"
    )
  }
  
  calculate_activation_to_first <- function(meta_row, first_date_name) {
    data <- get_dataset(meta_row)
    lookup_data <- get_dataset(meta_row, "lookup_join_system_1")
    
    source_site_col <- metadata_value(meta_row, "lookup_join_field_2")
    lookup_site_col <- metadata_value(meta_row, "lookup_join_field_1")
    activation_date_col <- metadata_value(meta_row, "lookup_value_field_1")
    event_date_col <- metadata_value(meta_row, "date_end_field")
    event_name <- metadata_value(meta_row, "event_name")
    field <- metadata_value(meta_row, "field")
    condition <- metadata_value(meta_row, "positive_condition")
    kri_id <- metadata_value(meta_row, "kri_id")
    
    check_fields(
      data,
      c(source_site_col, event_date_col, split_metadata_value(field)),
      kri_id
    )
    check_fields(
      lookup_data,
      c(lookup_site_col, activation_date_col),
      kri_id
    )
    
    event_data <- data
    
    if (!is.na(event_name)) {
      check_fields(data, "redcap_event_name", kri_id)
      
      event_data <- event_data |>
        dplyr::filter(.data$redcap_event_name %in%
                        split_metadata_value(event_name))
    }
    
    event_data <- apply_condition(
      event_data,
      field,
      condition
    )
    
    first_event <- event_data |>
      dplyr::filter(is_populated(.data[[source_site_col]])) |>
      dplyr::mutate(
        event_date = parse_date(.data[[event_date_col]])
      ) |>
      dplyr::filter(!is.na(.data$event_date)) |>
      dplyr::group_by(site = .data[[source_site_col]]) |>
      dplyr::summarise(
        first_event_date = min(.data$event_date),
        .groups = "drop"
      )
    
    site_activation <- lookup_data |>
      dplyr::filter(is_populated(.data[[lookup_site_col]])) |>
      dplyr::transmute(
        site = .data[[lookup_site_col]],
        activation_date =
          parse_date(.data[[activation_date_col]])
      ) |>
      dplyr::distinct(.data$site, .keep_all = TRUE)
    
    result <- site_activation |>
      dplyr::left_join(first_event, by = "site") |>
      dplyr::mutate(
        kri_value = as.numeric(
          .data$first_event_date - .data$activation_date
        )
      )
    
    names(result)[names(result) == "first_event_date"] <-
      first_date_name
    
    result
  }
  
  if (kri_exists("activation_to_first_recruitment")) {
    meta_row <- get_meta_row("activation_to_first_recruitment")
    result <- calculate_activation_to_first(
      meta_row,
      "first_recruitment_date"
    )
    
    kri_results[["activation_to_first_recruitment"]] <-
      prepare_result(result, meta_row, "kri_value", "integer")
  }
  
  if (kri_exists("activation_to_first_screen")) {
    meta_row <- get_meta_row("activation_to_first_screen")
    result <- calculate_activation_to_first(
      meta_row,
      "first_screening_date"
    )
    
    kri_results[["activation_to_first_screen"]] <-
      prepare_result(result, meta_row, "kri_value", "integer")
  }
  
  if (kri_exists("screen_failure_rate")) {
    meta_row <- get_meta_row("screen_failure_rate")
    data <- get_dataset(meta_row)
    participant_id_col <- names(data)[1]
    
    field <- metadata_value(meta_row, "field")
    numerator_condition <- metadata_value(
      meta_row,
      "positive_condition"
    )
    denominator_field <- metadata_value(
      meta_row,
      "denominator_field"
    )
    denominator_condition <- metadata_value(
      meta_row,
      "denominator_positive"
    )
    
    check_fields(
      data,
      c(participant_id_col, site_col, field, denominator_field),
      "screen_failure_rate"
    )
    
    all_sites <- get_all_sites(data)
    
    denominator_data <- apply_condition(
      data,
      denominator_field,
      denominator_condition
    )
    
    denominator <- denominator_data |>
      dplyr::filter(is_populated(.data[[site_col]])) |>
      dplyr::distinct(
        .data[[participant_id_col]],
        .data[[site_col]]
      ) |>
      dplyr::count(
        site = .data[[site_col]],
        name = "denominator"
      )
    
    numerator_data <- apply_condition(
      denominator_data,
      field,
      numerator_condition
    )
    
    numerator <- numerator_data |>
      dplyr::filter(is_populated(.data[[site_col]])) |>
      dplyr::distinct(
        .data[[participant_id_col]],
        .data[[site_col]]
      ) |>
      dplyr::count(
        site = .data[[site_col]],
        name = "screen_failures"
      )
    
    result <- all_sites |>
      dplyr::left_join(denominator, by = "site") |>
      dplyr::left_join(numerator, by = "site") |>
      dplyr::mutate(
        denominator =
          tidyr::replace_na(.data$denominator, 0L),
        screen_failures =
          tidyr::replace_na(.data$screen_failures, 0L),
        kri_value = dplyr::if_else(
          .data$denominator > 0,
          .data$screen_failures /
            .data$denominator * 100,
          NA_real_
        )
      )
    
    kri_results[["screen_failure_rate"]] <- prepare_result(
      result,
      meta_row,
      "kri_value",
      "percentage"
    )
  }
  
  if (kri_exists("screening_to_randomisation_time")) {
    meta_row <- get_meta_row("screening_to_randomisation_time")
    data <- get_dataset(meta_row)
    screening_data <- get_dataset(meta_row, "lookup_join_system_1")
    
    participant_id_col <- names(data)[1]
    source_join_col <- metadata_value(meta_row, "lookup_join_field_2")
    source_join_event <- metadata_value(meta_row, "lookup_join_event_2")
    lookup_join_col <- metadata_value(meta_row, "lookup_join_field_1")
    screening_date_col <- metadata_value(meta_row, "lookup_value_field_1")
    randomisation_event <- metadata_value(meta_row, "event_name")
    randomisation_date_col <- metadata_value(meta_row, "date_end_field")
    randomisation_field <- metadata_value(meta_row, "field")
    
    check_fields(
      data,
      c(
        participant_id_col, "redcap_event_name", source_join_col,
        randomisation_date_col, randomisation_field, site_col
      ),
      "screening_to_randomisation_time"
    )
    check_fields(
      screening_data,
      c(lookup_join_col, screening_date_col),
      "screening_to_randomisation_time"
    )
    
    id_lookup <- data |>
      dplyr::filter(.data$redcap_event_name == source_join_event) |>
      dplyr::filter(is_populated(.data[[source_join_col]])) |>
      dplyr::transmute(
        participant_id = .data[[participant_id_col]],
        screening_id = .data[[source_join_col]],
        site = .data[[site_col]]
      ) |>
      dplyr::distinct()
    
    randomisation_dates <- data |>
      dplyr::filter(
        .data$redcap_event_name == randomisation_event
      ) |>
      dplyr::filter(
        is_populated(.data[[randomisation_field]])
      ) |>
      dplyr::transmute(
        participant_id = .data[[participant_id_col]],
        randomisation_date =
          parse_date(.data[[randomisation_date_col]])
      ) |>
      dplyr::filter(!is.na(.data$randomisation_date)) |>
      dplyr::group_by(.data$participant_id) |>
      dplyr::summarise(
        randomisation_date =
          min(.data$randomisation_date),
        .groups = "drop"
      )
    
    screening_dates <- screening_data |>
      dplyr::filter(is_populated(.data[[lookup_join_col]])) |>
      dplyr::transmute(
        screening_id = .data[[lookup_join_col]],
        screening_date =
          parse_date(.data[[screening_date_col]])
      ) |>
      dplyr::filter(!is.na(.data$screening_date)) |>
      dplyr::group_by(.data$screening_id) |>
      dplyr::summarise(
        screening_date = min(.data$screening_date),
        .groups = "drop"
      )
    
    result <- id_lookup |>
      dplyr::inner_join(
        randomisation_dates,
        by = "participant_id"
      ) |>
      dplyr::inner_join(
        screening_dates,
        by = "screening_id"
      ) |>
      dplyr::filter(is_populated(.data$site)) |>
      dplyr::mutate(
        days_screening_to_randomisation = as.numeric(
          .data$randomisation_date -
            .data$screening_date
        )
      ) |>
      dplyr::group_by(.data$site) |>
      dplyr::summarise(
        n_participants =
          dplyr::n_distinct(.data$participant_id),
        average_days = mean(
          .data$days_screening_to_randomisation,
          na.rm = TRUE
        ),
        median_days = stats::median(
          .data$days_screening_to_randomisation,
          na.rm = TRUE
        ),
        min_days = min(
          .data$days_screening_to_randomisation,
          na.rm = TRUE
        ),
        max_days = max(
          .data$days_screening_to_randomisation,
          na.rm = TRUE
        ),
        .groups = "drop"
      )
    
    kri_results[["screening_to_randomisation_time"]] <-
      prepare_result(
        result,
        meta_row,
        "average_days",
        "decimal"
      )
  }
  
  # ==========================================================================
  # Participant retention and data completeness
  # ==========================================================================
  
  retention_map <- c(
    number_withdrawn = "number_withdrawn",
    number_dropout = "number_dropout",
    number_lost = "number_lost",
    number_partial = "partial_withdrawals"
  )
  
  for (kri_id in names(retention_map)) {
    if (!kri_exists(kri_id)) {
      next
    }
    
    meta_row <- get_meta_row(kri_id)
    data <- get_dataset(meta_row)
    participant_id_col <- names(data)[1]
    
    event_name <- metadata_value(meta_row, "event_name")
    field <- metadata_value(meta_row, "field")
    condition <- metadata_value(meta_row, "positive_condition")
    output_column <- retention_map[[kri_id]]
    
    check_fields(
      data,
      c(participant_id_col, "redcap_event_name", site_col, field),
      kri_id
    )
    
    all_sites <- get_all_sites(data)
    
    selected_data <- data |>
      dplyr::filter(.data$redcap_event_name == event_name)
    
    selected_data <- apply_condition(
      selected_data,
      field,
      condition
    )
    
    counts <- selected_data |>
      dplyr::filter(is_populated(.data[[site_col]])) |>
      dplyr::distinct(
        .data[[participant_id_col]],
        .data[[site_col]]
      ) |>
      dplyr::count(
        site = .data[[site_col]],
        name = output_column
      )
    
    result <- all_sites |>
      dplyr::left_join(counts, by = "site")
    
    result[[output_column]] <- tidyr::replace_na(
      result[[output_column]],
      0L
    )
    
    kri_results[[kri_id]] <- prepare_result(
      result,
      meta_row,
      output_column,
      "integer"
    )
  }
  
  if (kri_exists("number_missed")) {
    meta_row <- get_meta_row("number_missed")
    data <- get_dataset(meta_row)
    participant_id_col <- names(data)[1]
    
    events <- split_metadata_value(
      metadata_value(meta_row, "event_name")
    )
    field <- metadata_value(meta_row, "field")
    condition <- metadata_value(meta_row, "positive_condition")
    
    check_fields(
      data,
      c(participant_id_col, "redcap_event_name", site_col, field),
      "number_missed"
    )
    
    all_sites <- get_all_sites(data)
    
    missed_data <- data |>
      dplyr::filter(.data$redcap_event_name %in% events)
    
    missed_data <- apply_condition(
      missed_data,
      field,
      condition
    )
    
    counts <- missed_data |>
      dplyr::filter(is_populated(.data[[site_col]])) |>
      dplyr::distinct(
        .data[[participant_id_col]],
        .data[[site_col]]
      ) |>
      dplyr::count(
        site = .data[[site_col]],
        name = "number_missed"
      )
    
    result <- all_sites |>
      dplyr::left_join(counts, by = "site") |>
      dplyr::mutate(
        number_missed =
          tidyr::replace_na(.data$number_missed, 0L)
      )
    
    kri_results[["number_missed"]] <- prepare_result(
      result,
      meta_row,
      "number_missed",
      "integer"
    )
  }
  
  if (kri_exists("primary_endpoint_completeness")) {
    meta_row <- get_meta_row("primary_endpoint_completeness")
    data <- get_dataset(meta_row)
    participant_id_col <- names(data)[1]
    
    event_name <- metadata_value(meta_row, "event_name")
    endpoint_fields <- split_metadata_value(
      metadata_value(meta_row, "field")
    )
    denominator_field <- metadata_value(
      meta_row,
      "denominator_field"
    )
    denominator_condition <- metadata_value(
      meta_row,
      "denominator_positive"
    )
    
    check_fields(
      data,
      c(
        participant_id_col, "redcap_event_name", site_col,
        endpoint_fields, denominator_field
      ),
      "primary_endpoint_completeness"
    )
    
    all_sites <- get_all_sites(data)
    
    denominator_data <- data |>
      dplyr::filter(.data$redcap_event_name == event_name)
    
    denominator_data <- apply_condition(
      denominator_data,
      denominator_field,
      denominator_condition
    )
    
    denominator <- denominator_data |>
      dplyr::filter(is_populated(.data[[site_col]])) |>
      dplyr::distinct(
        .data[[participant_id_col]],
        .data[[site_col]]
      ) |>
      dplyr::count(
        site = .data[[site_col]],
        name = "endpoint_denominator"
      )
    
    complete <- denominator_data |>
      dplyr::filter(
        dplyr::if_all(
          dplyr::all_of(endpoint_fields),
          is_populated
        )
      ) |>
      dplyr::filter(is_populated(.data[[site_col]])) |>
      dplyr::distinct(
        .data[[participant_id_col]],
        .data[[site_col]]
      ) |>
      dplyr::count(
        site = .data[[site_col]],
        name = "endpoint_complete"
      )
    
    result <- all_sites |>
      dplyr::left_join(denominator, by = "site") |>
      dplyr::left_join(complete, by = "site") |>
      dplyr::mutate(
        endpoint_denominator =
          tidyr::replace_na(.data$endpoint_denominator, 0L),
        endpoint_complete =
          tidyr::replace_na(.data$endpoint_complete, 0L),
        kri_value = dplyr::if_else(
          .data$endpoint_denominator > 0,
          .data$endpoint_complete /
            .data$endpoint_denominator * 100,
          NA_real_
        )
      )
    
    kri_results[["primary_endpoint_completeness"]] <-
      prepare_result(
        result,
        meta_row,
        "kri_value",
        "percentage"
      )
  }
  
  # ==========================================================================
  # Participant safety
  # ==========================================================================
  
  safety_count_map <- c(
    number_ae = "number_ae",
    number_sae = "number_sae",
    number_susar = "number_susar"
  )
  
  for (kri_id in names(safety_count_map)) {
    if (!kri_exists(kri_id)) {
      next
    }
    
    meta_row <- get_meta_row(kri_id)
    data <- get_dataset(meta_row)
    
    event_name <- metadata_value(meta_row, "event_name")
    fields <- metadata_value(meta_row, "field")
    condition <- metadata_value(meta_row, "positive_condition")
    output_column <- safety_count_map[[kri_id]]
    
    check_fields(
      data,
      c("redcap_event_name", site_col, split_metadata_value(fields)),
      kri_id
    )
    
    all_sites <- get_all_sites(data)
    
    selected_data <- data |>
      dplyr::filter(.data$redcap_event_name == event_name)
    
    selected_data <- apply_condition(
      selected_data,
      fields,
      condition
    )
    
    counts <- selected_data |>
      dplyr::filter(is_populated(.data[[site_col]])) |>
      dplyr::count(
        site = .data[[site_col]],
        name = output_column
      )
    
    result <- all_sites |>
      dplyr::left_join(counts, by = "site")
    
    result[[output_column]] <- tidyr::replace_na(
      result[[output_column]],
      0L
    )
    
    kri_results[[kri_id]] <- prepare_result(
      result,
      meta_row,
      output_column,
      "integer"
    )
  }
  
  safety_percent_map <- c(
    percent_ae = "participants_with_ae",
    percent_sae = "participants_with_sae",
    percent_susar = "participants_with_susar"
  )
  
  for (kri_id in names(safety_percent_map)) {
    if (!kri_exists(kri_id)) {
      next
    }
    
    meta_row <- get_meta_row(kri_id)
    safety_data <- get_dataset(meta_row)
    recruitment_data <- get_dataset(
      meta_row,
      "lookup_join_system_2"
    )
    
    safety_event <- metadata_value(meta_row, "event_name")
    safety_fields <- metadata_value(meta_row, "field")
    condition <- metadata_value(meta_row, "positive_condition")
    safety_link_col <- metadata_value(
      meta_row,
      "lookup_join_field_1"
    )
    recruitment_link_col <- metadata_value(
      meta_row,
      "lookup_join_field_2"
    )
    recruitment_event <- metadata_value(
      meta_row,
      "lookup_join_event_2"
    )
    numerator_column <- safety_percent_map[[kri_id]]
    
    check_fields(
      safety_data,
      c(
        "redcap_event_name", safety_link_col,
        split_metadata_value(safety_fields)
      ),
      kri_id
    )
    check_fields(
      recruitment_data,
      c("redcap_event_name", recruitment_link_col, site_col),
      kri_id
    )
    
    selected_safety <- safety_data |>
      dplyr::filter(.data$redcap_event_name == safety_event)
    
    selected_safety <- apply_condition(
      selected_safety,
      safety_fields,
      condition
    )
    
    selected_safety <- selected_safety |>
      dplyr::filter(
        is_populated(.data[[safety_link_col]])
      ) |>
      dplyr::distinct(
        record_id = .data[[safety_link_col]]
      )
    
    recruited_lookup <- recruitment_data |>
      dplyr::filter(
        .data$redcap_event_name == recruitment_event
      ) |>
      dplyr::filter(
        is_populated(.data[[recruitment_link_col]])
      ) |>
      dplyr::filter(is_populated(.data[[site_col]])) |>
      dplyr::transmute(
        record_id = .data[[recruitment_link_col]],
        site = .data[[site_col]]
      ) |>
      dplyr::distinct()
    
    denominator <- recruited_lookup |>
      dplyr::count(
        .data$site,
        name = "denominator"
      )
    
    numerator <- selected_safety |>
      dplyr::inner_join(
        recruited_lookup,
        by = "record_id"
      ) |>
      dplyr::distinct(.data$record_id, .data$site) |>
      dplyr::count(
        .data$site,
        name = numerator_column
      )
    
    result <- denominator |>
      dplyr::left_join(numerator, by = "site")
    
    result[[numerator_column]] <- tidyr::replace_na(
      result[[numerator_column]],
      0L
    )
    
    result <- result |>
      dplyr::mutate(
        kri_value = dplyr::if_else(
          .data$denominator > 0,
          .data[[numerator_column]] /
            .data$denominator * 100,
          NA_real_
        )
      )
    
    kri_results[[kri_id]] <- prepare_result(
      result,
      meta_row,
      "kri_value",
      "percentage"
    )
  }
  
  if (kri_exists("late_report_safety")) {
    meta_row <- get_meta_row("late_report_safety")
    data <- get_dataset(meta_row)
    
    event_name <- metadata_value(meta_row, "event_name")
    report_date_col <- metadata_value(
      meta_row,
      "date_start_field"
    )
    onset_date_col <- metadata_value(
      meta_row,
      "date_end_field"
    )
    
    check_fields(
      data,
      c(
        "redcap_event_name", site_col,
        report_date_col, onset_date_col
      ),
      "late_report_safety"
    )
    
    all_sites <- get_all_sites(data)
    
    counts <- data |>
      dplyr::filter(.data$redcap_event_name == event_name) |>
      dplyr::filter(is_populated(.data[[site_col]])) |>
      dplyr::mutate(
        onset_date = parse_date(.data[[onset_date_col]]),
        report_date = parse_date(.data[[report_date_col]]),
        reporting_delay_days = as.numeric(
          .data$report_date - .data$onset_date
        )
      ) |>
      dplyr::filter(
        !is.na(.data$reporting_delay_days),
        .data$reporting_delay_days > 1
      ) |>
      dplyr::count(
        site = .data[[site_col]],
        name = "late_safety_reports"
      )
    
    result <- all_sites |>
      dplyr::left_join(counts, by = "site") |>
      dplyr::mutate(
        late_safety_reports =
          tidyr::replace_na(.data$late_safety_reports, 0L)
      )
    
    kri_results[["late_report_safety"]] <- prepare_result(
      result,
      meta_row,
      "late_safety_reports",
      "integer"
    )
  }
  
  if (kri_exists("late_pi")) {
    meta_row <- get_meta_row("late_pi")
    data <- get_dataset(meta_row)
    
    event_name <- metadata_value(meta_row, "event_name")
    initial_report_date_col <- metadata_value(
      meta_row,
      "date_start_field"
    )
    pi_completion_date_col <- metadata_value(
      meta_row,
      "date_end_field"
    )
    
    check_fields(
      data,
      c(
        "redcap_event_name", site_col,
        initial_report_date_col, pi_completion_date_col
      ),
      "late_pi"
    )
    
    all_sites <- get_all_sites(data)
    
    counts <- data |>
      dplyr::filter(.data$redcap_event_name == event_name) |>
      dplyr::filter(is_populated(.data[[site_col]])) |>
      dplyr::mutate(
        initial_report_date =
          parse_date(.data[[initial_report_date_col]]),
        pi_completion_date =
          parse_date(.data[[pi_completion_date_col]]),
        pi_delay_days = as.numeric(
          .data$pi_completion_date -
            .data$initial_report_date
        )
      ) |>
      dplyr::filter(
        !is.na(.data$pi_delay_days),
        .data$pi_delay_days > 3
      ) |>
      dplyr::count(
        site = .data[[site_col]],
        name = "late_pi_assessments"
      )
    
    result <- all_sites |>
      dplyr::left_join(counts, by = "site") |>
      dplyr::mutate(
        late_pi_assessments =
          tidyr::replace_na(.data$late_pi_assessments, 0L)
      )
    
    kri_results[["late_pi"]] <- prepare_result(
      result,
      meta_row,
      "late_pi_assessments",
      "integer"
    )
  }
  
  # ==========================================================================
  # Intervention adherence
  # ==========================================================================
  
  if (kri_exists("imp_compliance")) {
    meta_row <- get_meta_row("imp_compliance")
    data <- get_dataset(meta_row)
    participant_id_col <- names(data)[1]
    
    events <- split_metadata_value(
      metadata_value(meta_row, "event_name")
    )
    field <- metadata_value(meta_row, "field")
    compliant_value <- metadata_value(
      meta_row,
      "positive_condition"
    )
    denominator_field <- metadata_value(
      meta_row,
      "denominator_field"
    )
    denominator_value <- metadata_value(
      meta_row,
      "denominator_positive"
    )
    
    check_fields(
      data,
      c(
        participant_id_col, "redcap_event_name", site_col,
        field, denominator_field
      ),
      "imp_compliance"
    )
    
    all_sites <- get_all_sites(data)
    
    eligible_records <- data |>
      dplyr::filter(.data$redcap_event_name %in% events) |>
      dplyr::filter(is_populated(.data[[site_col]])) |>
      dplyr::filter(
        as.character(.data[[denominator_field]]) ==
          denominator_value
      )
    
    denominator <- eligible_records |>
      dplyr::group_by(
        .data[[participant_id_col]],
        .data[[site_col]]
      ) |>
      dplyr::summarise(
        events_expected =
          dplyr::n_distinct(.data$redcap_event_name),
        .groups = "drop"
      ) |>
      dplyr::filter(.data$events_expected == length(events)) |>
      dplyr::count(
        site = .data[[site_col]],
        name = "denominator"
      )
    
    compliant <- eligible_records |>
      dplyr::group_by(
        .data[[participant_id_col]],
        .data[[site_col]]
      ) |>
      dplyr::summarise(
        events_expected =
          dplyr::n_distinct(.data$redcap_event_name),
        events_compliant = dplyr::n_distinct(
          .data$redcap_event_name[
            as.character(.data[[field]]) ==
              compliant_value
          ]
        ),
        .groups = "drop"
      ) |>
      dplyr::filter(
        .data$events_expected == length(events),
        .data$events_compliant == length(events)
      ) |>
      dplyr::count(
        site = .data[[site_col]],
        name = "participants_compliant"
      )
    
    result <- all_sites |>
      dplyr::left_join(denominator, by = "site") |>
      dplyr::left_join(compliant, by = "site") |>
      dplyr::mutate(
        denominator =
          tidyr::replace_na(.data$denominator, 0L),
        participants_compliant =
          tidyr::replace_na(.data$participants_compliant, 0L),
        kri_value = dplyr::if_else(
          .data$denominator > 0,
          .data$participants_compliant /
            .data$denominator * 100,
          NA_real_
        )
      )
    
    kri_results[["imp_compliance"]] <- prepare_result(
      result,
      meta_row,
      "kri_value",
      "percentage"
    )
  }
  
  if (kri_exists("missed_dose_percent")) {
    meta_row <- get_meta_row("missed_dose_percent")
    data <- get_dataset(meta_row)
    participant_id_col <- names(data)[1]
    
    events <- split_metadata_value(
      metadata_value(meta_row, "event_name")
    )
    field <- metadata_value(meta_row, "field")
    missed_value <- metadata_value(
      meta_row,
      "positive_condition"
    )
    denominator_field <- metadata_value(
      meta_row,
      "denominator_field"
    )
    denominator_value <- metadata_value(
      meta_row,
      "denominator_positive"
    )
    
    check_fields(
      data,
      c(
        participant_id_col, "redcap_event_name", site_col,
        field, denominator_field
      ),
      "missed_dose_percent"
    )
    
    all_sites <- get_all_sites(data)
    
    eligible <- data |>
      dplyr::filter(.data$redcap_event_name %in% events) |>
      dplyr::filter(is_populated(.data[[site_col]])) |>
      dplyr::filter(
        as.character(.data[[denominator_field]]) ==
          denominator_value
      )
    
    denominator <- eligible |>
      dplyr::distinct(
        .data[[participant_id_col]],
        .data[[site_col]]
      ) |>
      dplyr::count(
        site = .data[[site_col]],
        name = "denominator"
      )
    
    numerator <- eligible |>
      dplyr::filter(
        as.character(.data[[field]]) == missed_value
      ) |>
      dplyr::distinct(
        .data[[participant_id_col]],
        .data[[site_col]]
      ) |>
      dplyr::count(
        site = .data[[site_col]],
        name = "participants_with_missed_dose"
      )
    
    result <- all_sites |>
      dplyr::left_join(denominator, by = "site") |>
      dplyr::left_join(numerator, by = "site") |>
      dplyr::mutate(
        denominator =
          tidyr::replace_na(.data$denominator, 0L),
        participants_with_missed_dose =
          tidyr::replace_na(
            .data$participants_with_missed_dose,
            0L
          ),
        kri_value = dplyr::if_else(
          .data$denominator > 0,
          .data$participants_with_missed_dose /
            .data$denominator * 100,
          NA_real_
        )
      )
    
    kri_results[["missed_dose_percent"]] <- prepare_result(
      result,
      meta_row,
      "kri_value",
      "percentage"
    )
  }
  
  if (kri_exists("missed_dose_number")) {
    meta_row <- get_meta_row("missed_dose_number")
    data <- get_dataset(meta_row)
    
    events <- split_metadata_value(
      metadata_value(meta_row, "event_name")
    )
    field <- metadata_value(meta_row, "field")
    missed_value <- metadata_value(
      meta_row,
      "positive_condition"
    )
    denominator_field <- metadata_value(
      meta_row,
      "denominator_field"
    )
    denominator_value <- metadata_value(
      meta_row,
      "denominator_positive"
    )
    
    check_fields(
      data,
      c("redcap_event_name", site_col, field, denominator_field),
      "missed_dose_number"
    )
    
    all_sites <- get_all_sites(data)
    
    counts <- data |>
      dplyr::filter(.data$redcap_event_name %in% events) |>
      dplyr::filter(is_populated(.data[[site_col]])) |>
      dplyr::filter(
        as.character(.data[[denominator_field]]) ==
          denominator_value
      ) |>
      dplyr::filter(
        as.character(.data[[field]]) == missed_value
      ) |>
      dplyr::count(
        site = .data[[site_col]],
        name = "missed_dose_instances"
      )
    
    result <- all_sites |>
      dplyr::left_join(counts, by = "site") |>
      dplyr::mutate(
        missed_dose_instances =
          tidyr::replace_na(.data$missed_dose_instances, 0L)
      )
    
    kri_results[["missed_dose_number"]] <- prepare_result(
      result,
      meta_row,
      "missed_dose_instances",
      "integer"
    )
  }
  
  # ==========================================================================
  # Create workbook
  # ==========================================================================
  
  if (length(kri_results) == 0L) {
    stop("No KRI results were produced.", call. = FALSE)
  }
  
  combined_results <- dplyr::bind_rows(kri_results)
  
  category_order <- kri_metadata |>
    dplyr::filter(.data$kri_id %in% names(kri_results)) |>
    dplyr::pull(.data$kri_category) |>
    unique()
  
  kri_order <- kri_metadata |>
    dplyr::filter(.data$kri_id %in% names(kri_results)) |>
    dplyr::pull(.data$kri_id) |>
    unique()
  
  workbook <- openxlsx::createWorkbook(
    creator = "Paigan Aspinall"
  )
  
  category_colours <- c(
    timely_recruitment = "#F4D7C8",
    participant_retention = "#D9EAD3",
    data_completeness = "#D9EAF7",
    participant_safety = "#F4CCCC",
    intervention_adherence = "#E4D7F5"
  )
  
  fallback_colours <- c(
    "#FFF2CC", "#D0E0E3", "#CFE2F3",
    "#EAD1DC", "#D9D2E9", "#FCE5CD"
  )
  
  category_names <- c(
    timely_recruitment = "Timely Recruitment",
    participant_retention = "Participant Retention",
    data_completeness = "Data Completeness",
    participant_safety = "Participant Safety",
    intervention_adherence = "Intervention Adherence"
  )
  
  for (category_index in seq_along(category_order)) {
    category <- category_order[[category_index]]
    
    category_results <- combined_results |>
      dplyr::filter(.data$kri_category == category)
    
    if (nrow(category_results) == 0L) {
      next
    }
    
    if (category %in% names(category_names)) {
      sheet_name <- category_names[[category]]
    } else {
      sheet_name <- stringr::str_to_title(
        gsub("_", " ", category)
      )
    }
    
    sheet_name <- substr(sheet_name, 1L, 31L)
    
    if (category %in% names(category_colours)) {
      category_colour <- category_colours[[category]]
    } else {
      colour_index <- (
        (category_index - 1L) %%
          length(fallback_colours)
      ) + 1L
      
      category_colour <- fallback_colours[[colour_index]]
    }
    
    openxlsx::addWorksheet(
      workbook,
      sheetName = sheet_name,
      gridLines = FALSE,
      tabColour = category_colour
    )
    
    category_title_style <- openxlsx::createStyle(
      fontName = "Arial",
      fontSize = 12,
      textDecoration = "bold",
      halign = "center",
      valign = "center",
      fgFill = category_colour,
      border = "TopBottomLeftRight",
      borderColour = "#000000"
    )
    
    kri_title_style <- openxlsx::createStyle(
      fontName = "Arial",
      fontSize = 10,
      textDecoration = "bold",
      halign = "left",
      valign = "center",
      fgFill = category_colour,
      border = "TopBottomLeftRight",
      borderColour = "#000000"
    )
    
    site_style <- openxlsx::createStyle(
      fontName = "Arial",
      fontSize = 10,
      halign = "center",
      valign = "center",
      border = "TopBottomLeftRight",
      borderColour = "#000000"
    )
    
    integer_style <- openxlsx::createStyle(
      fontName = "Arial",
      fontSize = 10,
      halign = "center",
      valign = "center",
      numFmt = "0",
      border = "TopBottomLeftRight",
      borderColour = "#000000"
    )
    
    decimal_style <- openxlsx::createStyle(
      fontName = "Arial",
      fontSize = 10,
      halign = "center",
      valign = "center",
      numFmt = "0.0",
      border = "TopBottomLeftRight",
      borderColour = "#000000"
    )
    
    category_kri_order <- kri_order[
      kri_order %in% unique(category_results$kri_id)
    ]
    
    maximum_sites <- category_results |>
      dplyr::filter(!is.na(.data$site)) |>
      dplyr::group_by(.data$kri_id) |>
      dplyr::summarise(
        number_sites = dplyr::n_distinct(.data$site),
        .groups = "drop"
      ) |>
      dplyr::pull(.data$number_sites)
    
    maximum_sites <- max(maximum_sites, 1L, na.rm = TRUE)
    
    openxlsx::mergeCells(
      workbook,
      sheet = sheet_name,
      cols = seq_len(maximum_sites),
      rows = 1
    )
    
    openxlsx::writeData(
      workbook,
      sheet = sheet_name,
      x = sheet_name,
      startCol = 1,
      startRow = 1,
      colNames = FALSE
    )
    
    openxlsx::addStyle(
      workbook,
      sheet = sheet_name,
      style = category_title_style,
      rows = 1,
      cols = seq_len(maximum_sites),
      gridExpand = TRUE
    )
    
    openxlsx::setRowHeights(
      workbook,
      sheet = sheet_name,
      rows = 1,
      heights = 24
    )
    
    current_row <- 2L
    
    for (kri_number in seq_along(category_kri_order)) {
      selected_kri_id <- category_kri_order[[kri_number]]
      
      selected_result <- category_results |>
        dplyr::filter(.data$kri_id == selected_kri_id) |>
        dplyr::filter(!is.na(.data$site)) |>
        dplyr::arrange(.data$site)
      
      if (nrow(selected_result) == 0L) {
        next
      }
      
      number_sites <- nrow(selected_result)
      kri_name <- selected_result$kri_name[[1]]
      display_type <- selected_result$display_type[[1]]
      
      openxlsx::mergeCells(
        workbook,
        sheet = sheet_name,
        cols = seq_len(maximum_sites),
        rows = current_row
      )
      
      openxlsx::writeData(
        workbook,
        sheet = sheet_name,
        x = paste0(kri_number, ". ", kri_name),
        startCol = 1,
        startRow = current_row,
        colNames = FALSE
      )
      
      openxlsx::addStyle(
        workbook,
        sheet = sheet_name,
        style = kri_title_style,
        rows = current_row,
        cols = seq_len(maximum_sites),
        gridExpand = TRUE
      )
      
      openxlsx::writeData(
        workbook,
        sheet = sheet_name,
        x = t(selected_result$site),
        startCol = 1,
        startRow = current_row + 1L,
        colNames = FALSE,
        rowNames = FALSE
      )
      
      openxlsx::addStyle(
        workbook,
        sheet = sheet_name,
        style = site_style,
        rows = current_row + 1L,
        cols = seq_len(number_sites),
        gridExpand = TRUE
      )
      
      openxlsx::writeData(
        workbook,
        sheet = sheet_name,
        x = t(selected_result$display_value),
        startCol = 1,
        startRow = current_row + 2L,
        colNames = FALSE,
        rowNames = FALSE,
        keepNA = FALSE
      )
      
      selected_style <- switch(
        display_type,
        decimal = decimal_style,
        percentage = decimal_style,
        integer = integer_style,
        integer_style
      )
      
      openxlsx::addStyle(
        workbook,
        sheet = sheet_name,
        style = selected_style,
        rows = current_row + 2L,
        cols = seq_len(number_sites),
        gridExpand = TRUE
      )
      
      current_row <- current_row + 4L
    }
    
    openxlsx::setColWidths(
      workbook,
      sheet = sheet_name,
      cols = seq_len(maximum_sites),
      widths = 14
    )
    
    openxlsx::freezePane(
      workbook,
      sheet = sheet_name,
      firstRow = TRUE
    )
    
  }
  
  openxlsx::saveWorkbook(
    workbook,
    file = output_file,
    overwrite = TRUE
  )
  
  invisible(kri_results)
}
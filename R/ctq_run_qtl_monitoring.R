#Title: QTL Monitoring
#Author: Paigan Aspinall
#Version & Date: V1.1.0 14AUG2026
#R version: 4.4.3

#' Run QTL Monitoring
#'
#' Calculates Quality Tolerance Limit (QTL) values using a metadata-driven
#' specification. Each row of the metadata dataset defines one QTL to be
#' evaluated, including the QTL identifier, source dataset, REDCap event,
#' field(s), positive condition, optional conditional denominator logic,
#' and RAG threshold bands.
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
#' the total recruitment count as their denominator, including:
#' \itemize{
#'   \item \code{"participant_retention"}
#'   \item \code{"total_saes"}
#'   \item \code{"related_saes"}
#' }
#'
#' Source datasets are identified from the \code{export_name} column in the
#' metadata. The named datasets must exist in the supplied environment.
#'
#' Conditional filtering is optional. Where no conditional filtering is
#' required, \code{conditional_event}, \code{conditional_field}, and
#' \code{conditional_logic} should all be blank.
#'
#' Where conditional filtering is required, all three columns must be supplied.
#' Multiple conditional fields and corresponding conditions may be separated
#' using semicolons. All conditions are combined using AND logic.
#'
#' For example:
#'
#' \code{conditional_event = "week_22_arm_1"}
#'
#' \code{conditional_field = "hooccuryn; primary_carer"}
#'
#' \code{conditional_logic = "=1; =1"}
#'
#' requires both \code{hooccuryn == 1} and \code{primary_carer == 1} at the
#' \code{week_22_arm_1} event.
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
#'       fields may be separated by \code{";"}, where supported.}
#'     \item{event_name}{REDCap event used for the numerator calculation}
#'     \item{export_name}{Name of the source dataset object}
#'     \item{positive_condition}{Condition defining a positive QTL event}
#'     \item{negative_condition}{Condition defining a negative QTL event}
#'     \item{further_condition}{Additional QTL-specific information, such as
#'       recruitment target or denominator notes}
#'     \item{conditional_event}{Optional single REDCap event used to define an
#'       eligible population or denominator}
#'     \item{conditional_field}{Optional field or fields used to define the
#'       eligible population. Multiple fields should be separated by
#'       \code{";"}.}
#'     \item{conditional_logic}{Optional conditions corresponding to
#'       \code{conditional_field}. Multiple conditions should be separated by
#'       \code{";"}. Supported operators are \code{=}, \code{==}, \code{!=},
#'       \code{>}, \code{>=}, \code{<}, and \code{<=}.}
#'   }
#'
#' @param output_file Optional path for an Excel output file.
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
run_qtl_monitoring <- function(
    metadata,
    output_file = NULL,
    env = parent.frame()
) {
  
  results <- list()
  recruitment_count <- NA
  
  
  # ============================================================
  # HELPER FUNCTIONS
  # ============================================================
  # ------------------------------------------------------------
  # Calculate expected recruitment as of the current date
  # from monthly end-of-month recruitment targets
  # ------------------------------------------------------------
  
  get_expected_recruitment <- function(
    expected_recruitment_data,
    current_date = Sys.Date()
  ) {
    
    recruitment_plan <- expected_recruitment_data
    
    
    # Convert month values such as "Jul-26" to the first
    # day of the corresponding month
    recruitment_plan$month_start <- as.Date(
      paste0(
        "01-",
        recruitment_plan$month
      ),
      format = "%d-%b-%y"
    )
    
    
    # Calculate the final day of each month
    recruitment_plan$month_end <- as.Date(
      vapply(
        recruitment_plan$month_start,
        function(x) {
          
          next_month <- seq(
            as.Date(x),
            by = "month",
            length.out = 2
          )[2]
          
          as.character(
            next_month - 1
          )
          
        },
        character(1)
      )
    )
    
    
    # Ensure expected recruitment is numeric
    recruitment_plan$expected_recruitment <- as.numeric(
      recruitment_plan$expected_recruitment
    )
    
    
    # Sort chronologically
    recruitment_plan <- recruitment_plan |>
      dplyr::arrange(
        month_end
      )
    
    
    # ----------------------------------------------------------
    # Before the first planned month-end
    #
    # Interpolate from zero at the beginning of the first
    # month to the first month-end target.
    # ----------------------------------------------------------
    
    if (
      current_date <=
      recruitment_plan$month_end[1]
    ) {
      
      first_month_start <-
        recruitment_plan$month_start[1]
      
      first_month_end <-
        recruitment_plan$month_end[1]
      
      first_target <-
        recruitment_plan$expected_recruitment[1]
      
      
      if (
        current_date <
        first_month_start
      ) {
        
        return(0)
        
      }
      
      
      proportion_elapsed <-
        as.numeric(
          current_date -
            first_month_start + 1
        ) /
        as.numeric(
          first_month_end -
            first_month_start + 1
        )
      
      
      expected_today <-
        first_target *
        proportion_elapsed
      
      
      return(
        expected_today
      )
      
    }
    
    
    # ----------------------------------------------------------
    # After final planned month
    #
    # Hold expected recruitment at final target.
    # ----------------------------------------------------------
    
    if (
      current_date >=
      max(
        recruitment_plan$month_end
      )
    ) {
      
      return(
        recruitment_plan$expected_recruitment[
          nrow(
            recruitment_plan
          )
        ]
      )
      
    }
    
    
    # ----------------------------------------------------------
    # Find current month target
    # ----------------------------------------------------------
    
    current_row <- which(
      recruitment_plan$month_end >=
        current_date
    )[1]
    
    
    previous_row <-
      current_row - 1
    
    
    previous_date <-
      recruitment_plan$month_end[
        previous_row
      ]
    
    current_month_end <-
      recruitment_plan$month_end[
        current_row
      ]
    
    
    previous_target <-
      recruitment_plan$expected_recruitment[
        previous_row
      ]
    
    current_target <-
      recruitment_plan$expected_recruitment[
        current_row
      ]
    
    
    # ----------------------------------------------------------
    # Interpolate expected recruitment between the previous
    # and current month-end targets
    # ----------------------------------------------------------
    
    proportion_elapsed <-
      as.numeric(
        current_date -
          previous_date
      ) /
      as.numeric(
        current_month_end -
          previous_date
      )
    
    
    expected_today <-
      previous_target +
      (
        current_target -
          previous_target
      ) *
      proportion_elapsed
    
    
    return(
      expected_today
    )
    
  }
  add_result <- function(
    qtl_id,
    qtl_name,
    numerator,
    denominator,
    qtl_value,
    flag
  ) {
    
    data.frame(
      qtl_id = qtl_id,
      qtl_name = qtl_name,
      numerator = numerator,
      denominator = denominator,
      qtl_value = round(qtl_value, 2),
      flag = flag
    )
  }
  
  
  get_meta <- function(id) {
    
    metadata[
      metadata$qtl_id == id,
    ][1, ]
    
  }
  
  
  has_qtl <- function(id) {
    
    any(metadata$qtl_id == id)
    
  }
  
  
  # ------------------------------------------------------------
  # Identify blank metadata values
  # ------------------------------------------------------------
  
  is_blank_meta <- function(x) {
    
    if (length(x) == 0) {
      return(TRUE)
    }
    
    if (is.na(x)) {
      return(TRUE)
    }
    
    value <- trimws(
      as.character(x)
    )
    
    value == "" ||
      toupper(value) == "NA"
    
  }
  
  
  # ------------------------------------------------------------
  # Apply one condition to a dataframe
  # ------------------------------------------------------------
  
  apply_single_condition <- function(
    data,
    field_name,
    condition
  ) {
    
    operator <- stringr::str_extract(
      condition,
      "^(>=|<=|!=|==|=|>|<)"
    )
    
    
    # If no operator is given, assume equality
    if (is.na(operator)) {
      
      operator <- "="
      condition_value <- condition
      
    } else {
      
      condition_value <- stringr::str_remove(
        condition,
        "^(>=|<=|!=|==|=|>|<)"
      )
      
    }
    
    
    condition_value <- trimws(
      condition_value
    )
    
    
    # Remove optional quotation marks
    condition_value <- stringr::str_remove(
      condition_value,
      "^['\"]"
    )
    
    condition_value <- stringr::str_remove(
      condition_value,
      "['\"]$"
    )
    
    
    # ----------------------------------------------------------
    # Equality
    # ----------------------------------------------------------
    
    if (operator %in% c("=", "==")) {
      
      data <- data |>
        dplyr::filter(
          as.character(
            .data[[field_name]]
          ) == condition_value
        )
      
      
      # --------------------------------------------------------
      # Not equal
      # --------------------------------------------------------
      
    } else if (operator == "!=") {
      
      data <- data |>
        dplyr::filter(
          as.character(
            .data[[field_name]]
          ) != condition_value
        )
      
      
      # --------------------------------------------------------
      # Greater than
      # --------------------------------------------------------
      
    } else if (operator == ">") {
      
      data <- data |>
        dplyr::filter(
          suppressWarnings(
            as.numeric(
              as.character(
                .data[[field_name]]
              )
            )
          ) >
            as.numeric(condition_value)
        )
      
      
      # --------------------------------------------------------
      # Greater than or equal to
      # --------------------------------------------------------
      
    } else if (operator == ">=") {
      
      data <- data |>
        dplyr::filter(
          suppressWarnings(
            as.numeric(
              as.character(
                .data[[field_name]]
              )
            )
          ) >=
            as.numeric(condition_value)
        )
      
      
      # --------------------------------------------------------
      # Less than
      # --------------------------------------------------------
      
    } else if (operator == "<") {
      
      data <- data |>
        dplyr::filter(
          suppressWarnings(
            as.numeric(
              as.character(
                .data[[field_name]]
              )
            )
          ) <
            as.numeric(condition_value)
        )
      
      
      # --------------------------------------------------------
      # Less than or equal to
      # --------------------------------------------------------
      
    } else if (operator == "<=") {
      
      data <- data |>
        dplyr::filter(
          suppressWarnings(
            as.numeric(
              as.character(
                .data[[field_name]]
              )
            )
          ) <=
            as.numeric(condition_value)
        )
      
    }
    
    
    return(data)
    
  }
  
  
  # ------------------------------------------------------------
  # Get participant/family IDs meeting optional conditional logic
  #
  # Returns:
  # NULL = no conditional logic supplied
  # dataframe = IDs meeting all conditional criteria
  # ------------------------------------------------------------
  
  get_conditional_ids <- function(
    data,
    participant_id_col,
    conditional_event,
    conditional_field,
    conditional_logic,
    qtl_id
  ) {
    
    event_blank <- is_blank_meta(
      conditional_event
    )
    
    field_blank <- is_blank_meta(
      conditional_field
    )
    
    logic_blank <- is_blank_meta(
      conditional_logic
    )
    
    
    # ----------------------------------------------------------
    # All conditional metadata blank:
    # no restriction required
    # ----------------------------------------------------------
    
    if (
      event_blank &&
      field_blank &&
      logic_blank
    ) {
      
      return(NULL)
      
    }
    
    
    # ----------------------------------------------------------
    # Conditional metadata partially completed
    # ----------------------------------------------------------
    
    if (
      event_blank ||
      field_blank ||
      logic_blank
    ) {
      
      stop(
        paste0(
          "Conditional metadata is incomplete for QTL '",
          qtl_id,
          "'. conditional_event, conditional_field and ",
          "conditional_logic must either all be populated ",
          "or all be blank."
        )
      )
      
    }
    
    
    conditional_event <- trimws(
      as.character(
        conditional_event
      )
    )
    
    
    # ----------------------------------------------------------
    # Split semicolon-separated fields and conditions
    # ----------------------------------------------------------
    
    conditional_fields <- strsplit(
      as.character(
        conditional_field
      ),
      ";",
      fixed = TRUE
    )[[1]]
    
    
    conditional_conditions <- strsplit(
      as.character(
        conditional_logic
      ),
      ";",
      fixed = TRUE
    )[[1]]
    
    
    conditional_fields <- trimws(
      conditional_fields
    )
    
    conditional_conditions <- trimws(
      conditional_conditions
    )
    
    
    # ----------------------------------------------------------
    # Check field/condition counts match
    # ----------------------------------------------------------
    
    if (
      length(conditional_fields) !=
      length(conditional_conditions)
    ) {
      
      stop(
        paste0(
          "For QTL '",
          qtl_id,
          "', the number of conditional fields (",
          length(conditional_fields),
          ") does not match the number of conditional ",
          "conditions (",
          length(conditional_conditions),
          ")."
        )
      )
      
    }
    
    
    # ----------------------------------------------------------
    # Check conditional fields exist
    # ----------------------------------------------------------
    
    missing_fields <- setdiff(
      conditional_fields,
      names(data)
    )
    
    
    if (
      length(missing_fields) > 0
    ) {
      
      stop(
        paste0(
          "For QTL '",
          qtl_id,
          "', the following conditional field(s) were ",
          "not found in the source dataset: ",
          paste(
            missing_fields,
            collapse = ", "
          )
        )
      )
      
    }
    
    
    # ----------------------------------------------------------
    # Check ID column exists
    # ----------------------------------------------------------
    
    if (
      !participant_id_col %in%
      names(data)
    ) {
      
      stop(
        paste0(
          "Participant ID column '",
          participant_id_col,
          "' was not found for QTL '",
          qtl_id,
          "'."
        )
      )
      
    }
    
    
    # ----------------------------------------------------------
    # Filter to conditional event
    # ----------------------------------------------------------
    
    conditional_data <- data |>
      dplyr::filter(
        redcap_event_name ==
          conditional_event
      )
    
    
    # ----------------------------------------------------------
    # Apply all conditional criteria
    #
    # Sequential filtering = AND logic
    # ----------------------------------------------------------
    
    for (
      i in seq_along(
        conditional_fields
      )
    ) {
      
      conditional_data <-
        apply_single_condition(
          data = conditional_data,
          field_name = conditional_fields[i],
          condition = conditional_conditions[i]
        )
      
    }
    
    
    # ----------------------------------------------------------
    # Return distinct eligible IDs
    # ----------------------------------------------------------
    
    conditional_ids <- conditional_data |>
      dplyr::distinct(
        .data[[participant_id_col]]
      )
    
    
    return(
      conditional_ids
    )
    
  }
  
  
  # ------------------------------------------------------------
  # Restrict a dataset to optional eligible IDs
  # ------------------------------------------------------------
  
  restrict_to_conditional_ids <- function(
    data,
    conditional_ids,
    participant_id_col
  ) {
    
    if (
      is.null(
        conditional_ids
      )
    ) {
      
      return(data)
      
    }
    
    
    data <- data |>
      dplyr::semi_join(
        conditional_ids,
        by = participant_id_col
      )
    
    
    return(data)
    
  }
  
  
  # ============================================================
  # TIMELY RECRUITMENT
  # ============================================================
  
  if (
    has_qtl(
      "timely_recruitment"
    )
  ) {
    
    meta_row <- get_meta(
      "timely_recruitment"
    )
    
    data <- get(
      meta_row$export_name,
      envir = env
    )
    
    
    participant_id_col <-
      names(data)[1]
    
    
    conditional_ids <-
      get_conditional_ids(
        data = data,
        participant_id_col = participant_id_col,
        conditional_event = meta_row$conditional_event,
        conditional_field = meta_row$conditional_field,
        conditional_logic = meta_row$conditional_logic,
        qtl_id = meta_row$qtl_id
      )
    
    
    recruitment_data <- data |>
      dplyr::filter(
        redcap_event_name ==
          meta_row$event_name
      )
    
    
    recruitment_data <-
      restrict_to_conditional_ids(
        data = recruitment_data,
        conditional_ids = conditional_ids,
        participant_id_col = participant_id_col
      )
    
    
    # Count actual recruitment
    recruitment_count <-
      recruitment_data |>
      dplyr::filter(
        !is.na(
          .data[[meta_row$field]]
        )
      ) |>
      dplyr::distinct(
        .data[[participant_id_col]]
      ) |>
      nrow()
    
    
    # Get expected recruitment as of today from the
    # monthly recruitment plan
    expected_recruitment <-
      get_expected_recruitment(
        expected_recruitment_data =
          get(
            "expected_recruitment_data",
            envir = env
          ),
        current_date = Sys.Date()
      )
    
    
    # Avoid division by zero before recruitment is expected
    # to have started
    if (
      expected_recruitment <= 0
    ) {
      
      recruitment_qtl_value <- 100
      
    } else {
      
      recruitment_qtl_value <-
        recruitment_count /
        expected_recruitment *
        100
      
    }
    
    
    recruitment_flag <-
      dplyr::case_when(
        recruitment_qtl_value >=
          90 ~ "green",
        recruitment_qtl_value >=
          75 ~ "amber",
        TRUE ~ "red"
      )
    
    
    results[["timely_recruitment"]] <- add_result(
      meta_row$qtl_id,
      meta_row$qtl_description,
      recruitment_count,
      expected_recruitment,
      recruitment_qtl_value,
      recruitment_flag
    )
    
  }
  
  # ============================================================
  # PARTICIPANT RETENTION
  # ============================================================
  
  if (
    has_qtl(
      "participant_retention"
    )
  ) {
    
    if (
      is.na(
        recruitment_count
      )
    ) {
      
      stop(
        paste0(
          "participant_retention requires ",
          "timely_recruitment to be present first."
        )
      )
      
    }
    
    
    meta_row <- get_meta(
      "participant_retention"
    )
    
    data <- get(
      meta_row$export_name,
      envir = env
    )
    
    
    participant_id_col <-
      names(data)[1]
    
    
    conditional_ids <-
      get_conditional_ids(
        data = data,
        participant_id_col = participant_id_col,
        conditional_event = meta_row$conditional_event,
        conditional_field = meta_row$conditional_field,
        conditional_logic = meta_row$conditional_logic,
        qtl_id = meta_row$qtl_id
      )
    
    
    retention_data <- data |>
      dplyr::filter(
        redcap_event_name ==
          meta_row$event_name
      )
    
    
    retention_data <-
      restrict_to_conditional_ids(
        data = retention_data,
        conditional_ids = conditional_ids,
        participant_id_col = participant_id_col
      )
    
    
    positive_values <-
      stringr::str_extract_all(
        meta_row$positive_condition,
        "\\d+"
      )[[1]]
    
    
    retention_count <-
      retention_data |>
      dplyr::filter(
        as.character(
          .data[[meta_row$field]]
        ) %in%
          positive_values
      ) |>
      dplyr::distinct(
        .data[[participant_id_col]]
      ) |>
      nrow()
    
    
    # If conditional logic is supplied, use the number of
    # conditionally eligible participants as the denominator.
    # Otherwise retain the original recruitment denominator.
    
    if (
      !is.null(
        conditional_ids
      )
    ) {
      
      retention_denominator <-
        nrow(
          conditional_ids
        )
      
    } else {
      
      retention_denominator <-
        recruitment_count
      
    }
    
    
    retention_qtl_value <-
      retention_count /
      retention_denominator *
      100
    
    
    retention_flag <-
      dplyr::case_when(
        retention_qtl_value <
          10 ~ "green",
        retention_qtl_value >=
          10 &
          retention_qtl_value <=
          24 ~ "amber",
        TRUE ~ "red"
      )
    
    
    results[["participant_retention"]] <- add_result(
      meta_row$qtl_id,
      meta_row$qtl_description,
      retention_count,
      retention_denominator,
      retention_qtl_value,
      retention_flag
    )
    
  }
  
  
  # ============================================================
  # ENDPOINT COMPLETENESS
  # ============================================================
  
  if (
    has_qtl(
      "endpoint_completeness"
    )
  ) {
    
    meta_row <- get_meta(
      "endpoint_completeness"
    )
    
    data <- get(
      meta_row$export_name,
      envir = env
    )
    
    
    participant_id_col <-
      names(data)[1]
    
    
    endpoint_fields <-
      strsplit(
        as.character(
          meta_row$field
        ),
        ";",
        fixed = TRUE
      )[[1]]
    
    
    endpoint_fields <-
      trimws(
        endpoint_fields
      )
    
    
    conditional_ids <-
      get_conditional_ids(
        data = data,
        participant_id_col = participant_id_col,
        conditional_event = meta_row$conditional_event,
        conditional_field = meta_row$conditional_field,
        conditional_logic = meta_row$conditional_logic,
        qtl_id = meta_row$qtl_id
      )
    
    
    # ----------------------------------------------------------
    # Denominator
    #
    # If conditional logic exists:
    # denominator = participants meeting conditional logic.
    #
    # If conditional logic is blank:
    # denominator = all participants at the endpoint event.
    # ----------------------------------------------------------
    
    if (
      !is.null(
        conditional_ids
      )
    ) {
      
      endpoint_denominator <-
        nrow(
          conditional_ids
        )
      
      eligible_participants <-
        conditional_ids
      
    } else {
      
      eligible_participants <-
        data |>
        dplyr::filter(
          redcap_event_name ==
            meta_row$event_name
        ) |>
        dplyr::distinct(
          .data[[participant_id_col]]
        )
      
      
      endpoint_denominator <-
        nrow(
          eligible_participants
        )
      
    }
    
    
    endpoint_count <-
      data |>
      dplyr::filter(
        redcap_event_name ==
          meta_row$event_name
      ) |>
      dplyr::semi_join(
        eligible_participants,
        by = participant_id_col
      ) |>
      dplyr::filter(
        dplyr::if_all(
          dplyr::all_of(
            endpoint_fields
          ),
          ~ !is.na(.x)
        )
      ) |>
      dplyr::distinct(
        .data[[participant_id_col]]
      ) |>
      nrow()
    
    
    endpoint_qtl_value <-
      endpoint_count /
      endpoint_denominator *
      100
    
    
    endpoint_flag <-
      dplyr::case_when(
        endpoint_qtl_value >=
          90 ~ "green",
        endpoint_qtl_value >=
          75 ~ "amber",
        TRUE ~ "red"
      )
    
    
    results[["endpoint_completeness"]] <- add_result(
      meta_row$qtl_id,
      meta_row$qtl_description,
      endpoint_count,
      endpoint_denominator,
      endpoint_qtl_value,
      endpoint_flag
    )
    
  }
  
  
  # ============================================================
  # TOTAL SAEs
  # ============================================================
  
  if (
    has_qtl(
      "total_saes"
    )
  ) {
    
    if (
      is.na(
        recruitment_count
      )
    ) {
      
      stop(
        paste0(
          "total_saes requires timely_recruitment ",
          "to be present first."
        )
      )
      
    }
    
    
    meta_row <- get_meta(
      "total_saes"
    )
    
    data <- get(
      meta_row$export_name,
      envir = env
    )
    
    
    participant_id_col <-
      names(data)[1]
    
    
    conditional_ids <-
      get_conditional_ids(
        data = data,
        participant_id_col = participant_id_col,
        conditional_event = meta_row$conditional_event,
        conditional_field = meta_row$conditional_field,
        conditional_logic = meta_row$conditional_logic,
        qtl_id = meta_row$qtl_id
      )
    
    
    total_sae_data <- data |>
      dplyr::filter(
        redcap_event_name ==
          meta_row$event_name
      )
    
    
    total_sae_data <-
      restrict_to_conditional_ids(
        data = total_sae_data,
        conditional_ids = conditional_ids,
        participant_id_col = participant_id_col
      )
    
    
    sae_value <-
      stringr::str_extract(
        meta_row$positive_condition,
        "\\d+"
      )
    
    
    total_sae_count <-
      total_sae_data |>
      dplyr::filter(
        as.character(
          .data[[meta_row$field]]
        ) ==
          sae_value
      ) |>
      dplyr::distinct(
        .data[[participant_id_col]]
      ) |>
      nrow()
    
    
    # Conditional logic, where supplied, defines the denominator.
    # Otherwise retain the original recruitment denominator.
    
    if (
      !is.null(
        conditional_ids
      )
    ) {
      
      total_sae_denominator <-
        nrow(
          conditional_ids
        )
      
    } else {
      
      total_sae_denominator <-
        recruitment_count
      
    }
    
    
    total_sae_qtl_value <-
      total_sae_count /
      total_sae_denominator *
      100
    
    
    total_sae_flag <-
      dplyr::case_when(
        total_sae_qtl_value <
          10 ~ "green",
        total_sae_qtl_value >=
          10 &
          total_sae_qtl_value <
          25 ~ "amber",
        TRUE ~ "red"
      )
    
    
    results[["total_saes"]] <- add_result(
      meta_row$qtl_id,
      meta_row$qtl_description,
      total_sae_count,
      total_sae_denominator,
      total_sae_qtl_value,
      total_sae_flag
    )
    
  }
  
  
  # ============================================================
  # RELATED SAEs
  # ============================================================
  
  if (
    has_qtl(
      "related_saes"
    )
  ) {
    
    if (
      is.na(
        recruitment_count
      )
    ) {
      
      stop(
        paste0(
          "related_saes requires timely_recruitment ",
          "to be present first."
        )
      )
      
    }
    
    
    meta_row <- get_meta(
      "related_saes"
    )
    
    data <- get(
      meta_row$export_name,
      envir = env
    )
    
    
    participant_id_col <-
      names(data)[1]
    
    
    conditional_ids <-
      get_conditional_ids(
        data = data,
        participant_id_col = participant_id_col,
        conditional_event = meta_row$conditional_event,
        conditional_field = meta_row$conditional_field,
        conditional_logic = meta_row$conditional_logic,
        qtl_id = meta_row$qtl_id
      )
    
    
    related_sae_data <- data |>
      dplyr::filter(
        redcap_event_name ==
          meta_row$event_name
      )
    
    
    related_sae_data <-
      restrict_to_conditional_ids(
        data = related_sae_data,
        conditional_ids = conditional_ids,
        participant_id_col = participant_id_col
      )
    
    
    # Allow one or more numerator fields separated by semicolons
    
    related_sae_fields <-
      strsplit(
        as.character(
          meta_row$field
        ),
        ";",
        fixed = TRUE
      )[[1]]
    
    
    related_sae_fields <-
      trimws(
        related_sae_fields
      )
    
    
    related_sae_conditions <-
      strsplit(
        as.character(
          meta_row$positive_condition
        ),
        ";",
        fixed = TRUE
      )[[1]]
    
    
    related_sae_conditions <-
      trimws(
        related_sae_conditions
      )
    
    
    # If only one field/condition is supplied, that is valid.
    # If multiple are supplied, numbers must match.
    
    if (
      length(
        related_sae_fields
      ) !=
      length(
        related_sae_conditions
      )
    ) {
      
      stop(
        paste0(
          "For related_saes, the number of fields (",
          length(
            related_sae_fields
          ),
          ") does not match the number of positive conditions (",
          length(
            related_sae_conditions
          ),
          ")."
        )
      )
      
    }
    
    
    # Apply numerator conditions using AND logic
    
    for (
      i in seq_along(
        related_sae_fields
      )
    ) {
      
      related_sae_data <-
        apply_single_condition(
          data = related_sae_data,
          field_name = related_sae_fields[i],
          condition = related_sae_conditions[i]
        )
      
    }
    
    
    related_sae_count <-
      related_sae_data |>
      dplyr::distinct(
        .data[[participant_id_col]]
      ) |>
      nrow()
    
    
    if (
      !is.null(
        conditional_ids
      )
    ) {
      
      related_sae_denominator <-
        nrow(
          conditional_ids
        )
      
    } else {
      
      related_sae_denominator <-
        recruitment_count
      
    }
    
    
    related_sae_qtl_value <-
      related_sae_count /
      related_sae_denominator *
      100
    
    
    related_sae_flag <-
      dplyr::case_when(
        related_sae_qtl_value <
          10 ~ "green",
        related_sae_qtl_value >=
          10 &
          related_sae_qtl_value <
          25 ~ "amber",
        TRUE ~ "red"
      )
    
    
    results[["related_saes"]] <- add_result(
      meta_row$qtl_id,
      meta_row$qtl_description,
      related_sae_count,
      related_sae_denominator,
      related_sae_qtl_value,
      related_sae_flag
    )
    
  }
  
  
  # ============================================================
  # IMP COMPLIANCE
  # ============================================================
  
  if (
    has_qtl(
      "imp_compliance"
    )
  ) {
    
    meta_row <- get_meta(
      "imp_compliance"
    )
    
    data <- get(
      meta_row$export_name,
      envir = env
    )
    
    
    participant_id_col <-
      names(data)[1]
    
    
    conditional_ids <-
      get_conditional_ids(
        data = data,
        participant_id_col = participant_id_col,
        conditional_event = meta_row$conditional_event,
        conditional_field = meta_row$conditional_field,
        conditional_logic = meta_row$conditional_logic,
        qtl_id = meta_row$qtl_id
      )
    
    
    # ----------------------------------------------------------
    # Denominator
    #
    # Conditional logic supplied:
    # participants meeting conditional criteria.
    #
    # Conditional logic blank:
    # all participants at QTL event.
    # ----------------------------------------------------------
    
    if (
      !is.null(
        conditional_ids
      )
    ) {
      
      eligible_participants <-
        conditional_ids
      
      imp_denominator <-
        nrow(
          conditional_ids
        )
      
    } else {
      
      eligible_participants <-
        data |>
        dplyr::filter(
          redcap_event_name ==
            meta_row$event_name
        ) |>
        dplyr::distinct(
          .data[[participant_id_col]]
        )
      
      
      imp_denominator <-
        nrow(
          eligible_participants
        )
      
    }
    
    
    positive_value <-
      stringr::str_extract(
        meta_row$positive_condition,
        "\\d+"
      )
    
    
    imp_count <-
      data |>
      dplyr::filter(
        redcap_event_name ==
          meta_row$event_name
      ) |>
      dplyr::semi_join(
        eligible_participants,
        by = participant_id_col
      ) |>
      dplyr::filter(
        as.character(
          .data[[meta_row$field]]
        ) ==
          positive_value
      ) |>
      dplyr::distinct(
        .data[[participant_id_col]]
      ) |>
      nrow()
    
    
    imp_qtl_value <-
      imp_count /
      imp_denominator *
      100
    
    
    imp_flag <-
      dplyr::case_when(
        imp_qtl_value >=
          90 ~ "green",
        imp_qtl_value >=
          75 ~ "amber",
        TRUE ~ "red"
      )
    
    
    results[["imp_compliance"]] <- add_result(
      meta_row$qtl_id,
      meta_row$qtl_description,
      imp_count,
      imp_denominator,
      imp_qtl_value,
      imp_flag
    )
    
  }
  
  
  # ============================================================
  # COMBINE RESULTS
  # ============================================================
  
  results_df <-
    dplyr::bind_rows(
      results
    )
  
  
  # ============================================================
  # OPTIONAL EXCEL OUTPUT
  # ============================================================
  
  if (
    !is.null(
      output_file
    )
  ) {
    
    if (
      !grepl(
        "\\.xlsx$",
        output_file,
        ignore.case = TRUE
      )
    ) {
      
      output_file <-
        paste0(
          output_file,
          ".xlsx"
        )
      
    }
    
    
    wb <-
      openxlsx::createWorkbook(
        creator = "Paigan Aspinall"
      )
    
    
    openxlsx::addWorksheet(
      wb,
      "QTL Monitoring"
    )
    
    
    title_style <-
      openxlsx::createStyle(
        fontSize = 14,
        textDecoration = "bold",
        fgFill = "#D9EAD3",
        halign = "center",
        border = "Bottom"
      )
    
    
    header_style <-
      openxlsx::createStyle(
        textDecoration = "bold",
        fgFill = "#D9EAD3",
        border = "TopBottomLeftRight"
      )
    
    
    percent_style <-
      openxlsx::createStyle(
        numFmt = "0.0"
      )
    
    
    rag_styles <- list(
      
      green =
        openxlsx::createStyle(
          fgFill = "#C6EFCE"
        ),
      
      amber =
        openxlsx::createStyle(
          fgFill = "#FFE699"
        ),
      
      red =
        openxlsx::createStyle(
          fgFill = "#F4CCCC"
        )
      
    )
    
    
    openxlsx::mergeCells(
      wb,
      sheet = 1,
      cols = 1:4,
      rows = 1
    )
    
    
    openxlsx::writeData(
      wb,
      sheet = 1,
      x = "Quality Tolerance Limit Monitoring",
      startRow = 1,
      colNames = FALSE
    )
    
    
    openxlsx::addStyle(
      wb,
      sheet = 1,
      style = title_style,
      rows = 1,
      cols = 1:4,
      gridExpand = TRUE
    )
    
    
    openxlsx::writeData(
      wb,
      sheet = 1,
      x = results_df,
      startRow = 3,
      headerStyle = header_style,
      withFilter = TRUE
    )
    
    
    if (
      nrow(
        results_df
      ) > 0
    ) {
      
      openxlsx::addStyle(
        wb,
        sheet = 1,
        style = percent_style,
        rows = 4:(
          nrow(
            results_df
          ) + 3
        ),
        cols = 3,
        gridExpand = TRUE,
        stack = TRUE
      )
      
    }
    
    
    for (colour in names(rag_styles)) {
      
      rows <- which(
        results_df$flag == colour
      )
      
      if (length(rows) > 0) {
        
        openxlsx::addStyle(
          wb,
          sheet = 1,
          style = rag_styles[[colour]],
          rows = rows + 3,
          cols = 6,
          gridExpand = TRUE,
          stack = TRUE
        )
        
      }
    }
    
    
    openxlsx::setColWidths(
      wb,
      sheet = 1,
      cols = 1:4,
      widths = "auto"
    )
    
    
    openxlsx::freezePane(
      wb,
      sheet = 1,
      firstRow = TRUE,
      firstCol = TRUE
    )
    
    
    openxlsx::saveWorkbook(
      wb,
      output_file,
      overwrite = TRUE
    )
    
  }
  
  
  # ============================================================
  # RETURN RESULTS
  # ============================================================
  
  return(
    results_df
  )
  
}
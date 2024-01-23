# Author: Paigan Aspinall
# Date: 23JAN2024
# R version: 4.2.2
#' Generate a stacked barchart to show timepoint completeness.
#'
#' This function produces a barchat that visualizes event completeness by allocation or site.
#'
#' @param data A complete dataset.
#' @param timepoint_names A vector containing the list of timepoint names.
#' @param category The category by which data will be organised (e.g. "Site" or "Allocation").
#' @param api_token The API token for accessing REDCap.
#' @param test Logical, indicating whether to use the test environment (default is FALSE).
#'
#' @return A dataset summarising event completeness.
#'
#'
#' @importFrom dplyr "%>%"
#' 
#' @examples
#' timepoint_names <- c("week_0_blinded_ass_arm_1", "week_2_blinded_ass_arm_1", "week_6_blinded_ass_arm_1", "week_8_blinded_as_arm_1", "week_22_blinded_as_arm_1")
#' 
#' example_plot <- plot_crf_completeness(dataset, timepoint_names, "Site", your_api_token, test=TRUE)
#'
#' @export
#'

plot_crf_completeness <- function(dataset, timepoint_names, category, api_token, test=FALSE){

  complete_data <- plot_crf_completeness_dataset_preparation(dataset, timepoint_names, category, api_token, test)
  
  status_colors <- c("Complete" = "#09eb18", "Partially complete" = "#ffdd00", "Not started" = "#ff0400")
  
  complete_data$timepoint_completeness <- factor(complete_data$timepoint_completeness, levels = c("Complete", "Partially complete", "Not started"))
  
  combinations_counts <- complete_data %>%
    count(event_name, .data[[category]])
  most_common_combination <- combinations_counts %>%
    filter(n == max(n))
  maximum <- most_common_combination$n[1] 
  if (maximum < 40) {
    increment <- 5
  } else if (maximum < 100) {
    increment <- 10
  } else if (maximum < 200) {
    increment <- 20
  } else {
    increment <- 50
  }
  
  ggplot(complete_data, aes(x = complete_data[[category]], fill = timepoint_completeness)) +
    geom_bar(position = 'stack') +
    facet_wrap(~event_name) +
    labs(title = paste0("Timepoint completeness by ", tolower(category)), y = "Number of participants", x = category) +
    scale_fill_manual(values = status_colors, name = "Completeness") +
    scale_y_continuous(breaks = seq(0, maximum, by = increment)) +
    theme_bw() +
    theme(legend.position = "bottom")
}
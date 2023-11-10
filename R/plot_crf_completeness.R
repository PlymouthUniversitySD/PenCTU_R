#' Generate a stacked barchart to show timepoint completeness.
#'
#' This function produces a barchat that visualizes event completeness by allocation or site.
#'
#' @param data A complete dataset.
#' @param timepoint_names A vector containing the list of timepoint names.
#' @param category The category by which data will be organised, must be "Site" or "Allocation".
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

  completedata <- plot_crf_completeness_dataset_preparation(dataset, timepoint_names, category, api_token, test)
  
  status_colors <- c("Complete" = "#52BE80", "Partially complete" = "#F4D03F", "Not started" = "#EC7063")
  
  if (category == "Site") {
    combinations_counts <- completedata %>%
      count(event_name, Site)
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
    
    ggplot(completedata, aes(x = Site, fill = timepoint_completeness)) +
      geom_bar(position = 'stack') +
      facet_wrap(~event_name) +
      labs(title = "Timepoint completeness by site", y = "Number of participants") +
      scale_fill_manual(values = status_colors, name = "Completeness") +
      scale_y_continuous(breaks = seq(0, maximum, by = increment)) +
      theme_bw() +
      theme(legend.position = "bottom")
    
  } else {
    combinations_counts <- completedata %>%
      count(event_name, Allocation)
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
    ggplot(completedata, aes(x = Allocation, fill = timepoint_completeness)) +
      geom_bar(position='stack') +
      facet_wrap(~event_name) +
      labs(title = "Timepoint completeness by site", y = "Number of participants") +
      scale_fill_manual(values = status_colors, name = "Completeness") +
      scale_y_continuous(breaks = seq(0, maximum, by = increment)) +
      theme_bw() +
      theme(legend.position = "bottom")
  }
}

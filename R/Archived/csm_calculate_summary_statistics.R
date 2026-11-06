#Title: Summary Statistics Function
#Author: Paigan Aspinall
#Version & Date: V1.0.0 24APR2026
#R version: 4.4.3

#' Calculate summary statistics for a numeric vector
#'
#' This function calculates basic descriptive statistics for a numeric vector,
#' including the number of non-missing observations, mean, standard deviation,
#' median, interquartile range, minimum, and maximum.
#'
#' @param x A numeric vector.
#'
#' @return A named numeric vector containing:
#' \itemize{
#'   \item \code{n}: number of non-missing observations
#'   \item \code{mean}: mean value
#'   \item \code{sd}: standard deviation
#'   \item \code{median}: median value
#'   \item \code{IQR}: interquartile range
#'   \item \code{min}: minimum value
#'   \item \code{max}: maximum value
#' }
#'
#' @examples
#' summary_stats(c(1, 2, 3, 4, NA))
#'
#' @export
#'

summary_stats <- function(x) {
  c(
    n = sum(!is.na(x)),
    mean = mean(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE),
    median = median(x, na.rm = TRUE),
    IQR = IQR(x, na.rm = TRUE),
    min = min(x, na.rm = TRUE),
    max = max(x, na.rm = TRUE)
  )
}
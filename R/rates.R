#' Calculate a General Rate per Population Unit
#'
#' This function computes a general rate (e.g., crude birth rate, crude death
#' rate, fertility rate) by dividing a numerator (e.g., count) by a denominator
#' (e.g., population) and scaling by a population unit (default 1,000).
#'
#' It supports tidy-evaluation so that output column names and input columns
#' can be supplied unquoted.
#'
#' @param data A data frame containing the variables used in the calculation.
#' @param rate_name Name of the new rate column to create, supplied quoted.
#' @param numerator Column containing numerator counts, supplied quoted.
#' @param denominator Column containing population denominators,
#'   supplied quoted.
#' @param by_pop Numeric. Population multiplier (e.g., 1000 for rate per 1,000).
#'   Defaults to `1000`.
#' @param digits Numeric. Number of decimal places to round the result to.
#'   Defaults to `2`.
#'
#' @return A data frame identical to `data` but with an additional column
#'   named after `rate_name` argument containing the computed rate.
#'
#' @export
calc_general_rate <- function(data,
                              rate_name,
                              numerator,
                              denominator,
                              by_pop = 1000,
                              digits = 2) {

  stopifnot(
    is.data.frame(data),
    numerator %in% names(data),
    denominator %in% names(data),
    is.numeric(by_pop),
    length(by_pop) == 1,
    is.numeric(digits),
    length(digits) == 1
  )

  rate <- round(data[[numerator]] / data[[denominator]] * by_pop, digits)
  dplyr::mutate(data, "{rate_name}" := rate)
}

#' Calculate crude birth rate by usual residence
#'
#' Calculates the crude birth rate (CBR) by year of birth and usual residence
#' using live birth registration data and population denominators.
#' The rate is expressed per a specified population size (default 1,000).
#'
#' @param data A data frame containing birth registration records. Must include
#'   variables `live_or_still`, `dobyr`, and `usual_residence`.
#' @param pop A data frame containing population denominators. Must include
#'   variables `year`, `geog`, and `population`.
#' @param by_pop Numeric. The population base for the crude birth rate
#'   (e.g. 1000 for rates per 1,000 population). Default is `1000`.
#' @param digits Integer. Number of decimal places to round the crude birth rate
#'   to. Default is `2`.
#'
#' @details
#' The function:
#' \itemize{
#'   \item Filters the registration data to live births only.
#'   \item Counts births by year of birth and usual residence.
#'   \item Creates a country-level total for each year  ("Not stated" in usual
#'   residence is counted in country-level but not presented in breakdown).
#'   \item Joins population denominators by year and geography.
#'   \item Calculates crude birth rates using `calc_general_rate()`.
#'   \item Excludes records with "Not stated" usual residence.
#' }
#'
#' The crude birth rate is calculated as:
#' \deqn{(Number\ of\ live\ births / Population) \times Population\ multiplier}
#'
#' @return
#' A tibble with one row per year and usual residence, containing:
#' \itemize{
#'   \item \code{dobyr}: Year of birth
#'   \item \code{usual_residence}: Area of usual residence
#'   \item \code{crude_birth_rate}: Crude birth rate per `by_pop` population
#' }
#'
#' @seealso
#' \code{\link{calc_general_rate}}, \code{\link{group_by_summarise_count}},
#' \code{\link{group_by_summarise_sum}}
#'
#' @export
calc_cbr_usual_residence <- function(data,
                                     pop,
                                     fcts,
                                     by_pop = 1000,
                                     digits = 2) {

  whole_pop <- pop |>
    dplyr::filter(Age == "All ages" & Sex == "All sexes") |>
    dplyr::rename("Place of usual residence" = "Geography")

  data |>
    general_count_by_var(
      agg_cols = c("dobyr", "usual_residence"),
      agg_cols_pub_names = c("Year", "Place of usual residence"),
      agg_count_colname = "Counts of live births",
      fcts = fcts,
      pre_agg_levels_to_keep = list(live_or_still = "Live birth"),
      post_agg_levels_to_keep = list(Year = fcts$dobyr)
    ) |>
    dplyr::left_join(whole_pop, by = c("Year", "Place of usual residence")) |>
    calc_general_rate(
      rate_name = "Crude birth rate",
      numerator = "Counts of live births",
      denominator = "Population",
      by_pop = by_pop,
      digits = digits
    ) |>
    dplyr::select(Year, `Place of usual residence`, `Counts of live births`,
                  Population, `Crude birth rate`) |>
    dplyr::filter(`Place of usual residence` != "Not stated") |>
    droplevels()
}

#' Calculate Age-Specific Fertility Rates for Mothers
#'
#' This function computes age-specific fertility rates (ASFR) for mothers by
#' combining a dataset of birth records with population denominators grouped
#' into 5-year age bands. It produces rates per 1,000 women for each
#' age group, geography, and year.
#'
#' @param data A data frame containing birth records. Must include:
#'   * `dobyr` — year of birth
#'   * `age_grp_m` — mother's age group
#'   * `usual_residence` — geographic area of residence
#'   * `live_or_still` — indicator of live or still birth
#'
#' @param pop A data frame containing population counts. Must include:
#'   * `year` — year of population estimate
#'   * `geog` — geography
#'   * `age` — single-year age
#'   * `population` — count of women
#'
#' @param fcts A list of factor level definitions used by the `group_age()`
#'   function (e.g., mother age group labels). Typically contains
#'   `birth_fcts$mother_age_group`.
#'
#' @inheritParams calc_general_rate
#'
#' @return A tibble containing:
#'   * `dobyr` — year
#'   * `age_grp_m` — mother’s age group (including "All ages")
#'   * `usual_residence` — geography (including "Country")
#'   * `age_specific_fert_rate` — ASFR per 1,000 women
#'
#' The returned table is sorted by decreasing year, then age group, then
#' geography.
#'
#' @details
#'
#' The function performs the following steps:
#'   1. Filters the population to fertile ages and groups into 5-year age bands.
#'   2. Adds an "All ages" population denominator for each year–geography.
#'   3. Harmonises birth data age groups (e.g., "Under 15" and "15 to 19" →
#'   "Under 20").
#'   4. Summarises birth counts and generates country-level aggregates.
#'   5. Adds an "All ages" birth numerator for each geography–year.
#'   6. Joins births with population denominators.
#'   7. Computes age-specific fertility rates per 1,000 women.
#' The numerator is the number of live births to mothers in each
#' age group, geography, and year.
#'
#' The denominator is the female population in the same age group,
#' geography, and year.
#'
#' Age groups are aligned between the birth data and population data by
#' collapsing `"Under 15"` and `"15 to 19"` into `"Under 20"`.
#'
#' The fertility rate is calculated as:
#'
#' \deqn{ASFR = (births / population) * 1000}
#'
#' Rows where `usual_residence == "Not stated"` are dropped.
#'
#' @export
calc_age_spec_fert_rate_m <- function(data,
                                      pop,
                                      fcts,
                                      pop_sex = "Female",
                                      fert_range = 15:44,
                                      by_pop = 1000,
                                      digits = 1) {

  pop_asfr <- pop |>
    process_pop_for_asfr(fcts, pop_sex, fert_range) |>
    dplyr::rename("Place of usual residence" = "Geography")

  data |>
    dplyr::mutate(
      asfr_mother_age_group = dplyr::case_when(
        age_grp_m %in% c("Under 15", "15 to 19") ~ "Under 20",
        .default = age_grp_m
      )
    ) |>
    general_count_by_var(
      agg_cols = c("dobyr", "asfr_mother_age_group", "usual_residence"),
      agg_cols_pub_names = c("Year", "Mothers age group",
                             "Place of usual residence"),
      agg_count_colname = "Counts of live births",
      fcts = fcts,
      pre_agg_levels_to_keep = list(live_or_still = "Live birth"),
      post_agg_levels_to_keep = list(Year = fcts$dobyr)
    ) |>
    dplyr::left_join(
      pop_asfr,
      by = c("Year", "Mothers age group", "Place of usual residence")
    ) |>
    calc_general_rate(
      "Age-specific fertility rate",
      "Counts of live births",
      "Population",
      by_pop = by_pop,
      digits = digits
    ) |>
    dplyr::filter(`Mothers age group` != "All ages" &
                    `Place of usual residence` != "Not stated") |>
    droplevels()

}


#' Process population data for Age-Specific Fertility Rate calculation
#'
#' This function filters population data to the fertility age range,
#' groups ages into 5-year bands, aggregates population counts,
#' and prepares age‑grouped totals for ASFR calculations.
#'
#' @param pop A data frame containing at least the columns:
#'   `year`, `geog`, `age`, and `population`.
#' @param pop_sex Character string containing either "Female", "Male" or
#'   "All people". Default is "Female".
#' @param fcts A list containing lookup vectors or functions used for grouping,
#'   specifically `mother_age_group` for mapping age groups.
#' @param fert_range An integer vector giving the fertility age range.
#'   Defaults to `15:44`.
#'
#' @return A data frame containing:
#'   * Population totals by year, geography, and 5‑year age group;
#'   * A row where all age groups are aggregated into `"All ages"`;
#'   * A modified youngest group labelled `"Under 20"`.
#'
#' @export
process_pop_for_asfr <- function(pop,
                                 fcts,
                                 pop_sex = "Female",
                                 fert_range = 15:44) {

  length_fct <- (((max(fert_range) - 4) - (min(fert_range) + 5)) / 5) + 3

  stopifnot(
    is.data.frame(pop),
    sum(is.na(pop)) == 0,
    all(c("Year", "Geography", "Age", "Population") %in% names(pop)),
    is.list(fcts),
    "asfr_mother_age_group" %in% names(fcts),
    is.integer(fert_range) && is.numeric(fert_range),
    ((max(fert_range) - min(fert_range) + 1) / 5) %% 1 == 0,
    length(fcts$asfr_mother_age_group) == length_fct
  )

  pop |>
    dplyr::filter(Age %in% fert_range & Sex == pop_sex) |>
    dplyr::mutate(Age = as.numeric(as.character(Age))) |>
    group_age(
      age_col = "Age",
      new_col = "Mothers age group",
      start_brk = min(fert_range + 5),
      end_brk = max(fert_range - 4),
      brk_step = 5,
      col_factor = fcts$asfr_mother_age_group
    ) |>
    group_by_summarise_sum(
      c("Year", "Geography", "Mothers age group"),
      "Population"
    )
}

#' Simulate population data by age, sex, and region (wide format)
#'
#' Generates a synthetic (dummy) population dataset by single year of age (0 to
#' 90 years), sex, and region, including multiple years of population counts.
#' National-level totals are also calculated and appended to the dataset.
#'
#' @param seed Integer. Random seed for reproducibility. Default is 249.
#'
#' @returns A data frame with synthetic population counts by region, sex, and
#' age, including additional aggregated rows for the total "Country".
#'
#' @details
#' Population counts are constant within each group in the base dataset
#' and are summed across regions to create national totals. The returned
#' data includes both regional and aggregated country-level records.
#'
#' @examples
#' create_population_data()
#'
#' @export
create_population_data <- function(seed = 249) {

  regions <- paste0("Region", c("A", "B", "C", "D"))
  pop_ages <- 0:90

  pops <- data.frame(
    birth1c = rep(regions, each = length(pop_ages) * 2),
    birth2a = rep(
      rep(c("Female", "Male"), each = length(pop_ages)),
      length(regions)
    ),
    pop_age = rep(pop_ages, 2 * length(regions)),
    population_2017 = rep(390),
    population_2018 = rep(412),
    population_2019 = rep(452),
    population_2020 = rep(441),
    population_2021 = rep(475),
    population_2022 = rep(532),
    population_2023 = rep(525)
  )

  pops |>
    dplyr::group_by(birth2a, pop_age) |>
    dplyr::summarise(
      birth1c = "Country",
      dplyr::across(dplyr::starts_with("population"), sum),
      .groups = "drop"
    ) |>
    dplyr::bind_rows(pops)
}


#' Simulate tidy (long format) population data
#'
#' Generates a synthetic population dataset in tidy format with one row with a
#' population per combination of year, geography, sex, and single year of age.
#'
#' The dataset includes:
#'
#' * Four consecutive years starting at `yr_start`
#' * Four regions: `"RegionA"`, `"RegionB"`, `"RegionC"`, `"RegionD"`
#' * Two sex categories: `"Male"` and `"Female"`
#' * Ages from 0 to 90
#'
#' A random integer `population` value between 0 and 500 is generated for each
#' demographic combination. Setting the `seed` ensures full reproducibility.
#'
#' @param seed Integer. Random seed for reproducibility. Default is 97.
#' @param yr_start Integer. The first year to include in the dataset.
#' Data will cover three more/consecutive years from the first year. Default is
#' 2022.
#'
#' @returns A data frame with the following columns:
#' * `year` — integer year
#' * `geog` — region name string (`"RegionA"`–`"RegionD"`)
#' * `sex` — `"Male"` or `"Female"`
#' * `pop_age` — pop_age in years (0–90)
#' * `population` — randomly generated integer between 0 and 500
#'
#' @examples
#' create_tidy_pop_data()
#'
#' @export
create_tidy_pop_data <- function(seed = 97, yr_start = 2022) {

  set.seed(seed)

  tidyr::expand_grid(
    year = yr_start:(yr_start + 3),
    geog = paste0("Region", c("A", "B", "C", "D")),
    sex = c("Male", "Female"),
    pop_age = 0:90
  ) |>
    dplyr::mutate(
      population = sample(0:500, dplyr::n(), replace = TRUE)
    )
}


#' Add totals to tidy population data
#'
#' Aggregates tidy population data produced by `create_tidy_pop_data()` to
#' produce totals across categories, using a supplied set of factor levels
#' (`cfg`).
#' The function filters to the years of interest from the `cfg`, standardises
#' factor levels, performs aggregation for totals, and removes unwanted output
#' categories.
#' The resulting dataset includes all original rows plus all aggregated totals,
#' with factors ordered according to the levels defined in `cfg`.
#'
#' @param data Data frame of tidy population data, such as that created by
#' `create_tidy_pop_data()`. Must include columns: `sex`, `pop_age`, `geog`,
#' and `population`.
#'
#' @param cfg A named list specifying the valid levels for each demographic
#' dimension. It must contain:
#'
#'   * `sex`  – character vector of sex categories (e.g., `c("Male","Female")`)
#'   * `pop_age`  – vector of pop_age groups (numeric or character)
#'   * `geog` – character vector of geographical units
#'
#' These vectors are used to construct factor levels for the output table.
#'
#' @returns A data frame with aggregated population counts at four levels:
#'
#'   * individual sex × pop_age × geography
#'   * all people × pop_age × geography
#'   * sex × all ages × geography
#'   * sex × pop_age × country
#'
#' All three categorical variables (`sex`, `pop_age`, `geog`) are returned as
#' ordered factors.
#'
#' @details
#' This function uses helper functions:
#' * [factorise_cols()]
#' * [general_count_by_var()]
#'
#' @export
add_totals_tidy_pop <- function(pop_data, fcts) {
  pop_data |>
    dplyr::filter(year %in% fcts$year) |>
    factorise_cols(level_list = fcts, strict = FALSE) |>
    general_count_by_var(
      agg_cols = c("year", "geog", "sex", "pop_age"),
      agg_cols_pub_names = c("Year", "Geography", "Sex", "Age"),
      agg_count_colname = "Population",
      count_or_sum = "sum",
      sum_col = "population",
      fcts = fcts,
      pre_agg_levels_to_keep = list(year = fcts$year)
    ) |>
    dplyr::filter(Year != "All years", Geography != "Not stated",
                  Sex != "Not stated") |>
    droplevels()
}

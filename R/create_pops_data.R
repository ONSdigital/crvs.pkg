#' Create wide dummy populations data - Not currently in use
#'
#' @param seed Numerical value of seed for reproducibility. Default is 249.
#'
#' @return Data frame.
#' @export
#'
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


#' Generate Synthetic Tidy Population Data for Testing
#'
#' This function creates a reproducible synthetic population dataset suitable
#' for testing population-processing pipelines. It expands all combinations of
#' year, region, sex, and pop_age, and assigns each row a random population
#' count.
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
#' @param seed Integer. Random seed used to generate population values.
#'   Defaults to `97`. Use different seeds to generate different datasets.
#'
#' @param yr_start Integer. The first year to include in the dataset.
#'   Data will cover four years: `yr_start`, `yr_start + 1`, `yr_start + 2`,
#'   and `yr_start + 3`.
#'
#' @return A data frame (tibble-like) with the following columns:
#'
#' * `year` — integer year
#' * `geog` — region name string (`"RegionA"`–`"RegionD"`)
#' * `sex` — `"Male"` or `"Female"`
#' * `pop_age` — pop_age in years (0–90)
#' * `population` — randomly generated integer between 0 and 500
#'
#' The number of rows is:
#'
#' ```
#' 4 years × 4 regions × 2 sexes × 91 ages = 2912 rows
#' ```
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


#' Create a Tidy Population Table with All People, All Ages, and Country Totals
#'
#' This function takes a population dataset and a configuration list, and
#' generates a fully aggregated tidy population table. It adds population totals
#' for:
#'
#' * **All people** — summed across all sex groups within each pop_age ×
#' geography
#' * **All ages**   — summed across all pop_age groups within each sex ×
#' geography
#' * **Country**    — summed across all geographies within each sex × pop_age
#'
#' The resulting dataset includes all original rows plus all aggregated totals,
#' with factors ordered according to the levels defined in `cfg`.
#'
#' @param data A data frame or tibble containing population data. Must include
#' columns: `sex`, `pop_age`, `geog`, and `population`.
#'
#' @param cfg A named list specifying the valid levels for each demographic
#' dimension. It must contain:
#'
#'   * `sex`  – character vector of sex categories (e.g., `c("Male","Female")`)
#'   * `pop_age`  – vector of pop_age groups (numeric or character)
#'   * `geog` – character vector of geographical units
#'
#'   These vectors are used to construct factor levels for the output table.
#'
#' @return A tibble with aggregated population counts at four levels:
#'
#'   * individual sex × pop_age × geography
#'   * all people × pop_age × geography
#'   * sex × all ages × geography
#'   * sex × pop_age × country
#'
#' Columns included in the output:
#'
#' * `geog`
#' * `sex`
#' * `pop_age`
#' * `population`
#'
#' All three categorical variables (`sex`, `pop_age`, `geog`) are returned as
#' ordered factors.
#'
#' @details
#' Internally, this function uses the helper `group_by_summarise_pop()`,
#' which performs grouped summation and assigns a fixed label to a specified
#' column using tidy evaluation.
#'
#' The function follows a clear aggregation workflow:
#'
#' 1. *All people* totals across sex
#' 2. Merge with original data
#' 3. *All ages* totals across pop_age
#' 4. Country totals across geographies
#' 5. Apply factor levels and return tidy, ordered output
#'
#' The function guarantees consistent ordering and complete population
#' coverage across all demographic dimensions.
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

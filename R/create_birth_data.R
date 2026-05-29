#' Simulate raw birth registration data
#'
#' Generates a synthetic (dummy) dataset of raw birth registration data,
#' including child, birth event, and maternal characteristics. The output
#' includes a small number of deliberately irregular records for testing
#' validation and cleaning workflows.
#'
#' @param n Number of records to generate. Default is 10,000.
#' @param seed Random seed for reproducibility. Default is 249.
#'
#' @returns A dataframe of synthetic births data. The column names are unique
#' identifiers, rather than descriptive names (e.g. `birth1a`).
#'
#' @details
#' Values are sampled from simple distributions to approximate realistic
#' patterns. Five additional rows are appended containing edge cases
#' (e.g. missing or inconsistent dates) to support testing.
#'
#' @examples
#' create_birth_data_raw()
#' create_birth_data_raw(n = 1000, seed = 123)
#'
#' @export
create_birth_data_raw <- function(n = 10000, seed = 249) {

  set.seed(seed)

  dob <- seq(as.Date("2024-01-01"), as.Date("2025-12-31"), by = 1)
  dob <- sample(dob, n, replace = TRUE)
  regions <- list(
    strings = c(paste0("Region", c("A", "B", "C", "D")), NA_character_),
    probs = c(0.4, 0.2, 0.2, 0.1, 0.1)
  )
  multi <- list(
    strings = c("Single", "Twin", "Triplet", "Quadruplet or higher"),
    probs = c(0.70, 0.15, 0.1, 0.05)
  )
  sex <- c("Female", "Male", "Not stated")
  attendant <- c("Midwife", "Nurse", "Doctor", "Other")
  pob_type <- c("Home", "Clinic", "Hospital", "Other")
  mother_dob <- seq(as.Date("1982-01-01"), as.Date("2009-12-31"), by = 1)
  mar_stat <- c("Single", "Married", "Divorced", "Widowed", "Not stated")


  df <- data.frame(
    birth1a = dob,
    birth1b = dob + sample(2:370, n, replace = TRUE),
    birth1c = sample(regions$strings, n, replace = TRUE, prob = regions$probs),
    birth1g = sample(multi$strings, n, replace = TRUE, prob = multi$probs),
    birth1h = sample(attendant, n, replace = TRUE),
    birth1i = sample(pob_type, n, replace = TRUE),
    birth1j = sample(c(1, NA), n, replace = TRUE, prob = c(0.3, 0.7)),
    birth2a = sample(sex, n, replace = TRUE, prob = c(0.45, 0.45, 0.1)),
    birth3a = sample(mother_dob, n, replace = TRUE),
    birth3c = sample(mar_stat, n, replace = TRUE),
    birth3l = sample(regions$strings, n, replace = TRUE),
    birth3n = sample(c("Urban", "Rural", "Not stated"), n, replace = TRUE)
  )

  extra_data <- data.frame(
    birth1a = c(rep(as.Date("2024-01-02"), 2), as.Date("2023-12-01"),
                as.Date("2026-02-02"), NA),
    birth1b = c(as.Date("2024-01-01"), rep(as.Date("2024-02-02"), 3),
                as.Date("2026-03-02")),
    birth1c = rep("RegionA", 5),
    birth1g = rep("Single", 5),
    birth1h = rep("Midwife", 5),
    birth1i = rep("Clinic", 5),
    birth1j = rep(1, 5),
    birth2a = rep("Female", 5),
    birth3a = c(as.Date("1992-09-24"), NA, rep(as.Date("1992-09-24"), 3)),
    birth3c = rep("Married", 5),
    birth3l = rep("RegionB", 5),
    birth3n = rep("Rural", 5)
  )

  df |>
    dplyr::bind_rows(extra_data) |>
    dplyr::mutate(
      unique_id = dplyr::row_number(),
      .before = dplyr::everything()
    )

}

#' Process dummy birth data - ADD INFO WHEN FINISHED.
#'
#' @param data Data frame of record-level birth data.
#'
#' @return Processed data frame.
#' @export
#'
process_dummy_birth_data <- function(data, col_factor) {

  data |>
    derive_age(age_m, dob_m, dob) |>
    derive_timeliness(dob, dor) |>
    group_age("age_m", "age_grp_m", 15, 40, 5, col_factor) |>
    make_na_not_stated("age_m") |>
    derive_year(dob, dobyr) |>
    tidyr::drop_na(dobyr)
}

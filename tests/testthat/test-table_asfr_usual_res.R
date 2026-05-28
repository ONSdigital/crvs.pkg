test_that("Processing pop for ASFR works on valid input", {

  pop <- data.frame(
    Year = factor(rep(c("2024", "2025"), each = 12)),
    Geography = factor(
      rep(c("Country", "RegionA", "RegionB"), each = 4, times = 2)
    ),
    Sex = factor(rep(c("Female", "Male"), each = 2, times = 6)),
    Age = factor(rep(c(16:27), times = 2)),
    Population = rep(1:24)
  )

  fcts <- list(
    asfr_mother_age_group = c("Under 20", "20 to 24", "25 and over")
  )

  expected <- tibble::tibble(
    Year = factor(rep(2024:2025, each = 9),
                  levels = c("2024", "2025")),
    Geography = factor(
      rep(c("Country", "RegionA", "RegionB"), each = 3, times = 2)
    ),
    `Mothers age group` = factor(
      rep(fcts$asfr_mother_age_group, times = 6),
      levels = fcts$asfr_mother_age_group
    ),
    Population = c(3, 0, 0, 0, 11, 0, 0, 9, 0, 27, 0, 0, 0, 35, 0, 0, 21, 0)
  )

  output <- process_pop_for_asfr(pop,
                                 fcts = fcts,
                                 pop_sex = "Female",
                                 fert_range = 15:24)

  expect_equal(expected, output)
})

test_that("Process pop for ASFR fails validation", {

  pop <- data.frame(
    Year = factor(rep(c("2024", "2025"), each = 12)),
    Geography = factor(
      rep(c("Country", "RegionA", "RegionB"), each = 4, times = 2)
    ),
    Sex = factor(rep(c("Female", "Male"), each = 2, times = 6)),
    Age = factor(rep(c(16:27), times = 2)),
    Population = rep(1:24)
  )

  fcts <- list(
    asfr_mother_age_group = c("Under 20", "20 to 24", "25 and over")
  )

  expect_error(process_pop_for_asfr("not_a_df", fcts))

  pop_wrong_col <- dplyr::rename(pop, wrong_colname = "Year")
  expect_error(process_pop_for_asfr(pop_wrong_col, fcts))

  expect_error(process_pop_for_asfr(pop, "not_a_list"))

  fcts_wrong <- list(
    wrong_name = c("Under 20", "20 to 24", "25 and over")
  )
  expect_error(process_pop_for_asfr(pop, fcts_wrong, fert_range = 15:24))

  expect_error(process_pop_for_asfr(pop, fcts, fert_range = "15 to 44"))

  expect_error(process_pop_for_asfr(pop, fcts, fert_range = 15:25))

  fcts_wrong_length <- list(
    asfr_mother_age_group = c("Under 20", "20 and over")
  )
  expect_error(process_pop_for_asfr(pop, fcts_wrong_length, fert_range = 15:24))

  pop_na <- data.frame(
    Year = factor(rep(c("2024", "2025"), each = 12)),
    Geography = factor(
      rep(c("Country", "RegionA", "RegionB"), each = 4, times = 2)
    ),
    Sex = factor(rep(c("Female", NA), each = 2, times = 6)),
    Age = factor(rep(c(16:27), times = 2)),
    Population = rep(1:24)
  )
  expect_error(process_pop_for_asfr(pop_na, fcts, fert_range = 15:24))
})

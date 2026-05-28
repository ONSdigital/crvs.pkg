test_that("Filtering for factor levels", {

  fcts <- list(
    Year = c("2025", "2024"),
    `Birth type` = c("Live birth", "Stillbirth"),
    `Urban or rural` = c("Urban", "Rural", "Not stated")
  )

  urban_lvl <- c("All place rural or urban", fcts$`Urban or rural`)
  data <- data.frame(
    Year = factor(rep(c(2024:2025), each = 4), c(fcts$Year)),
    `Urban or rural` = factor(rep(urban_lvl, 2), urban_lvl),
    `Birth type` = factor(rep(fcts$`Birth type`, 4), c(fcts$`Birth type`)),
    other = 1:8,
    check.names = FALSE
  )

  expected <- data.frame(
    Year = factor(rep(2024:2025, each = 3), c("2025", "2024")),
    `Urban or rural` = factor(rep(fcts$`Urban or rural`, 2), urban_lvl),
    `Birth type` = factor(
      c("Stillbirth", "Live birth", "Stillbirth", "Stillbirth", "Live birth",
        "Stillbirth"),
      fcts$`Birth type`
    ),
    other = c(2:4, 6:8),
    check.names = FALSE
  )

  actual <- filter_factor_levels(data, levels_to_keep = fcts)
  expect_equal(expected, actual)
})

test_that("Filtering factor levels - error for wrong data object", {

  fcts <- list(Year = c("2025", "2024"))
  data <- list(Year = factor(c(2022:2025), c(fcts$Year)), other = 1:4)

  expect_error(
    filter_factor_levels(data, levels_to_keep = fcts),
    "is.data.frame\\(data\\) is not TRUE"
  )
})

test_that("Filt factor levels - error if mismatch col in levels and data", {

  fcts <- list(Year = c("2025", "2024"), Birth = c("a", "b"))
  data <- data.frame(
    Year = factor(fcts$Year, fcts$Year),
    Not_birth = factor(fcts$Birth, fcts$Birth),
    other = 1:2
  )

  expect_error(
    filter_factor_levels(data, levels_to_keep = fcts),
    "Missing column: Birth"
  )
})

test_that("Filtering factor levels - error for wrong col type", {

  fcts <- list(Year = c("2025", "2024"), Birth = c("a", "b"))
  data <- data.frame(
    Year = 2024:2025,
    Birth = factor(fcts$Birth, fcts$Birth),
    other = 1:2
  )

  expect_error(
    filter_factor_levels(data, levels_to_keep = fcts),
    "Column is not factor or character: Year"
  )
})

test_that("Filtering skipped for factor levels with NULL filter", {

  fcts <- list(Year = NULL)
  data <- data.frame(Year = factor(2:3, c("2", "3")), other = 1:2)

  actual <- filter_factor_levels(data, levels_to_keep = fcts)

  expect_equal(data, actual)

})

test_that("Filtering for factor levels - NA in factor col not kept", {

  fcts <- list(Year = c("2025", "2024"), Birth = c("a", "b"))
  data <- data.frame(
    Year = factor(c(NA, fcts$Year, NA), fcts$Year),
    Birth = factor(c("a", "a", "b", "b"), fcts$Birth),
    other = 1:4
  )
  expected <- data.frame(
    Year = factor(fcts$Year, fcts$Year),
    Birth = factor(fcts$Birth, fcts$Birth),
    other = c(2, 3)
  )

  actual <- filter_factor_levels(data, levels_to_keep = fcts)

  expect_equal(expected, actual)
})

test_that("General count by var", {

  fcts <- list(
    dobyr = c("2025", "2024"),
    live_or_still = c("Live birth", "Stillbirth"),
    age_grp_m = c("Under 15", "20 to 24", "40 and over", "Not stated"),
    urban_rural = c("Urban", "Rural", "Not stated"),
    mar_stat = c("Single", "Married", "Not stated")
  )

  data <- data.frame(
    dobyr = c(rep(c(rep(2025, 4), 2024), 7), 2024),
    urban_rural = rep(fcts$urban_rural, each = 12),
    live_or_still = rep(c(rep("Live birth", 11), "Stillbirth"), 3),
    age_grp_m = rep(fcts$age_grp_m, 9),
    mar_stat = rep(rep(fcts$mar_stat, each = 3), 4),
    other = 1:36
  )

  # Length dictated by the number of factor levels in the fcts object
  expected <- data.frame(
    Year = factor(
      rep(c(2025, 2024), each = 80),
      levels = c("2025", "2024")
    ),
    `Urban or rural` = factor(
      rep(rep(c("All places rural or urban", fcts$urban_rural), each = 20), 2),
      levels = c("All places rural or urban", fcts$urban_rural)
    ),
    `Mothers age` = factor(
      rep(rep(c("All ages", fcts$age_grp_m), each = 4), 8),
      levels = c("All ages", fcts$age_grp_m)
    ),
    `Marital status` = factor(
      rep(c("All marital statuses", fcts$mar_stat), 40),
      levels = c("All marital statuses", fcts$mar_stat)
    ),
    `Counts of births` = c(
      26, 8, 9, 9, 7, 3, 2, 2, 7, 1, 3, 3, 7, 3, 2, 2, 5, 1, 2, 2, 9, 4, 2, 3,
      2, 1, 0, 1, 2, 1, 1, 0, 3, 2, 0, 1, 2, 0, 1, 1, 9, 2, 4, 3, 3, 1, 1, 1, 3,
      0, 2, 1, 2, 1, 1, 0, 1, 0, 0, 1, 8, 2, 3, 3, 2, 1, 1, 0, 2, 0, 0, 2, 2, 0,
      1, 1, 2, 1, 1, 0, 7, 3, 2, 2, 2, 0, 1, 1, 2, 2, 0, 0, 2, 0, 1, 1, 1, 1, 0,
      0, 2, 1, 1, 0, 1, 0, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 1, 1, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 1, 1, 0, 0, 3, 1, 0, 2, 1, 0, 0, 1, 1,
      1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0
    ),
    check.names = FALSE
  )

  actual <- general_count_by_var(
    data = data,
    agg_cols = c("dobyr", "urban_rural", "age_grp_m", "mar_stat"),
    agg_cols_pub_names = c("Year", "Urban or rural", "Mothers age",
                           "Marital status"),
    agg_count_colname = c("Counts of births"),
    fcts = fcts,
    pre_agg_levels_to_keep = list(live_or_still = "Live birth"),
    post_agg_levels_to_keep = list(
      `Urban or rural` = c("All places rural or urban", "Urban", "Rural",
                           "Not stated"),
      Year = c("2024", "2025")
    )
  )

  expect_equal(actual, expected)
})

test_that("General - missing cols", {

  fcts <- list(
    dobyr = c("2020", "2021"),
    live_or_still = c("Live birth", "Stillbirth"),
    age_grp_m = c("Under 15", "20 to 24", "40 and over", "Not stated"),
    mar_stat = c("Single", "Married", "Not stated")
  )

  data <- data.frame(
    dobyr = c(2020, 2020, 2021, 2021),
    live_or_still = c("Live birth", "Live birth", "Still birth", "Live birth"),
    age_grp_m = fcts$age_grp_m
  )

  expect_error(
    general_count_by_var(
      data = data,
      agg_cols = c("dobyr", "age_grp_m", "mar_stat"),
      agg_cols_pub_names = c("Year", "Mothers age", "Marital status"),
      fcts = fcts,
      pre_agg_levels_to_keep = list(live_or_still = "Live birth")
    ),
    "Missing required columns: mar_stat."
  )
})

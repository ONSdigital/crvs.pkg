test_that("Age derived correctly", {

  df <- data.frame(
    date1 = as.Date(
      c("1992-09-24", "1960-12-28", "2023-10-31", "2024-01-10", NA_character_)),
    date2 = as.Date(
      c("2025-11-04", "2025-03-31", "2025-06-17", NA_character_, "2024-01-10")),
    other_col = 1:5
  )

  expected <- data.frame(
    date1 = as.Date(
      c("1992-09-24", "1960-12-28", "2023-10-31", "2024-01-10", NA_character_)),
    date2 = as.Date(
      c("2025-11-04", "2025-03-31", "2025-06-17", NA_character_, "2024-01-10")),
    other_col = 1:5,
    age_column = c(33, 64, 1, NA, NA)
  )

  expect_equal(derive_age(df, age_column, date1, date2), expected)
})

test_that("Age derivation causes error for wrong column type", {

  df <- data.frame(
    date1 =
      c("1992-09-24", "1960-12-28", "2023-10-31", "2024-01-10", NA_character_),
    date2 = as.Date(
      c("2025-11-04", "2025-03-31", "2025-06-17", NA_character_, "2024-01-10")),
    other_col = 1:5
  )

  expect_error(derive_age(df, age_column, date1, date2),
               "Both `dob_col` and `end_date_col` must be Date type.")

})

test_that("Timeliness derived correctly with default thresholds", {

  df <- data.frame(
    birth1a = as.Date(
      c("2023-01-01", "2022-01-01", "2020-01-01", "2023-06-01", "2023-06-01")),
    birth1b = as.Date(
      c("2023-01-15", "2022-02-15", "2021-06-01", "2024-06-01", "2022-06-01")),
    other_col = 1:5
  )

  expected <- data.frame(
    birth1a = as.Date(
      c("2023-01-01", "2022-01-01", "2020-01-01", "2023-06-01", "2023-06-01")),
    birth1b = as.Date(
      c("2023-01-15", "2022-02-15", "2021-06-01", "2024-06-01", "2022-06-01")),
    other_col = 1:5,
    diff_reg_occ = c(14, 45, 517, 366, -365),
    timeliness = c("current", "late", "delayed", "delayed", NA_character_)
  )

  expect_equal(derive_timeliness(df, birth1a, birth1b), expected)

})

test_that("Timeliness derived correctly with reg on threshold boundary", {

  df <- data.frame(
    birth1a = as.Date(rep("2023-01-01", 7)),
    birth1b = as.Date(
      c("2023-01-01", "2023-01-30", "2023-01-31", "2023-02-01", "2023-12-31",
        "2024-01-01", "2024-01-02")),
    other_col = 1:7
  )

  expected <- data.frame(
    birth1a = as.Date(rep("2023-01-01", 7)),
    birth1b = as.Date(
      c("2023-01-01", "2023-01-30", "2023-01-31", "2023-02-01", "2023-12-31",
        "2024-01-01", "2024-01-02")),
    other_col = 1:7,
    diff_reg_occ = c(0, 29, 30, 31, 364, 365, 366),
    timeliness = c("current", "current", "late", "late", "late", "delayed",
                   "delayed")
  )

  expect_equal(derive_timeliness(df, birth1a, birth1b), expected)

})

test_that("Timeliness derived correctly with non-default thresholds", {

  df <- data.frame(
    birth1a = as.Date(
      c("2023-01-01", "2022-01-01", "2020-01-01", "2023-06-01")),
    birth1b = as.Date(
      c("2023-01-15", "2022-02-15", "2021-06-01", "2024-06-01")),
    other_col = 1:4
  )

  expected <- data.frame(
    birth1a = as.Date(
      c("2023-01-01", "2022-01-01", "2020-01-01", "2023-06-01")),
    birth1b = as.Date(
      c("2023-01-15", "2022-02-15", "2021-06-01", "2024-06-01")),
    other_col = 1:4,
    diff_reg_occ = c(14, 45, 517, 366),
    timeliness = c("late", "late", "delayed", "late")
  )

  expect_equal(derive_timeliness(df, birth1a, birth1b, 10, 367), expected)

})

test_that("Year column derived correctly with default year_col", {

  df <- data.frame(
    birth1a = as.Date(
      c("2023-01-01", "2022-01-01", "2020-01-01", "2023-06-01")),
    birth1b = as.Date(
      c("2023-01-15", "2022-02-15", "2021-06-01", "2024-06-01")),
    other_col = 1:4
  )

  expected <- data.frame(
    birth1a = as.Date(
      c("2023-01-01", "2022-01-01", "2020-01-01", "2023-06-01")),
    birth1b = as.Date(
      c("2023-01-15", "2022-02-15", "2021-06-01", "2024-06-01")),
    other_col = 1:4,
    year = as.integer(c(2023, 2022, 2020, 2023))
  )

  expect_equal(derive_year(df, birth1a), expected)
})

test_that("Year column derivation errors when not date type", {

  df <- data.frame(
    birth1a = c("2023-01-01", "2022-01-01", "2020-01-01", "2023-06-01"),
    other_col = 1:4
  )

  expect_error(derive_year(df, birth1a))
})

test_that("Year column derived correctly with non-default year_col", {

  df <- data.frame(
    birth1a = as.Date(
      c("2023-01-01", "2022-01-01", "2020-01-01", "2023-06-01")),
    birth1b = as.Date(
      c("2023-01-15", "2022-02-15", "2021-06-01", "2024-06-01")),
    other_col = 1:4
  )

  expected <- data.frame(
    birth1a = as.Date(
      c("2023-01-01", "2022-01-01", "2020-01-01", "2023-06-01")),
    birth1b = as.Date(
      c("2023-01-15", "2022-02-15", "2021-06-01", "2024-06-01")),
    other_col = 1:4,
    year1 = as.integer(c(2023, 2022, 2021, 2024))
  )

  expect_equal(derive_year(df, birth1b, year1), expected)

})

test_that("Group age", {

  data <- data.frame(
    age = c(15, 20, 66, NA, 50, 36, 32, 202),
    other_col = 1:8
  )

  age_group_factors <- c(
    "Under 20",
    "20 to 24",
    "25 to 29",
    "30 to 34",
    "35 to 39",
    "40 to 44",
    "45 to 49",
    "50 to 54",
    "55 to 59",
    "60 to 64",
    "65 and over",
    "Not stated"
  )

  expected <- data.frame(
    age = c(15, 20, 66, NA, 50, 36, 32, 202),
    other_col = 1:8,
    age_grp = factor(
      c("Under 20", "20 to 24", "65 and over", "Not stated", "50 to 54",
        "35 to 39", "30 to 34", "Not stated"),
      age_group_factors
    )
  )

  output <- group_age(
    data,
    age_col = "age",
    new_col = "age_grp",
    start_brk = 20,
    end_brk = 65,
    brk_step = 5,
    col_factor = age_group_factors
  )
  expect_equal(output, expected)
})

test_that("Group age - error for not numeric age col", {

  data <- data.frame(
    age = as.character(c(15, 20, 66, NA, 202)),
    other = 1:5
  )
  age_group_factors <- c("Under 20", "Over 20", "Not stated")

  expect_error(
    group_age(
      data,
      age_col = "age",
      new_col = "age_grp",
      start_brk = 20,
      end_brk = 20,
      brk_step = 5,
      col_factor = age_group_factors
    ),
    "`age_col` must be numeric."
  )
})

test_that("Group age - mismatch between factor levels and outputs", {

  data <- data.frame(
    age = c(15, 20, 66, NA, 50, 36, 32, 202),
    other_col = 1:8
  )

  age_group_factors <- c("Under 20", "20 to 39", "40 and over")

  expect_error(
    group_age(
      data,
      age_col = "age",
      new_col = "age_grp",
      start_brk = 20,
      end_brk = 40,
      brk_step = 20,
      col_factor = age_group_factors
    )
  )
})

test_that("Group age - more breaks than factor levels", {

  data <- data.frame(
    age = c(15, 20, 66, NA, 50, 36, 32, 202),
    other_col = 1:8
  )

  age_group_factors <- c("Under 20", "20 to 39", "40 and over", "Not stated")

  expect_error(
    group_age(
      data,
      age_col = "age",
      new_col = "age_grp",
      start_brk = 20,
      end_brk = 40,
      brk_step = 5,
      col_factor = age_group_factors
    )
  )
})

test_that("CRVS columns are renamed correctly (default names)", {
  df <- data.frame(a = 1:3, b = 4:6)

  lookup <- data.frame(
    input_variable_id = c("a", "b"),
    standardised_variable_name = c("alpha", "beta")
  )

  expected <- data.frame(alpha = 1:3, beta = 4:6)

  expect_equal(rename_crvs_columns(df, lookup), expected)
})

test_that("Column renaming warning is issued for extra cols (default names)", {
  df <- data.frame(a = 1:3, b = 4:6, c = 7:9, d = 10:12)

  lookup <- data.frame(
    input_variable_id = c("a", "b"),
    standardised_variable_name = c("alpha", "beta")
  )

  expect_warning(
    rename_crvs_columns(df, lookup),
    "Data frame contains columns not in the lookup table: c, d"
  )
})

test_that("Column renaming empty lookup returns original names (default)", {
  df <- data.frame(a = 1:3)

  lookup <- data.frame(
    input_variable_id = character(),
    standardised_variable_name = character()
  )

  result <- suppressWarnings(rename_crvs_columns(df, lookup))
  expect_equal(names(result), "a")

  expect_warning(
    rename_crvs_columns(df, lookup),
    "Data frame contains columns not in the lookup table: a"
  )
})

test_that("Column renaming empty dataframe returns empty dataframe (default)", {
  df <- data.frame()

  lookup <- data.frame(
    variable_id = c("a"),
    variable_name = c("alpha")
  )

  expect_error(
    rename_crvs_columns(df, lookup),
    "The data frame to rename is empty."
  )
})

test_that("Columns are renamed correctly (different colnames)", {
  df <- data.frame(a = 1:3, b = 4:6)

  lookup <- data.frame(
    old_colnames = c("a", "b"),
    new_colnames = c("alpha", "beta")
  )

  expected <- data.frame(alpha = 1:3, beta = 4:6)

  actual <- rename_crvs_columns(
    df,
    lookup,
    old_names = old_colnames,
    new_names = new_colnames
  )

  expect_equal(actual, expected)
})

test_that("Columns are renamed correctly (different colnames)", {
  df <- data.frame(a = 1:3, b = 4:6)

  lookup <- data.frame(
    old_cols = c("a", "b"),
    new_cols = c("alpha", "beta")
  )

  expected <- data.frame(alpha = 1:3, beta = 4:6)

  expect_error(
    rename_crvs_columns(df, lookup, old_names = not_name, new_names = new_cols),
    "object 'not_name' not found"
  )
})

test_that("Convert NA to Not stated where appropriate, character column", {
  df <- data.frame(
    col_to_change = c("not NA", "also not NA", NA),
    other_col = c(1:3)
  )

  expected <- data.frame(
    col_to_change = c("not NA", "also not NA", "Not stated"),
    other_col = c(1:3)
  )

  expect_equal(make_na_not_stated(df, "col_to_change"), expected)

})

test_that("Convert NA to Not stated where appropriate, numeric column", {
  df <- data.frame(
    col_to_change = c(1, 2, NA),
    other_col = c(1:3)
  )

  expected <- data.frame(
    col_to_change = c("1", "2", "Not stated"),
    other_col = c(1:3)
  )

  expect_equal(make_na_not_stated(df, "col_to_change"), expected)

})

test_that("Derive mar_stat", {
  df <- data.frame(
    mar_stat = c("Divorced", "Married", "Not stated", "Single", "Widowed", NA,
                 "Wrong"),
    other_col = c(1:7)
  )

  fcts <- list(
    mar_stat = c("Divorced", "Married", "Single", "Widowed", "Not stated")
  )

  expected <- data.frame(
    mar_stat = factor(
      c("Divorced", "Married", "Not stated", "Single", "Widowed", "Not stated",
        NA),
      levels = c("Divorced", "Married", "Single", "Widowed", "Not stated")
    ),
    other_col = c(1:7)
  )

  expect_equal(derive_var(df, "mar_stat", fcts$mar_stat), expected)

})

test_that("Derive general variable - numeric col with levels and NA", {
  df <- data.frame(
    age = c(1:5, NA, 10),
    other_col = c(1:7)
  )

  fcts <- list(age = c(1:6))

  expected <- data.frame(
    age = factor(
      c("1", "2", "3", "4", "5", "Not stated", NA),
      levels = c("1", "2", "3", "4", "5", "6")
    ),
    other_col = c(1:7)
  )

  expect_equal(derive_var(df, "age", fcts$age), expected)

})

test_that("round function works correctly", {

  expect_equal(round(1.14, 1), 1.1)  # Test round down
  expect_equal(round(1.15, 1), 1.2) # Test round up
  expect_equal(round(1.15, 0), 1.0)  # Test with zero
  expect_equal(round(-1.4, 0), -1)  # Test round down with negative number
  expect_equal(round(-1.5, 0), -2) # Test round up with negative number
  expect_error(round("a", 1))  # Test with a non-numeric input
})

test_that("Group by and summarise dataframe", {
  abc <- c("a", "b", "c")

  data <- data.frame(
    col_1 = c(rep("Female", 14), rep("Male", 7)),
    col_2 = factor(c(rep(c("a", "b"), 10), "c"), abc),
    col_3 = 1:21
  )

  expected <- tibble::tibble(
    col_1 = c(rep("Female", 3), rep("Male", 3)),
    col_2 = factor(rep(abc, 2), abc),
    count = c(7, 7, 0, 3, 3, 1)
  )

  expect_equal(group_by_summarise_count(data, c("col_1", "col_2")), expected)

})

test_that("Group by summarise sum is summed correctly by grouping columns", {

  data <- data.frame(
    region = c("A", "A", "B"),
    sex    = c("M", "F", "M"),
    population = c(10, 20, 30)
  )

  expected <- tibble::tibble(
    region = c("A", "B"),
    population = c(30, 30)
  )

  output <- group_by_summarise_sum(
    data,
    grouping_cols = "region",
    sum_col = "population"
  )

  expect_equal(expected, output)
})

test_that("Group by sum population is summed correctly by grouping columns", {

  data <- data.frame(
    region = c("A", "A", "B"),
    sex    = c("M", "F", "M"),
    population = c(10, 20, 30)
  )

  expected <- tibble::tibble(
    region = c("A", "B"),
    population = c(30, 30)
  )

  output <- group_by_summarise_sum(
    data,
    grouping_cols = "region",
    sum_col = "population"
  )

  expect_equal(expected, output)
})

test_that("Group sum handles multiple grouping columns", {

  data <- data.frame(
    geog = c("A", "A", "B", "B", "A", "B"),
    sex  = c("M", "F", "M", "F", "F", "M"),
    population = c(10, 15, 20, 25, 1, 1),
    other = 1:6
  )

  expected <- tibble::tibble(
    geog = rep(c("A", "B"), each = 2),
    sex = rep(c("F", "M"), 2),
    population = c(16, 10, 25, 21)
  )

  output <- group_by_summarise_sum(
    data,
    grouping_cols = c("geog", "sex"),
    sum_col = "population"
  )

  expect_equal(expected, output)
})

test_that("Group sum by empty factor levels preserved due to .drop = FALSE", {

  data <- data.frame(
    geog = factor(c("A"), levels = c("A", "B")),
    population = 10
  )

  expected <- tibble::tibble(
    geog = factor(c("A", "B"), levels = c("A", "B")),
    population = c(10, 0)
  )

  output <- group_by_summarise_sum(
    data,
    grouping_cols = "geog",
    sum_col = "population"
  )

  expect_equal(expected, output)
})

test_that("Group sum zero-row input is handled cleanly", {

  data <- data.frame(
    group = character(),
    population = numeric()
  )

  expected <- tibble::tibble(
    group = character(),
    population = numeric()
  )

  output <- group_by_summarise_sum(
    data,
    grouping_cols = "group",
    sum_col = "population"
  )

  expect_equal(expected, output)
})

test_that("Group by sum pop error is when grouping columns do not exist", {

  data <- data.frame(
    x = 1:3,
    population = 1:3
  )

  expect_error(
    group_by_summarise_pop(
      data,
      grouping_cols = "y",
      total_var = type,
      total_str = "X",
      sum_col = population
    )
  )
})


test_that("Factorise: checks if level_list not named list", {

  df <- data.frame(group = "hello", population = 1)

  lvl <- "not a list"

  expect_error(
    factorise_cols(df, level_list = lvl, cols = names(lvl), strict = TRUE),
    paste("`level_list` must be a named list where each name matches a",
          "col in `df` and each value is a character vector of levels.")
  )

  n_lvl <- 10

  expect_error(
    factorise_cols(df, level_list = n_lvl, cols = names(n_lvl), strict = TRUE),
    paste("`level_list` must be a named list where each name matches a",
          "col in `df` and each value is a character vector of levels.")
  )

  lvl_list <- list(group = "hello")
  expect_no_error(
    factorise_cols(
      df,
      level_list = lvl_list,
      cols = names(lvl_list),
      strict = TRUE
    )
  )
})

test_that("Factorise: throws error when no colnames provided", {

  df <- data.frame(sex = character(), population = numeric())
  lvl_list <- list(sex = c("Female", "Male"))

  expect_error(
    factorise_cols(df, level_list = lvl_list, cols = NULL, strict = TRUE),
    paste("`cols` cannot be NULL. Provide column names or ensure",
          "`level_list` has names.")
  )

})

test_that("Factorise: works with named list when no colnames arg provided", {

  df <- data.frame(sex = character(), population = numeric())
  lvl_list <- list(sex = c("Female", "Male"))

  expect_no_error(factorise_cols(df, level_list = lvl_list, strict = TRUE))
})

test_that("Factorise: checks if cols are present in data", {

  df <- data.frame(sex = character(), population = numeric())
  lvl_list <- list(sex = c("Female", "Male"))

  expect_message(
    factorise_cols(
      df,
      level_list = lvl_list,
      cols = c("sex", "a", "b"),
      strict = TRUE
    ),
    paste("These columns from fct object are not in `df` so ignored: a, b")
  )

  expect_no_error(
    factorise_cols(df, level_list = lvl_list, cols = "sex", strict = TRUE)
  )
})

test_that("Factorise: checks if cols are present in level list", {

  df <- data.frame(sex = character(), population = numeric())
  lvl_list <- list(geog = c("RegionA", "RegionB"))

  expect_error(
    factorise_cols(df, level_list = lvl_list, cols = c("sex"), strict = TRUE),
    paste("`level_list` is missing level definitions for: sex")
  )

  lvl_list <- list(sex = c("Female", "Male"))

  expect_no_error(
    factorise_cols(df, level_list = lvl_list, cols = c("sex"), strict = TRUE)
  )
})

test_that("Factorise: checks if all levels in level list are characters", {

  df <- data.frame(
    sex = c("Female", "Male", "Male", "Female"),
    geog = c(1, 2, 3, 3),
    population = c(100, 200, 300, 400)
  )

  lvl_list <- list(sex = c("Female", "Male"), geog = c(1, 2, 3))

  expect_error(
    factorise_cols(
      df,
      level_list = lvl_list,
      cols = c("sex", "geog"),
      strict = TRUE
    ),
    paste("Levels for column 'geog' must be a character vector.")
  )

  lvl_list <- list(sex = c("Female", "Male"), geog = c("1", "2", "3"))

  expect_no_error(
    factorise_cols(
      df,
      level_list = lvl_list,
      cols = c("sex", "geog"),
      strict = TRUE
    )
  )
})

test_that("Factorise: checks if all levels are characters in level list", {

  df <- data.frame(
    sex = c("Female", "Male", "Male", "Female"),
    geog = c(1, 2, 3, 3),
    population = c(100, 200, 300, 400)
  )

  lvl_list <- list(sex = c("Female", "Male"), geog = c("1", "2"))

  expect_error(
    factorise_cols(
      df,
      level_list = lvl_list,
      cols = c("sex", "geog"),
      strict = TRUE
    ),
    paste("Column 'geog' contains values not in provided levels: 3.",
          "Set `strict = FALSE` to coerce unknowns to NA.")
  )

  expect_warning(
    factorise_cols(
      df,
      level_list = lvl_list,
      cols = c("sex", "geog"),
      strict = FALSE
    ),
    paste0("Column 'geog' contains values not in provided levels: 3; ",
           "these will be set to NA.")
  )
})

test_that("Summarise, count variables, all combos, default count_or_sum", {

  lvls <- list(
    agem = c("Under 30", "30 and over", "Not stated", "Sum"),
    agef = c("Under 40", "40 and over", "Not stated", "Sum"),
    mar = c("Within", "Outside", "Sum")
  )

  data <- data.frame(
    agebm_grp = factor(c(rep("Under 30", 5), "Not stated"), lvls$agem[-4]),
    agebf_grp = factor(
      c(rep("Under 40", 3), rep("40 and over", 3)),
      lvls$agef[-4]
    ),
    marriage_reg = factor(c(rep("Within", 6)), lvls$mar[-3]),
    other = 1:6
  )

  expected <- expand.grid(lvls$agem, lvls$agef, lvls$mar)
  names(expected) <- c("agebm_grp", "agebf_grp", "marriage_reg")
  attributes(expected)$out.attrs <- NULL
  expected$Count <- c(
    3, 0, 0, 3, 2, 0, 1, 3, 0, 0, 0, 0, 5, 0, 1, 6, rep(0, 16), 3, 0, 0, 3, 2,
    0, 1, 3, 0, 0, 0, 0, 5, 0, 1, 6
  )

  actual <- summarise_count_vars(
    data,
    "Count",
    c("agebm_grp", "agebf_grp", "marriage_reg")
  )
  expect_equal(actual, expected)
})

test_that("Summarise, count variables works with default count_or_sum", {

  lvls <- list(
    agem = c("a", "b"),
    agef = c("c", "d")
  )

  data <- data.frame(
    agebm_grp = factor(c(rep("a", 5), "b"), lvls$agem),
    agebf_grp = factor(c("c", rep("d", 5)), lvls$agef),
    other = 1:6
  )

  expected <- data.frame(
    agebm_grp = factor(
      c("a", "b", "Sum", "a", "b", "Sum", "a", "b", "Sum"),
      c(lvls$agem, "Sum")
    ),
    agebf_grp = factor(
      c("c", "c", "c", "d", "d", "d", "Sum", "Sum", "Sum"),
      c(lvls$agef, "Sum")
    ),
    Count = c(1, 0, 1, 4, 1, 5, 5, 1, 6)
  )

  actual <- summarise_count_vars(data, "Count", c("agebm_grp", "agebf_grp"))
  expect_equal(actual, expected)
})

test_that("Summarise, count vars, all combos, count_or_sum count,", {

  lvls <- list(
    agem = c("Under 30", "30 and over", "Not stated", "Sum"),
    agef = c("Under 40", "40 and over", "Not stated", "Sum"),
    mar = c("Within", "Outside", "Sum")
  )

  data <- data.frame(
    agebm_grp = factor(c(rep("Under 30", 5), "Not stated"), lvls$agem[-4]),
    agebf_grp = factor(
      c(rep("Under 40", 3), rep("40 and over", 3)), lvls$agef[-4]
    ),
    marriage_reg = factor(c(rep("Within", 6)), lvls$mar[-3]),
    pop = c(1:6),
    other = 1:6
  )

  expected <- expand.grid(lvls$agem, lvls$agef, lvls$mar)
  names(expected) <- c("agebm_grp", "agebf_grp", "marriage_reg")

  expected$Count <- c(
    3, 0, 0, 3, 2, 0, 1, 3, 0, 0, 0, 0, 5, 0, 1, 6, rep(0, 16), 3, 0, 0, 3, 2,
    0, 1, 3, 0, 0, 0, 0, 5, 0, 1, 6
  )

  attributes(expected)$out.attrs <- NULL

  actual <- summarise_count_vars(
    data,
    pub_name_col = "Count",
    grouping_cols = c("agebm_grp", "agebf_grp", "marriage_reg"),
    count_or_sum = "count",
    sum_col = "pop"
  )

  expect_equal(actual, expected)
})

test_that("Warning for summarise and total due to NA in input dataframe", {

  lvls <- list(
    agem = c("a", "b"),
    agef = c("c", "d")
  )

  data <- data.frame(
    agebm_grp = factor(c(lvls$agem, NA), lvls$agem),
    agebf_grp = factor(c(NA, lvls$agef), lvls$agef),
    other = 1:3
  )

  expect_warning(
    summarise_count_vars(data, "Count", c("agebm_grp", "agebf_grp")),
    paste("There are NA in the grouping columns, check data has been",
          "processed correctly.")
  )
})


test_that("Renaming 'Sum' and refactoring in multiple columns", {

  fcts <- list(
    age_grp_m = c("Under 15", "Not stated"),
    urban_rural = c("Urban", "Rural")
  )

  data <- data.frame(
    my_age_col = c("Sum", "Under 15", "Not stated"),
    urban_rural = c("Sum", "Urban", "Rural"),
    other = c("Sum", "Sum", "x")
  )

  expected <- data.frame(
    `Age of Mother at birth` = factor(
      c("All ages", "Under 15", "Not stated"),
      c("All ages", fcts$age_grp_m)
    ),
    `Urban or Rural` = factor(
      c("All places rural or urban", "Urban", "Rural"),
      c("All places rural or urban", "Urban", "Rural")
    ),
    other = c("Sum", "Sum", "x"),
    check.names = FALSE
  )

  actual <- rename_multi_sum_str(
    data,
    c("my_age_col", "urban_rural"),
    c("age_grp_m", "urban_rural"),
    c("Age of Mother at birth", "Urban or Rural"),
    fcts
  )

  expect_equal(actual, expected)
})

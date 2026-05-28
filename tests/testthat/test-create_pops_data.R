test_that("create_tidy_pop_data returns a dataframe with correct columns", {

  df <- create_tidy_pop_data()

  expected_cols <- c("year", "geog", "sex", "pop_age", "population")

  expect_true(all(expected_cols %in% names(df)))
  expect_s3_class(df, "data.frame")
})


test_that("Creating pop data seeding works", {

  df1 <- create_tidy_pop_data(seed = 50)
  df2 <- create_tidy_pop_data(seed = 50)
  df3 <- create_tidy_pop_data(seed = 25)

  expect_identical(df1, df2)
  expect_false(identical(df1$population, df3$population))
})


test_that("Create tidy pop correct values and num rows (age/sex/geog/year)", {

  df <- create_tidy_pop_data(yr_start = 2030)

  expect_equal(unique(df$year), 2030:2033)
  expect_true(all(df$population >= 0 & df$population <= 500))
  expect_equal(sort(unique(df$pop_age)), 0:90)
  expect_equal(sort(unique(df$geog)), paste0("Region", c("A", "B", "C", "D")))
  expect_equal(sort(unique(df$sex)), c("Female", "Male"))

  # 4 years × 4 geogs × 2 sexes × 91 ages
  expected_n <- 4 * 4 * 2 * 91
  expect_equal(nrow(df), expected_n)

})


test_that("Create tidy pop has correct total by group", {

  cfg <- list(
    year = c("2022"),
    sex = c("Female", "Male"),
    geog = c("RegionA", "RegionB"),
    pop_age = as.character(c(0:5))
  )

  df <- data.frame(
    year = rep(2022),
    geog = factor(rep(rep(c("RegionA", "RegionB"), 6), 2),
                  levels = c("RegionA", "RegionB")),
    sex = factor(rep(rep(c("Female", "Male"), each = 2), 6),
                 levels  = c("Female", "Male")),
    pop_age = factor(rep(c(0:5), each = 4), levels = 0:5),
    population = 1:24,
    other = rep(1)
  )

  expected <- data.frame(
    Year = factor(
      rep("2022"),
      levels = "2022"
    ),
    Geography = factor(
      rep(c("Country", "RegionA", "RegionB"), each = 21),
      levels = c("Country", "RegionA", "RegionB")
    ),
    Sex = factor(
      rep(rep(c("All sexes", "Female", "Male"), each = 7), 3),
      levels = c("All sexes", "Female", "Male")
    ),
    Age = factor(
      rep(c("All ages", 0:5), 9),
      levels = c("All ages", 0:5)
    ),
    Population = c(
      300, 10, 26, 42, 58, 74, 90, 138, 3, 11, 19, 27, 35, 43, 162, 7, 15, 23,
      31, 39, 47, 144, 4, 12, 20, 28, 36, 44, 66, 1, 5, 9, 13, 17, 21, 78, 3,
      7, 11, 15, 19, 23, 156, 6, 14, 22, 30, 38, 46, 72, 2, 6, 10, 14, 18, 22,
      84, 4, 8, 12, 16, 20, 24
    ),
    check.names = FALSE
  )

  actual <- add_totals_tidy_pop(df, cfg)

  expect_equal(actual, expected)

})

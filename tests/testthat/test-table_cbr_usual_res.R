test_that("Calc cbr: default by_pop and digits", {

  fcts <- list(
    dobyr = c("2025", "2024"),
    live_or_still = c("Live birth", "Stillbirth"),
    usual_residence = c("RegionA", "RegionB", "Not stated")
  )

  data <- data.frame(
    live_or_still = c(rep("Live birth", 19), "Stillbirth"),
    dobyr = factor(c(2024, 2025), fcts$dobyr),
    usual_residence = c(rep("RegionA", 5), rep("RegionB", 15)),
    extra = 1:20
  )

  pop <- data.frame(
    Year = factor(rep(c(2024, 2025), each = 3), fcts$dobyr),
    Geography = rep(c("Country", "RegionA", "RegionB"), 2),
    Age = "All ages",
    Sex = "All sexes",
    Population = c(1234, 900, 334, 333, 111, 222),
    other = rep(1, 6)
  )

  expected <- data.frame(
    Year = factor(rep(c(2025, 2024), each = 3), levels = c("2025", "2024")),
    `Place of usual residence` = rep(c("Country", "RegionA", "RegionB"), 2),
    `Counts of live births` = c(9, 2, 7, 10, 3, 7),
    Population = c(333, 111, 222, 1234, 900, 334),
    `Crude birth rate` = c(27.03, 18.02, 31.53, 8.10, 3.33, 20.96),
    check.names = FALSE
  )

  output <- calc_cbr_usual_residence(data, pop, fcts)

  expect_equal(expected, output)
})

test_that("Calc cbr: other by_pop and digits", {

  fcts <- list(
    dobyr = c("2025", "2024"),
    live_or_still = c("Live birth", "Stillbirth"),
    usual_residence = c("RegionA", "RegionB", "Not stated")
  )

  data <- data.frame(
    live_or_still = c(rep("Live birth", 19), "Stillbirth"),
    dobyr = c(2024, 2025),
    usual_residence = c(rep("RegionA", 5), rep("RegionB", 15))
  )

  pop <- data.frame(
    Year = factor(rep(c(2024, 2025), each = 3), fcts$dobyr),
    Geography = rep(c("Country", "RegionA", "RegionB"), 2),
    Age = "All ages",
    Sex = "All sexes",
    Population = c(1234, 900, 334, 333, 111, 222)
  )

  expected <- data.frame(
    Year = factor(rep(c(2025, 2024), each = 3), c("2025", "2024")),
    `Place of usual residence` = rep(c("Country", "RegionA", "RegionB"), 2),
    `Counts of live births` = c(9, 2, 7, 10, 3, 7),
    Population = c(333, 111, 222, 1234, 900, 334),
    `Crude birth rate` = c(270.3, 180.2, 315.3, 81.0, 33.3, 209.6),
    check.names = FALSE
  )

  output <- calc_cbr_usual_residence(
    data,
    pop,
    fcts,
    by_pop = 10000,
    digits = 1
  )

  expect_equal(expected, output)
})

test_that("Calc cbr: Not stated in the usual region, other defaults", {

  fcts <- list(
    dobyr = c("2025", "2024"),
    live_or_still = c("Live birth", "Stillbirth"),
    usual_residence = c("RegionA", "RegionB", "Not stated")
  )

  data <- data.frame(
    live_or_still = rep("Live birth", 20),
    dobyr = c(2024, 2025),
    usual_residence = c(rep("RegionA", 5), rep("RegionB", 14), "Not stated")
  )

  pop <- data.frame(
    Year = factor(
      rep(c(2024, 2025), each = 3),
      fcts$dobyr
    ),
    Geography = factor(
      rep(c("Country", "RegionA", "RegionB"), 2),
      c("Country", "RegionA", "RegionB")
    ),
    Age = "All ages",
    Sex = "All sexes",
    Population = c(1234, 900, 334, 333, 111, 222),
    other = rep(1, 6)
  )

  expected <- data.frame(
    Year = factor(rep(c(2025, 2024), each = 3), fcts$dobyr),
    `Place of usual residence` = factor(
      rep(c("Country", "RegionA", "RegionB"), 2),
      levels = c("Country", "RegionA", "RegionB")
    ),
    `Counts of live births` = c(10, 2, 7, 10, 3, 7),
    Population = c(333, 111, 222, 1234, 900, 334),
    `Crude birth rate` = c(30.03, 18.02, 31.53, 8.10, 3.33, 20.96),
    check.names = FALSE
  )

  output <- calc_cbr_usual_residence(data, pop, fcts)

  expect_equal(expected, output)
})

test_that("Calc rate: default pop and rounding", {

  data <- data.frame(
    count = c(100:104),
    pop = c(333, 77777, 1000, 15000, 12345),
    other = rep(1, 5)
  )

  expected <- data.frame(
    count = c(100:104),
    pop = c(333, 77777, 1000, 15000, 12345),
    other = rep(1, 5),
    my_rate = c(300.30, 1.30, 102.00, 6.87, 8.42)
  )

  actual <- calc_general_rate(data,
                              "my_rate",
                              "count",
                              "pop",
                              by_pop = 1000,
                              digits = 2)

  expect_equal(expected, actual)
})

test_that("Calc rate: custom pop and rounding", {

  data <- data.frame(
    count = c(100:104),
    pop = c(333, 77777, 1000, 15000, 12345),
    other = rep(1, 5)
  )

  expected <- data.frame(
    count = c(100:104),
    pop = c(333, 77777, 1000, 15000, 12345),
    other = rep(1, 5),
    my_rate = c(150.2, 0.6, 51.0, 3.4, 4.2)
  )

  actual <- calc_general_rate(data,
                              "my_rate",
                              "count",
                              denominator = "pop",
                              by_pop = 500,
                              digits = 1)

  expect_equal(expected, actual)
})

test_that("Calc rate: default and NA", {

  data <- data.frame(
    count = c(100, NA),
    pop = c(333, 77777),
    other = rep(1, 2)
  )

  expected <- data.frame(
    count = c(100, NA),
    pop = c(333, 77777),
    other = rep(1, 2),
    my_rate = c(150.2, NA)
  )

  actual <- calc_general_rate(data,
                              "my_rate",
                              "count",
                              denominator = "pop",
                              by_pop = 500,
                              digits = 1)

  expect_equal(expected, actual)
})


test_that("Calc rate: failing validation", {

  data <- data.frame(
    count = c(100:104),
    pop = c(333, 77777, 1000, 15000, 12345),
    other = rep(1, 5)
  )

  expect_error(
    calc_general_rate(
      data = 123,
      rate_name = "rate",
      numerator = "count",
      denominator = "pop",
      by_pop = 1000,
      digits = 2
    ),
    "is.data.frame\\(data\\) is not TRUE"
  )

  expect_error(
    calc_general_rate(
      data = data,
      rate_name = "rate",
      numerator = "count",
      denominator = "pop",
      by_pop = "string",
      digits = 2
    ),
    "is.numeric\\(by_pop\\) is not TRUE"
  )

  expect_error(
    calc_general_rate(
      data = data,
      rate_name = "rate",
      numerator = "not_count",
      denominator = "pop",
      by_pop = 1000,
      digits = 2
    ),
    "numerator %in% names\\(data\\)"
  )

  expect_error(
    calc_general_rate(
      data = data,
      rate_name = "rate",
      numerator = "count",
      denominator = "not_pop",
      by_pop = 1000,
      digits = 2
    ),
    "denominator %in% names\\(data\\)"
  )

  expect_error(
    calc_general_rate(
      data = data,
      rate_name = "rate",
      numerator = "count",
      denominator = "pop",
      by_pop = c(1000, 2000),
      digits = 2
    ),
    "length\\(by_pop\\) == 1 is not TRUE"
  )

  expect_error(
    calc_general_rate(
      data = data,
      rate_name = "rate",
      numerator = "count",
      denominator = "pop",
      by_pop = 1000,
      digits = "string"
    ),
    "is.numeric\\(digits\\) is not TRUE"
  )

  expect_error(
    calc_general_rate(
      data = data,
      rate_name = "rate",
      numerator = "count",
      denominator = "pop",
      by_pop = 1000,
      digits = c(1, 2)
    ),
    "length\\(digits\\) == 1 is not TRUE"
  )

})

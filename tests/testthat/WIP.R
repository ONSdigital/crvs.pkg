test_that("Processing pop for ASFR works on valid input", {
  
  pop <- data.frame(
    Year = factor(rep(c("2024", "2025"), each = 12)),
    Geography = factor(
      rep(c("Country", "RegionA", "RegionB"), each = 4, times = 2)),
    Sex = factor(rep(c("Female", "Male"), each = 2, times = 6)),
    Age = factor(rep(c(16:27), times = 2)),
    Population = rep(1:24)
  )
  
  fcts <- list(
    dobyr = c("2025", "2024"),
    asfr_mother_age_group = c("Under 20", "20 and over", "Not stated")
  )
  
  data <- data.frame(
    
  )
  
  expected <- tibble::tibble(
    Year = factor(rep(2024:2025, each = 9),
                  levels = c("2024", "2025")),
    Geography = factor(
      rep(c("Country", "RegionA", "RegionB"), each = 3, times = 2)),
    `Mothers age group` = factor(
      rep(fcts$asfr_mother_age_group, times = 6),
      levels = fcts$asfr_mother_age_group),
    Population = c(
      3, 0, 0, 0, 11, 0, 0, 9, 0, 27, 0, 0, 0, 35, 0, 0, 21, 0)
  )
  
  output <- process_pop_for_asfr(pop,
                                 fcts = fcts, 
                                 pop_sex = "Female",
                                 fert_range = 15:24)

  expect_equal(expected, output)
})


test_that("Age-spec fert rates, defaults", {
  
  fcts <- list(
    live_or_still = c("Live birth", "Stillbirth"),
    asfr_mother_age_group = c(
      "Under 20",
      "20 to 24",
      "25 to 29",
      "30 to 34",
      "35 to 39",
      "40 and over",
      "Not stated"),
    all_ages = c(
      "Under 15",
      "15 to 19",
      "20 to 24",
      "25 to 29",
      "30 to 34",
      "35 to 39",
      "40 and over"),
    age_total = "All ages",
    geog = c("RegionA",
             "RegionB"),
    geog_total = "Country"
  )
  
  data <- data.frame(
    dobyr = c(rep(2025, 20), rep(2024, 22)),
    age_grp_m = rep(birth_fcts$all_ages, 6),
    usual_residence = c(rep("RegionA", 11), rep("RegionB", 9),
                        rep("RegionA", 12), rep("RegionB", 10)),
    live_or_still = c(rep("Live birth", 40), rep("Stillbirth", 2)),
    other = rep(1)
  )
  
  pop <- data.frame(
    Year = c(rep(2025, 21), rep(2024, 21)),
    Geography = rep(c("Country", "RegionA", "RegionB"), 14),
    Age = rep(
      c("All ages", "Under 20", "20 to 24",
        "25 to 29", "30 to 34",
        "35 to 39", "40 and over"),
      each = 3, times = 2),
    Population = c(
      15384, 7547, 7837,
      1628, 883, 745,
      2573, 1684, 889,
      2347, 1433, 914,
      2240, 1165, 1075,
      3052, 1183, 1869,
      3544, 1199, 2345,
      14318, 7599, 6719,
      1527, 1089, 438,
      2416, 709, 1707,
      2360, 1439, 921,
      2022, 560, 1462,
      2024, 1573, 451,
      3969, 2229, 1740),
    other = 1:42)
  
  expected <- tibble::tibble(
    dobyr = c(rep(2025, 21), rep(2024, 21)),
    usual_residence = factor(rep(c("Country", "RegionA", "RegionB"), 14),
                             c(fcts$geog_total, fcts$geog)),
    age_grp_m = factor(rep(
      c("All ages", "Under 20", "20 to 24",
        "25 to 29", "30 to 34",
        "35 to 39", "40 and over"),
      each = 3, times = 2),
      c("All ages", fcts$asfr_mother_age_group)),
    `Age-specific fertility rate` = c(
      1.30, 1.46, 1.15, 3.69, 4.53, 2.68, 1.17, 1.19, 1.12, 1.28,
      1.40, 1.09, 1.34, 0.86, 1.86, 0.98, 0.85, 1.07, 0.56, 0.83,
      0.43, 1.40, 1.58, 1.19, 3.93, 3.67, 4.57, 1.24, 2.82, 0.59,
      1.27, 1.39, 1.09, 1.48, 1.79, 1.37, 0.99, 0.64, 2.22, 0.76,
      0.90, 0.57))
  
  actual <- calc_age_spec_fert_rate_m(data, pop, birth_fcts)
  
  expect_equal(expected, actual)
  
})

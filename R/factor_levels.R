#' List of strings to replace "Sum" with in columns
#'
#' @description The process of calculating totals (`rename_multi_sum_str`,
#' `rename_sum_str`) for all combinations of multiple variables creates a factor
#' level of "Sum" which should be re-coded to the true factor level.
#' The name of the string should match the name of the vector from the config.
#'
#' @return List object of strings to use for publication-ready totals.
#' @export
#'
sum_to_string <- function() {
  list(
    live_or_still = "All births",
    dobyr = "All years",
    year = "All years",
    urban_rural = "All places rural or urban",
    live_or_still = "All births",
    age_grp_m = "All ages",
    asfr_mother_age_group = "All ages",
    age_m = "All ages",
    pop_age = "All ages",
    mar_stat = "All marital statuses",
    sex = "All sexes",
    pob = "Country",
    geog = "Country",
    usual_residence = "Country",
    bth_attendant = "All attendants",
    pob_type = "All types of place of birth"
  )
}

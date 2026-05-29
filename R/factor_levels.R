#' Labels to replace "Sum" in output columns
#'
#' @description
#' When totals are calculated using [rename_multi_sum_str()] or
#' [rename_sum_str()], the value `"Sum"` is added as a factor level.
#' This function provides the labels used to replace `"Sum"` with
#' clearer, more meaningful text for reporting.
#'
#' Each item in the list matches a variable/column name and gives the label
#' that should be used instead of `"Sum"` for that variable.
#'
#' The names of the list elements must match the variable names (columns)
#' defined in the configuration to ensure correct recoding.
#'
#' @details
#' This helper is typically used in post-processing steps to standardise
#' totals across outputs, ensuring consistency in tables, charts, and reports.
#'
#' @return
#' A named list of character strings where each name corresponds to a variable
#' and each value is the label used to replace `"Sum"` for that variable.
#'
#' @examples
#' sum_labels <- sum_to_string()
#' sum_labels$sex
#'
#' @export
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

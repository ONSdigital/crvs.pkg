#' Error if required columns not present
#'
#' @description Stops the execution of the function and returns an error
#' message stating which of the required columns are missing.
#'
#' @param data Dataframe to check.
#' @param required_cols Vector of strings of the required columns.
#'
#' @return Error message stating which of the required columns are missing
#' @export
check_columns_present <- function(data, required_cols) {
  if (!all(required_cols %in% colnames(data))) {
    missing_cols <- required_cols[!required_cols %in% colnames(data)]
    stop(
      "Missing required columns: ", paste(missing_cols, collapse = ", "), "."
    )
  }
  data
}

#' Generate a validation report for a particular column
#'
#' @param col Column to apply validation to, in format data$column.
#' @param valid_func The validation function to apply to the column.
#'
#' @return List output of validation report.
#' @export
generate_report <- function(col, valid_func) {

  valid_results <- valid_func(col)

  results <- list(
    pass_count = length(which(valid_results)),
    fail_count = length(which(!valid_results)),
    null_count = length(which(is.na(col))),
    fail_indices = which(!valid_results),
    null_indices = which(is.na(col))
  )

  if (results$fail_count != 0) {
    warning(
      paste0("There are ", results$fail_count, " records that fail validation",
             " for ", substitute(valid_func), ". Check validation report.")
    )
  }

  results
}

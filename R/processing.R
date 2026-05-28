#' Derive age column in data frame
#'
#' @description Uses a date of birth column and a column for the date to
#' calculate to. Dynamically selects what to name the column. Errors if the
#' columns provided are not Date type.
#'
#' @param data Data frame.
#' @param age_col The unquoted name of the new column to store the calculated
#' age.
#' @param dob_col The unquoted name of the Date type column containing the date
#' of birth values.
#' @param end_date_col The unquoted name of the Date type column containing the
#' end date of calculation.
#'
#' @return Data frame with added age column.
#' @export
#'
derive_age <- function(data, age_col, dob_col, end_date_col) {

  if (class(dplyr::pull(data, {{ dob_col }})) == "Date" &&
        class(dplyr::pull(data, {{ end_date_col }})) == "Date") {
    dplyr::mutate(
      data,
      {{ age_col }} := as.integer(
        difftime({{ end_date_col}}, {{ dob_col }}, units = "days") / 365.25
      )
    )
  } else {
    stop("Both `dob_col` and `end_date_col` must be Date type.")
  }
}


#' Derive timeliness (registration delay)
#'
#' @description The time between event and registration (registration delay) is
#' a valuable metric for assessing if the civil registration process is working.
#' The delay can be classified whether the delay is within an acceptable
#' threshold, late or delayed. These thresholds can be based on UN guidelines or
#' laws within a country.
#'
#' @details The categories are: current, late and then delayed.
#'
#' @param data Data frame.
#' @param date_event_col The unquoted name of the column containing the date of
#' event.
#' @param date_reg_col The unquoted name of the column containing the date of
#' registration of the event.
#' @param threshold_late Numeric value of when a registration goes from current
#' to late. Default is 30. The late threshold is not inclusive for "current"
#' (e.g. a 30 day delay with a late threshold of 30 would be late).
#' @param threshold_delayed Numeric value of when a registration goes from late
#' to delayed Default is 365. The delayed threshold is not inclusive for "late"
#' (e.g. a 365 day delay with a delayed threshold of 365 would be delayed).
#'
#' @return Data frame with data1c (days between event and registration) and
#' data2c (category of timeliness) columns.
#' @export
#'
derive_timeliness <- function(data,
                              date_event_col,
                              date_reg_col,
                              threshold_late = 30,
                              threshold_delayed = 365) {

  dplyr::mutate(
    data,
    diff_reg_occ = as.numeric(
      difftime({{ date_reg_col }}, {{ date_event_col }}, units = "days")
    ),
    timeliness = dplyr::case_when(
      diff_reg_occ %in% c(0:(threshold_late - 1)) ~ "current",
      diff_reg_occ >= threshold_late &
        diff_reg_occ < threshold_delayed ~ "late",
      diff_reg_occ >= threshold_delayed ~ "delayed",
      TRUE ~ NA_character_
    )
  )
}


#' Derive year
#'
#' @param data Data frame.
#' @param date_col The unquoted name of the column containing the date to
#' extract the year from. The date must Date type in format "YYYY-MM-DD".
#' @param year_col The unquoted name of the new column to store the year.
#' Default is year.
#'
#' @return Data frame with new column for year.
#' @export
#'
derive_year <- function(data, date_col, year_col = year) {

  dplyr::mutate(
    data,
    {{ year_col }} := as.integer(format({{ date_col }}, "%Y"))
  )
}

#' Group age column as specified
#'
#' @param data Data frame.
#' @param age_col Quoted name of age column.
#' @param new_col Quoted name of new column for derived groups.
#' @param start_brk Integer for where granular age groups begin (anything less
#' grouped together).
#' @param end_brk Integer for where granular age groups end (anything greater
#' lumped into one group).
#' @param brk_step Integer for size of age groups (e.g. if 5, then groups of
#' 20 - 24, 25 - 29, ...).
#' @param col_factor Vector containing the order of values.
#'
#' @return Data frame with new age group column.
#' @export
group_age <- function(data,
                      age_col,
                      new_col,
                      start_brk,
                      end_brk,
                      brk_step,
                      col_factor) {

  breaks <- c(1, seq(start_brk, end_brk, by = brk_step), 200, Inf)

  if (!is.numeric(data[[age_col]])) {
    stop("`age_col` must be numeric.")
  }

  if (length(breaks) != length(col_factor) + 1) {
    stop(
      paste(
        "The number of items in `col_factor` must be:",
        "(end_brk - start_brk) / brk_step + 3. The final item must relate to",
        "a not stated category."
      )
    )
  }

  dplyr::mutate(
    data,
    !!new_col := dplyr::if_else(is.na(.data[[age_col]]), 200, .data[[age_col]]),
    !!new_col := cut(
      .data[[new_col]],
      breaks = breaks,
      labels = col_factor,
      right = FALSE
    )
  )
}

#' Rename columns in a data frame using a lookup table
#'
#' Renames columns in a data frame based on a lookup table that maps old column
#' names to new column names. Column names are supplied using tidy‑evaluation
#' (data masking), so should be provided without quote marks.
#'
#' @param df A data frame with columns to be renamed.
#' @param lookup A data frame containing the lookup between old and new column
#'   names.
#' @param old_names An unquoted column name in `lookup` containing the existing
#'   column names in `df`. Defaults to `variable_id`.
#' @param new_names An unquoted column name in `lookup` containing the new
#'   column names to apply. Defaults to `variable_name`.
#'
#' @details
#' The function renames matching columns according to the lookup table.
#' Columns in `df` that do not appear in the lookup table are left unchanged.
#'
#' @return A data frame with renamed columns or original columns if no match in
#' lookup.
#'
#' @export
rename_crvs_columns <- function(df,
                                lookup,
                                old_names = input_variable_id,
                                new_names = standardised_variable_name) {

  if (nrow(df) == 0) {
    stop("The data frame to rename is empty.")
  }

  extra_cols <- setdiff(names(df), dplyr::pull(lookup, {{ old_names }}))

  if (length(extra_cols) > 0) {
    warning(
      paste(
        "Data frame contains columns not in the lookup table:",
        paste(extra_cols, collapse = ", "), ", consider adding to the",
        "data lookup."
      )
    )
  }

  rename_map <- setNames(dplyr::pull(lookup, {{ new_names }}),
                         dplyr::pull(lookup, {{ old_names }}))

  names(df) <- dplyr::if_else(
    names(df) %in% names(rename_map),
    rename_map[names(df)],
    names(df)
  )
  df
}

#' Convert to factor with levels, no NA
#'
#' @description Convert NA to "Not stated" and change the column to a factor
#' with defined factor levels.
#'
#' @param data Data frame containing variable to change.
#' @param col_str String of the column to change.
#' @param fcts_vector Character vector of the variable to use as factor levels.
#'
#' @return Original data frame with changed variable
#' @export
#'
derive_var <- function(data, col_str, fcts_vector) {
  data |>
    make_na_not_stated(col_str) |>
    dplyr::mutate(!!col_str := factor(.data[[col_str]], levels = fcts_vector))
}

#' Convert a missing/NA into "Not stated"
#'
#' @description Processing sub-function that cleans data by converting NA into
#' "Not stated" or keeps the original value, and makes the column character).
#' Used in other processing functions.
#'
#' @param data Data frame with the column to convert.
#' @param colname String of column name to change.
#'
#' @return Original data frame with the column converted.
#' @export
#'
make_na_not_stated <- function(data, colname) {
  dplyr::mutate(
    data,
    !!colname := dplyr::if_else(
      is.na(.data[[colname]]),
      "Not stated",
      as.character(.data[[colname]])
    )
  )
}

#' Derive live or still birth from variable codes
#'
#' @description The `birth_type` column is replaced with `live_or_still` factor
#' with two levels: "Live birth" and "Stillbirth".
#'
#' @param data Data frame with `birth_type` column.
#' @param fcts List of character vectors with the `live_or_still` factor levels.
#'
#' @return Original data frame with recoded variable.
#' @export
#'
derive_live_or_still <- function(data, fcts) {
  data |>
    dplyr::mutate(
      live_or_still = factor(
        dplyr::case_match(birth_type, NA ~ "Live birth", 1 ~ "Stillbirth"),
        levels = fcts$live_or_still
      )
    ) |>
    dplyr::select(-birth_type)
}

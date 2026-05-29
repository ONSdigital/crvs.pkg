#' Round values using half-up rounding (e.g. 0.5 --> 1)
#'
#' @description Replaces R round function to round half values up function
#' across the package to align with how Excel rounds mathematically.
#' Rounds numeric values using "round half up" behaviour, where values
#' exactly halfway between two possibilities are always rounded up.
#'
#' @param x Numeric vector to round.
#' @param digits Integer. Number of digits to round to, default 1.
#'
#' @details
#' Not currently exported.
#'
#' This function is a thin wrapper around [janitor::round_half_up()],
#' providing a consistent rounding method for reporting and publication.
#'
#' Note: this function masks [base::round()].
#'
#' @returns Numeric vector with each element rounded to chosen number of digits.
#'
#' @examples
#' round(c(1.25, 1.35), digits = 1)
#' round(0.5)
#'
round <- function(x, digits = 1) {
  janitor::round_half_up(x, digits)
}


#' Count rows by grouping variables
#'
#' @description Groups `data` by one or more chosen variables/columns
#' (`grouping_cols`), counts the number of rows in each group. All combinations
#' of factor variable levels will be returned, even with no instances (i.e.
#' counts of 0).
#'
#' @param data Data frame.
#' @param grouping_cols Vector of character strings of column names to group by.
#'
#' @return Data frame with counts column (ungrouped).
#'
#' @examples
#' df <- data.frame(
#'   region = c("A", "A", "B"),
#'   sex = c("Male", "Female", "Male")
#' )
#'
#' group_by_summarise_count(df, c("region", "sex"))
#'
#' @export
group_by_summarise_count <- function(data, grouping_cols) {
  data |>
    dplyr::group_by(
      dplyr::across(dplyr::all_of(grouping_cols)),
      .drop = FALSE
    ) |>
    dplyr::summarise(count = dplyr::n()) |>
    dplyr::ungroup()
}


#' Sum values by grouping variables
#'
#' @description Groups by one or more chosen variables/columns
#' (`grouping_cols`), calculates the sum of a chosen numeric variable/column
#' (`sum_col`) in each group. All combinations of factor variable levels will be
#' returned, even with no instances (i.e. counts of 0).
#'
#' @param data A data frame containing at least the `grouping_cols` and 
#' `sum_col`.
#' @param grouping_cols Vector of character strings of column names to group by.
#' @param sum_col A character string of the numeric column name to sum.
#'
#' @returns A data frame with one row per unique combination of grouping
#' variables and a column containing the summed values.
#'
#' @examples
#' df <- data.frame(
#'   region = c("A", "A", "B"),
#'   sex = c("Male", "Female", "Male"),
#'   value = c(10, 20, 30)
#' )
#'
#' group_by_summarise_sum(df, c("region", "sex"), "value")
#'
#' @export
group_by_summarise_sum <- function(data, grouping_cols, sum_col) {
  data |>
    dplyr::group_by(
      dplyr::across(dplyr::all_of(grouping_cols)),
      .drop = FALSE
    ) |>
    dplyr::summarise(!! sum_col := sum(.data[[sum_col]]), .groups = "drop") |>
    dplyr::ungroup()
}


#' Create cross-tabulated counts or sums with totals added
#'
#' @description Aggregates a dataset by specified grouping variables
#' (`grouping_cols`), producing either counts or sums,
#' and returns a cross-tabulation (all combinations of the variables) with the
#' totals/sums (marginal totals) calculated.
#'
#' @param data Processed dataframe with no NA in grouping columns (should be
#' "Not stated" or similar).
#' @param col Unquoted name to rename the count column with.
#' @param grouping_cols Vector of strings of column names to be used when
#' grouping, summarising and counting.
#' @param count_or_sum String of "count" or "sum" to define if the chosen column
#' should count by rows or sum. Default is "count".
#' @param sum_col What is the column to be summed if `count_or_sum` is "sum".
#' Should be supplied unquoted. Default is NULL.
#'
#' @return Dataframe.
#' @export
#'
summarise_count_vars <- function(data,
                                 pub_name_col,
                                 grouping_cols,
                                 count_or_sum = "count",
                                 sum_col) {

  if (sum(is.na(data[grouping_cols])) > 0) {
    warning(paste("There are NA in the grouping columns, check data has been",
                  "processed correctly."))
  }

  if (count_or_sum == "count") {

    data <- group_by_summarise_count(data, grouping_cols)
    sum_col <- "count"

  } else if (count_or_sum == "sum") {

    if (!all(sum_col %in% colnames(data))) {
      stop(
        paste('If `count_or_sum` is "sum", `sum_col` must not be NULL',
              'and must be a column name in `data`')
      )
    }

    data <- group_by_summarise_sum(data,
                                   grouping_cols = grouping_cols,
                                   sum_col = sum_col)

  } else {
    stop('`count_or_sum` must be either "count" or "sum"')
  }

  formula <- paste0(sum_col, " ~ ", paste(grouping_cols, collapse = " + "))

  xtabs(as.formula(formula), data) |>
    addmargins() |>
    data.frame() |>
    dplyr::rename(!! pub_name_col := Freq)
}

#' Rename 'Sum' in multiple columns to informative string
#'
#' @description Also renames the column names.
#'
#' @param data Data frame.
#' @param cols Vector of string column names to rename and change 'Sum' to
#' correct factor level.
#' @param vars Vector of strings of variables that correspond to the names of
#' the list of factor levels from the config and also match the `sum_to_string`
#' function.
#' @param pub_colnames Vector strings of new column names for the `vars`
#' columns.
#'
#' @return Formatted data frame.
#' @export
#'
rename_multi_sum_str <- function(data,
                                 cols,
                                 vars,
                                 pub_colnames,
                                 fcts) {

  if (length(cols) != length(pub_colnames) || length(cols) != length(vars)) {
    stop("`vars`, `cols` and `pub_colnames` and must be the same length.")
  }

  for (x in seq_along(vars)) {
    data <- rename_sum_str(data, cols[x], vars[x], pub_colnames[x], fcts)
  }
  data
}

#' Rename 'Sum' in columns to informative string
#'
#' @param data Data frame.
#' @param col Name of column to mutate as a string.
#' @param var Quoted string of name of variable that corresponds with the names
#' of the vectors of factor levels in the config and `sum_str`.
#' @param pub_colname Quoted column name for publishing.
#' @param fcts List of character vectors from config for the factor levels to
#' use.
#'
#' @return Data frame.
#' @export
rename_sum_str <- function(data, col, var, pub_colname, fcts) {

  s_name <- sum_to_string()[[var]]
  data |>
    dplyr::mutate(
      !!col := factor(
        dplyr::if_else(.data[[col]] == "Sum", s_name, .data[[col]]),
        levels = c(s_name, fcts[[var]])
      )
    ) |>
    dplyr::rename(!!pub_colname := all_of(col))
}

#' Convert selected data.frame columns to factors using a named list of levels
#'
#' @param df A data.frame (or tibble).
#' @param level_list A named list. Each name must match a column in df.
#' Each value must be a character vector of desired levels.
#' @param cols Optional character vector of column names to convert.
#' Defaults to names(level_list).
#' @param strict Logical; if TRUE, error when df has values not found in
#' provided levels. If FALSE, those values become NA with a warning.
#'
#' @return The modified data.frame with selected columns turned into factors.
factorise_cols <- function(df,
                           level_list,
                           cols = names(level_list),
                           strict = TRUE) {

  if (!is.list(level_list) || is.null(names(level_list))) {
    stop(paste("`level_list` must be a named list where each name matches a",
               "col in `df` and each value is a character vector of levels."))
  }

  if (is.null(cols)) {
    stop(paste("`cols` cannot be NULL. Provide column names or ensure",
               "`level_list` has names."))
  }

  if (!all(cols %in% names(df))) {
    missing_cols <- setdiff(cols, names(df))
    message("These columns from fct object are not in `df` so ignored: ",
            paste(missing_cols, collapse = ", "))
    cols <- cols[!cols %in% missing_cols]
  }

  if (!all(cols %in% names(level_list))) {
    missing_levels <- setdiff(cols, names(level_list))
    stop("`level_list` is missing level definitions for: ",
         paste(missing_levels, collapse = ", "))
  }

  for (col in cols) {
    x <- df[[col]]

    if (!is.character(x)) {
      x <- as.character(x)
    }

    levels_vec <- level_list[[col]]
    if (!is.character(levels_vec)) {
      stop("Levels for column '", col, "' must be a character vector.")
    }

    # Strict validation: ensure all non-NA values are in provided levels
    unknown <- setdiff(unique(x[!is.na(x)]), levels_vec)

    if (length(unknown) > 0) {
      if (strict) {
        stop(
          paste0("Column '", col, "' contains values not in provided levels: ",
                 paste(unknown, collapse = ", "),
                 ". Set `strict = FALSE` to coerce unknowns to NA.")
        )
      } else {
        warning(
          paste0("Column '", col, "' contains values not in provided levels: ",
                 paste(unknown, collapse = ", "), "; these will be set to NA.")
        )
      }
    }

    df[[col]] <- factor(x, levels = levels_vec)
  }
  df
}

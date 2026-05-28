#' General function to count by chosen aggregation columns
#'
#' @param data Processed data frame (including factorising aggregation
#' variables).
#' @param agg_cols Vector of characters of input data frame variable names of
#' variables to aggregate by.
#' @param agg_cols_pub_names Vector of characters of intended names to rename
#' the input aggregation variables to (must be same length as
#' `agg_cols_pub_names`). Should be publication ready.
#' @param agg_count_colname Symbol (unquoted or ``) to name the aggregated
#' count column. Default is `Counts of births`.
#' @param fcts List object of character vectors of factor levels within each of
#' the aggregation variables from `config.R`.
#' @param pre_agg_levels_to_keep List object of character vectors to filter the
#' input dataset before aggregation.
#' Names of list items must relate to the input column names (`agg_cols`).
#' Items in the list are the factor levels to keep.
#' Default is no filers applied. See `filter_factor_levels` for more information
#' on the filtering.
#' @param post_agg_levels_to_keep List object of character vectors to filter the
#' input dataset after aggregation.
#' Names of list items must relate to the input column names (`agg_cols`).
#' Items in the list are the factor levels to keep.
#' Default is no filers applied. See `filter_factor_levels` for more information
#' on the filtering.
#'
#' @return Aggregated counts of chosen variables and breakdowns.
#' @export
general_count_by_var <- function(data,
                                 agg_cols,
                                 agg_cols_pub_names,
                                 agg_count_colname = "Counts of births",
                                 count_or_sum = "count",
                                 sum_col = NULL,
                                 fcts,
                                 pre_agg_levels_to_keep = list(),
                                 post_agg_levels_to_keep = list()) {

  data <- check_columns_present(data, agg_cols)

  data |>
    filter_factor_levels(pre_agg_levels_to_keep) |>
    summarise_count_vars(
      pub_name_col = agg_count_colname,
      grouping_cols = agg_cols,
      count_or_sum = count_or_sum,
      sum_col = sum_col
    ) |>
    rename_multi_sum_str(
      cols = agg_cols,
      vars = agg_cols,
      pub_colnames = agg_cols_pub_names,
      fcts = fcts
    ) |>
    filter_factor_levels(post_agg_levels_to_keep) |>
    dplyr::arrange(dplyr::across(dplyr::all_of(agg_cols_pub_names))) |>
    droplevels() |>
    dplyr::ungroup()
}

#' Filter multiple factor or character columns by allowed levels
#'
#' Filters a data frame by keeping only rows where specified
#' factor or character columns contain allowed levels.
#' Each column is filtered independently, and all specified
#' filters are combined using logical AND.
#'
#' @param data A data frame to be filtered.
#'
#' @param levels_to_keep A named list defining the allowed levels
#'   for each column. Names must correspond to column names in
#'   \code{data}. Values should be character vectors of levels to
#'   retain. Columns with \code{NULL} values are ignored.
#'
#' @details
#' This function is designed for production use where filtering
#' rules may vary by column and be defined dynamically.
#'
#' For each element in \code{levels_to_keep}:
#' \itemize{
#'   \item The column must exist in \code{data}
#'   \item The column must be of type factor or character
#'   \item Rows are retained if the column value is contained
#'         in the corresponding vector of allowed levels
#' }
#'
#' All filters are applied sequentially, meaning rows must satisfy
#' every specified column filter to be kept
#'
#' @return
#' A data frame filtered to include only rows matching all specified
#' factor or character level constraints.
#'
#' @export
filter_factor_levels <- function(data, levels_to_keep = list()) {

  stopifnot(is.data.frame(data))

  for (col in names(levels_to_keep)) {
    if (!col %in% colnames(data)) {
      stop(paste("Missing column:", col))
    }

    if (!is.factor(data[[col]]) && !is.character(data[[col]])) {
      stop(paste("Column is not factor or character:", col))
    }

    allowed <- levels_to_keep[[col]]

    if (!is.null(allowed)) {
      data <- dplyr::filter(data, .data[[col]] %in% allowed)
    }
  }
  data
}

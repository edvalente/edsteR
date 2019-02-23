#' Find missing cells
#'
#' Identifies how many rows would be
#' removed if columns with number of missing
#' above tolerance are removed.
#'
#' @rdname remove_missing
#' @param data data.frame to be checked
#' @param tolerance maximum fraction of missing
#' values allowed in a column.
#' @return summary of rows and columns that
#' could be removed
#' @export
find_missing <- function(data, tolerance = 0.9) {
  cols_missing <- vapply(data, function(x) sum(is.na(x)), FUN.VALUE = integer(1L))
  ncols <- ncol(data)
  nrows <- nrow(data)
  removed_cols <- names(cols_missing)[cols_missing/nrows >= tolerance]
  remaining_cols <- colnames(data)[!colnames(data) %in% removed_cols]
  n_removed_rows <- sum(apply(data[remaining_cols], function(x) any(is.na(x)), MARGIN = 1))
  perc_rows_removed <- round(n_removed_rows / nrows, 4)
  perc_cols_removed <- round(length(removed_cols) / ncols, 4)

  return(list(
    "cols_removed" = removed_cols,
    "cols_remaining" = remaining_cols,
    "n_removed_cols" = length(removed_cols),
    "n_removed_rows" = n_removed_rows,
    "perc_rows_removed" = perc_rows_removed,
    "perc_cols_removed" = perc_cols_removed
  ))
}

#' Remove missing values
#'
#' Removes rows with missing values after
#' removing columns with fraction of missing
#' values above tolerance.
#'
#' @param data data.frame to be checked
#' @param tolerance maximum fraction of missing
#' values allowed in a column.
#' @return summary of rows and columns that
#' @return summary of rows and columns that
#' could be removed
#' @export
remove_missing <- function(data, tolerance = 0.9) {
  missing <- find_missing(data, tolerance)
  message("Removed ", missing$n_removed_cols, " cols")
  message("Removed ", missing$n_removed_rows, " rows")

  return(tidyr::drop_na(data[missing$cols_remaining]))
}

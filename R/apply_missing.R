#' Transforms strings to NA
#'
#' @param df data.frame with values that should be NA
#' @param missing_strings strings that should be replaced by NA
#' @return data.frame with strings replaced by NA
#' @export
apply_missing <- function(df, missing_strings=c("", "NA", "NULL")) {
  missing_patterns = paste0("(^", missing_strings, "$)", collapse="|")
  has_empty <- vapply(df, function(x) {
    any(as.character(x[!is.na(x)]) %in% missing_strings)
  }, FUN.VALUE = logical(length = 1L))
  if (any(has_empty)) {
    df[has_empty] <- lapply(df[has_empty], function(x) gsub(missing_patterns, NA, x))
  }
  return(df)
}

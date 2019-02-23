#' Saves DataFrame to compressed file
#'
#' Saves a DataFrame to compressed .xz or .gz file. Default is .xz
#'
#' @param df DataFrame to be saved
#' @param path String. Path to file to be saved without extension
#' @param compress String. Type of compression. Only using .xz or .gz. Default is .xz
#' @return NULL
#' @export
write_compressed <- function(df, path, compress='xz'){
  file_ext <- paste(path, compress, sep='.')
  file <- if (compress == 'xz') {
    xzfile(file_ext, "w")
  } else {
    gzfile(file_ext, "w")
  }
  utils::write.csv(df, file, fileEncoding="UTF-8", row.names=FALSE)
  close(file)
}

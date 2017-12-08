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
  file_ext <- paste(path, '.', compress, sep='')
  if(compress == 'xz'){
    to_xz <- xzfile(file_ext, "w")
    utils::write.csv(df, to_xz)
    close(to_xz)
  } else {
    to_gz <- gzfile(file_ext, "w")
    utils::write.csv(df, to_gz)
    close(to_gz)
  }
}

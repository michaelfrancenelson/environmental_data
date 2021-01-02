#' Get the yaml header from an Rmd file
#'
#' @param filename the name of the Rmd file to read
#' @param file_lines a character vector containing the lines of a source Rmd file.
#'
#' @export

get_rmd_header = function(filename, file_lines = NULL)
{
  if (is.null(file_lines)) file_lines = readLines(filename)
  header_symbols = which(grepl("---", file_lines))
  return(file_lines[header_symbols[1]:header_symbols[2]])
}

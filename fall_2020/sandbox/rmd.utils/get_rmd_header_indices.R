#' Get the line indices of the header in an Rmd file
#'
#' @param filename the name of the Rmd file to read
#' @param file_lines a character vector containing the lines of a source Rmd file.
#' @param header_delimiter the delimiter for the beginning and ending of the Rmd header section.
#'
#' @export

get_rmd_header = function(filename, file_lines = NULL, header_delimiter = "----")
{
  if (is.null(file_lines)) file_lines = readLines(filename)
  return(which(grepl(header_delimiter, file_lines))[1:2])
}

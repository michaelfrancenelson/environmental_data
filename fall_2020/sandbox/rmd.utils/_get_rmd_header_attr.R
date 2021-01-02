#' 
#' 
#' @param filename the name of the Rmd file to read
#' @param file_lines a character vector containing the lines of a source Rmd file.
#' @param attr_prefix the attribute to retrieve
#' @param header_delimiter the delimiter for the beginning and ending of the Rmd header section.

get_rmd_header_attr = function(
  filename, 
  file_lines = NULL, 
  attr_prefix = "title:",
  header_delimiter = "----")
{
  if (is.null(file_lines)) file_lines = readLines(filename, warn = FALSE)
  
  header_indices = get_rmd_header_indices(NULL, file_lines = file_lines, header_delimiter = header_delimiter)
  header_lines = file_lines[header_indices[1]:header_indices[2]]
  header_line = header_lines[grepl(attr_prefix, header_lines)]
  header_attr = gsub("\"", "", trimws(gsub(attr_prefix, "", header_line)))
                            
  return(header_attr)
}

#' Swap the value of an Rmd header attribute
#'
#' @param header_lines A character vector with the lines of the header
#' @param attr_prefix The name of the attribute (with ending colon)
#' @param new_attr_val The value to substitute
#'
#'
#'
#' @export

substitute_rmd_header_attr = function(
  header_lines,
  attr_prefix = "title:",
  new_attr_val)
{
  attr_line = which(grepl(attr_prefix, header_lines))
  header_lines[attr_line] = sprintf("%s %s", attr_prefix, new_attr_val)
  return(header_lines)
}

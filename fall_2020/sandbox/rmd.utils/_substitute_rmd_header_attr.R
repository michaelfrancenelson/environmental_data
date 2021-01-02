substitute_rmd_header_attr = function(
  header_lines, 
  attr_prefix = "title:",
  new_attr)
{
  attr_line = which(grepl(attr_prefix, header_lines))
  header_lines[attr_line] = sprintf("%s %s", attr_prefix, new_attr)
  return(header_lines)
}
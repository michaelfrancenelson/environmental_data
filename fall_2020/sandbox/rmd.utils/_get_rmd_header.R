get_rmd_header = function(filename, file_lines = NULL)
{
  if (is.null(file_lines)) file_lines = readLines(filename)
  header_symbols = which(grepl("---", file_lines))
  return(file_lines[header_symbols[1]:header_symbols[2]])
}

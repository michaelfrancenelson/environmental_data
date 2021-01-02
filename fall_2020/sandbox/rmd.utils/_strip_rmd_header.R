if (FALSE)
{
  source(here::here("rmd_tools", "find_file.R"))
  filename = find_file("Q6_sim_data_", extension = ".Rmd")
strip_rmd_header(filename)  
}
strip_rmd_header = function(filename, file_lines = NULL)
{
  if (is.null(file_lines)) file_lines = readLines(filename)
  header_symbols = which(grepl("---", file_lines))
  return(file_lines[(header_symbols[2] + 1):length(file_lines)])
}



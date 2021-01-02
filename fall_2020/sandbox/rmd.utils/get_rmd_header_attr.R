#'
#'
#' @export

get_rmd_header_attr = function(
  filename,
  file_lines = NULL,
  header_prefix = "title:",
  yaml_header_delimiter = "----")
{
  if (FALSE)
  {
    file_lines = NULL
    filename = "C:/Users/michaelnelso/git/eco_602_634_2020/assignments/eco_602/week_03_data_exploration_deterministic_functions/moodle/Q1_histograms_elevation.Rmd"
    file_lines = readLines(filename, warn = FALSE)
    title_prefix = "title:"
  }

  get_rmd_header = function(filename, file_lines = NULL)
  {
    if (is.null(file_lines)) file_lines = readLines(filename)
    header_symbols = which(grepl("---", file_lines))
    return(file_lines[header_symbols[1]:header_symbols[2]])
  }
  if (is.null(file_lines)) file_lines = readLines(filename, warn = FALSE)

  header_lines = get_rmd_header(NULL, file_lines = file_lines)
  header_line = header_lines[grepl(header_prefix, header_lines)]
  header_attr = gsub("\"", "", trimws(gsub(header_prefix, "", header_line)))
  return(header_attr)
}

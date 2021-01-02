#' Get the line indices for a section
#'
#' Finds the line indices of a section in an .Rmd file formatted
#' for use with the 'exams' package.
#'
#' @param rmd_source_lines A character vector with the lines of the .Rmd source file.
#' @param section_name Name of section, e.g. "Question", "Solution", "Meta-information"
#' @param section_delimiter The section break delimiter to search for
#'
#' @export
#'
# @param invert_section
# @param ignore_error


get_moodle_question_section_line_indices = function(
  rmd_source_lines,
  section_name = "Meta-information",
  section_delimiter = "=====")
  # ignore_error = FALSE,
  # invert_section = FALSE)
{

  if (FALSE)
  {
    rmd_source_lines = readLines(find_file("Q01", extension = ".Rmd"))
    section_name = "Meta-information"
    section_name = "Question"
    section_delimiter = "====="
  }

  name_match = grep(section_name, rmd_source_lines)
  delim_matches = grep(section_delimiter, rmd_source_lines)

  name_match = name_match[name_match %in% (delim_matches - 1)]
  delim_index_start = which(delim_matches == name_match + 1)

  # If it is the final section, cut everything after the section name and return lines
  if ((length(delim_matches) == delim_index_start))
  {
    line_indices = name_match:length(rmd_source_lines)
  } else
  {
    line_indices = name_match:(delim_matches[delim_index_start + 1] - 2)
  }

  return(line_indices)
}

if (FALSE)
{
  rmd_source_lines = readLines(find_file("bolker_questions_alternative_hypotheses.Rmd"), warn=FALSE)
  section_name = "Meta-information"
  section_name = "Solution"
  section_delimiter = "====="
  rmd_source_lines[get_moodle_question_section_line_indices(rmd_source_lines)]
}

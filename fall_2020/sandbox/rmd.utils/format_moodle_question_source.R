#' Format a moodle question source file
#'
#' Prepare the text of a moodle question source file by including or excluding
#' the "Solution" and "Meta-information" sections.
#'
#' @param lines_i a character vector containing the lines of a Rmd source file.
#' @param include_question boolean: whether nor not to include the 'Solution' section of the source files
#' @param include_solution boolean: whether nor not to include the 'Solution' section of the source files
#' @param include_metadata boolean: whether or not to include the 'Meta-information' section of the source files.
#' @param insert_line_breaks_in_metadata
#'
#' @export


format_moodle_question_source = function(
  lines_i,
  include_question = TRUE,
  include_solution = FALSE,
  include_metadata = FALSE,
  insert_line_breaks_in_metadata = TRUE)
{

  if(FALSE)
  {
    filename = find_file("Q01", extension = ".Rmd")
    lines_i = readLines(filename)
    include_solution = FALSE
    include_metadata = TRUE
  }

  question_line_indices = get_moodle_question_section_line_indices(lines_i, section_name = "Question")
  metadata_line_indices = get_moodle_question_section_line_indices(lines_i, section_name = "Meta-information")
  solution_line_indices = get_moodle_question_section_line_indices(lines_i, section_name = "Solution")

  question = ""
  solution = ""
  metadata = ""

  if (include_metadata)
  {
    metadata = lines_i[metadata_line_indices]

    #Insert line breaks into metadata section
    if (insert_line_breaks_in_metadata)
      metadata = insert_line_breaks(metadata)
  }

  if (include_solution)
  {
    solution = lines_i[solution_line_indices]
  }

  if (include_question)
  {
    question = lines_i[question_line_indices]
  }

  lines_out = c(lines_i[-c(metadata_line_indices, solution_line_indices)], solution, metadata)
  return(lines_out)
}

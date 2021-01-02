format_moodle_quiz_question_source = function(
  lines_i,
  build_answer_key = FALSE,
  include_metadata = FALSE)
{
  
  if (FALSE)
  {
    lines_i = source_lines[[1]]
  }
  
  metadata_line_indices = get_moodle_question_section_line_indices(lines_i)
  solution_line_indices = get_moodle_question_section_line_indices(lines_i, section_name = "Solution")
  
  metadata = ""
  solution = ""
  
  if (include_metadata)
  {
    #Insert line breaks into metadata section
    metadata = insert_line_breaks(lines_i[metadata_lines])
    
  }
  
  if (build_answer_key)
  {
    solution = lines_i[solution_lines]
  }
  
  lines_out = c(lines_i[-c(metadata_line_indices, solution_line_indices)], solution, metadata)
  return(lines_out)
}
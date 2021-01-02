
get_moodle_question_section_line_indices = function(
  source_lines_i, 
  section_name = "Meta-information", 
  section_delimiter = "=====", 
  allow_final = TRUE, 
  ignore_error = FALSE,
  invert_section = FALSE)
{
  
  if (FALSE)
  {
    source_lines_i = source_lines[[1]]
    section_name = "Meta-information"
    section_name = "Solution"
    section_delimiter = "====="
    allow_final = TRUE
    ignore_error = FALSE
  }
  
  name_match = grep(section_name, source_lines_i)
  delim_matches = grep(section_delimiter, source_lines_i)
  delim_index_start = which(delim_matches == name_match + 1)
  
  # If it is the final section, cut everything after the section name and return lines
  if ((length(delim_matches) == delim_index_start) & allow_final)
  {
    line_indices = name_match:length(source_lines_i)
  } else
  {
    line_indices = name_match:(delim_matches[delim_index_start + 1] - 2)
  }
  
  return(line_indices)
}






get_moodle_question_section = function(
  source_lines_i, 
  section_name = "Meta-information", 
  section_delimiter = "=====", 
  allow_final = TRUE, 
  ignore_error = FALSE)
{
  
  if (FALSE)
  {
    source_lines_i = source_lines[[1]]
    section_name = "Meta-information"
    section_name = "Solution"
    section_delimiter = "====="
    allow_final = TRUE
    ignore_error = FALSE
  }
  
  name_match = grep(section_name, source_lines_i)
  delim_matches = grep(section_delimiter, source_lines_i)
  delim_index_start = which(delim_matches == name_match + 1)
  
  # If it is the final section, cut everything after the section name and return lines
  if ((length(delim_matches) == delim_index_start) & allow_final)
    return(source_lines_i[1:(name_match - 1)])
  
  lines_to_cut = (name_match - 1):delim_matches[(delim_index_start + 1)]
  
  return(source_lines_i[-lines_to_cut])
}



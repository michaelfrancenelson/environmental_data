get_moodle_question_body = function(
  filename, 
  file_lines = NULL, 
  start_header = "Question", 
  delimiter = "========", 
  end_header = "Solution")
{
  if (FALSE)
  {
    start_header = "Question"
    delimiter = "========" 
    end_header = "Solution"
    
    start_header = "Solution"
    end_header = "Meta-Information"
    
    file_lines = NULL
    
    question_source_files
    filename = question_source_files$question_source_files[1]
  }
  
  if (is.null(file_lines)) file_lines = readLines(filename)
  
  # Find adjacent lines matching the `exams` package question and solution section delimiters
  delimiter_lines = which(grepl(delimiter, file_lines))
  question_lines = which(grepl(start_header, file_lines))
  soln_lines = which(grepl(end_header, file_lines, ignore.case = TRUE))
  
  q_line = question_lines[question_lines %in% (delimiter_lines - 1)]
  s_line = soln_lines[ soln_lines %in% (delimiter_lines - 1)]
  
  if (length(q_line) != 1 | length(s_line) != 1)
    cat(sprintf(
      "Could not locate the Moodle Question and Solution delimiters in file: %s",
      filename))
  return(file_lines[(q_line + 2) : (s_line - 1)])
}
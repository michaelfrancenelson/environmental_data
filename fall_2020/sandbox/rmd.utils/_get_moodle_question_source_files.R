# Find moodle quiz question source files associated
# with an assignment.
# Searches for the source Rmd file for the assignment
# to locate individual moodle quiz question files.
get_moodle_question_source_files = function(
  assignment_rmd_filename,
  moodle_question_subdir = "moodle",
  search_path = NULL
)
{
  if (!("here" %in% library()$results)) 
    cat("\nPackage 'here' is required function 'get_question_source_files()'")
  
  # This function relies on find_file()  
  if (!("find_file" %in% ls())) 
    source(here::here("rmd_tools", "find_file.R"))
  
  if (FALSE)
  {
    assignment_rmd_filename = "week_03_data_exploration_deterministic_functions.Rmd"
    moodle_question_subdir = "moodle"
    search_path = here::here("assignments", "eco_602")
    
    get_question_source_files(
      assignment_rmd_filename = assignment_rmd_filename,
      search_path = search_path)
  }
  
  assignment_file_basename = tools::file_path_sans_ext(assignment_rmd_filename)
  
  if (file.exists(assignment_rmd_filename))
  {
    assignment_source_abs_path = assignment_rmd_filename
  } else
  {
    assignment_source_abs_path = find_file(
      assignment_rmd_filename,
      search_path = search_path)
  }  
  
  questions_abs_path = 
    file.path(
      dirname(assignment_source_abs_path), 
      moodle_question_subdir)
  stopifnot(dir.exists(questions_abs_path))
  
  question_sources_abs_paths = find_file(
    filename = ".Rmd", 
    search_path = questions_abs_path, 
    return_all = TRUE, 
    duplicated_files_error = FALSE)
  
  return(list(
    assignment_source_file = assignment_source_abs_path,
    question_source_files = question_sources_abs_paths))
}

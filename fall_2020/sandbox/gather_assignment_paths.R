#' Create an answer key for a moodle quiz-question based assignment.
#'
#' @export
#'

gather_assignment_paths = function(
  week_n,
  assignment_prefix,
  out_dir,
  # answer_key = TRUE,
  # moodle_xml = TRUE,
  moodle_xml_out_dir = here::here("docs", "moodle_quiz_questions"),
  out_fmt = "%s_%0.2d",
  key_out_dir = here::here("docs", "answer_keys"),
  key_suffix = "answer_key",
  source_dir_fmt = "%s_%0.2d_",
  source_file_fmt = "%s_%0.2d",
  source_search_path = NULL,
  moodle_question_source_subdir = "moodle",
  moodle_question_prefix = "Q_",
  key_file_fmt = "%s_%0.2d_answer_key",
  moodle_xml_out_fmt = "%s_%0.2d_moodle_questions",
  doc_source_ext = ".Rmd",
  question_source_ext = ".Rmd"
)
{
  
  if (FALSE)
  {
    
    require(rmd.utils)
    # devtools::install_github("michaelfrancenelson/rmd.utils")
    week_n = 7
    assignment_prefix = "lab"
    
    out_dir = here::here("docs", "assignments", "eco_634")
    
    out_fmt = "%s_%0.2d"
    
    source_dir_fmt = "%s_%0.2d_"
    source_file_fmt = "%s_%0.2d"
    source_search_path = NULL
    
    out_dir = here::here("docs", "assignments", "eco_602", "reading_questions")
    out_fmt = "%s_week_%0.2d"
    source_dir_fmt = "%s_week_%0.2d"
    source_file_fmt = "%s_week_%0.2d"
    source_search_path = NULL
    
    week_n = 3
    assignment_prefix = "reading_questions"
    
    key_file_fmt = "%s_week_%0.2d_answer_key"
    key_out_dir = here::here("docs", "answer_keys")
    
    moodle_question_source_subdir = "moodle"
    moodle_question_prefix = "Q_"
    moodle_xml_out_dir = here::here("docs", "moodle_quiz_questions")
    moodle_xml_out_fmt = "%s_week_%0.2d_moodle_questions"
    
    key_suffix = "answer_key"
    
    doc_source_ext = ".Rmd"
    question_source_ext = ".Rmd"
    
    # mdl_question_fmt
    # sss = sprintf("Q_%0.2s_%s_%0.2d", "%0.2d", assignment_prefix, week_n)
    
    
  }
  
  source_path =
    find_file(
      sprintf(source_dir_fmt, assignment_prefix, week_n),
      search_path =  source_search_path,
      directory = TRUE)
  
  mdl_question_path =
    file.path(
      source_path,
      moodle_question_source_subdir)
  
  doc_source_file =
    find_file(
      filename = sprintf(source_file_fmt, assignment_prefix, week_n),
      search_path = source_path,
      extension = doc_source_ext, 
      return_all = TRUE,
      recursive = FALSE)
  
  question_source_files =
    find_file(
      filename = moodle_question_prefix,
      search_path = mdl_question_path,
      extension = question_source_ext,
      return_all = TRUE)
  
  doc_target_file =
    file.path(
      out_dir,
      sprintf(out_fmt, assignment_prefix, week_n))
  
  key_target_file =
    file.path(
      key_out_dir,
      sprintf(key_file_fmt, assignment_prefix, week_n, key_suffix))
  
  moodle_xml_target_file =
    file.path(
      moodle_xml_out_dir,
      sprintf(moodle_xml_out_fmt, assignment_prefix, week_n)
    )
  return(
    list(
      doc_source_file = doc_source_file,
      question_source_files = question_source_files,
      doc_target_file = doc_target_file,
      key_target_file = key_target_file,
      moodle_xml_target_file = moodle_xml_target_file)
  )
}

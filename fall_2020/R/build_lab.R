build_lab = function(
  week_n,
  assignment_prefix = "lab",
  out_dir = here::here("docs", "assignments", "eco_634"),
  answer_key = TRUE,
  moodle_xml = TRUE,
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
    
    week_n = 8
    assignment_prefix = "lab"
    out_dir = here::here(
      "docs", "assignments", "eco_634")
    
    require(rmd.utils)
    build_lab(7)
    answer_key = TRUE
    answer_key = FALSE
    moodle_xml = TRUE
    out_fmt = "%s_%0.2d"
    moodle_xml_out_fmt = "%s_%0.2d_moodle_questions"
    moodle_xml_out_dir = here::here(
      "docs", "moodle_quiz_questions")
    
    sprintf(out_fmt, assignment_prefix, week_n)
  }
  
  assignment_name = sprintf(out_fmt, assignment_prefix, week_n)
  
  a_f = gather_assignment_paths(
    week_n = week_n, 
    assignment_prefix = assignment_prefix,
    out_dir = out_dir,
  )
  
  key_target_file = NULL
  
  if(answer_key) key_target_file = a_f$key_target_file
  
  build_assignment_doc(
    doc_source_file = a_f$doc_source_file,
    question_source_files = a_f$question_source_files,
    doc_target_file = a_f$doc_target_file,
    key_target_file = key_target_file
  )
  
  if (moodle_xml)
  {
    build_moodle_xml_questions(
      question_source_files =
        a_f$question_source_files,
      assignment_name = "",
      xml_output_path = moodle_xml_out_dir,
      xml_output_filename =
        sprintf(
          moodle_xml_out_fmt,
          assignment_prefix, week_n)
    )
  }
}


build_reading_questions = function(
  week_n,
  assignment_prefix = "reading_questions",
  out_dir = here::here("docs", "assignments", "eco_602", "reading_questions"),
  answer_key = TRUE,
  moodle_xml = TRUE,
  moodle_xml_out_dir = here::here("docs", "moodle_quiz_questions"),
  out_fmt = "%s_week_%0.2d",
  source_dir_fmt = "%s_week_%0.2d_",
  source_file_fmt = "%s_week_%0.2d",
  key_out_dir = here::here("docs", "answer_keys"),
  key_suffix = "answer_key",
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
    
    out_dir = here::here(
      "docs", "assignments", "eco_602", "reading_questions")
    out_fmt = "%s_week_%0.2d"
    source_dir_fmt = "%s_week_%0.2d"
    source_file_fmt = "%s_week_%0.2d"
    source_search_path = NULL
    
    assignment_prefix = "reading_questions"
    
    key_file_fmt = "%s_%0.2d_answer_key"
    key_out_dir = here::here("docs", "answer_keys")
    
    moodle_question_source_subdir = "moodle"
    moodle_question_prefix = "Q_"
    moodle_xml_out_dir = here::here("docs", "moodle_quiz_questions")
    moodle_xml_out_fmt = "%s_%0.2d_moodle_questions"
    
    key_suffix = "answer_key"
    
    doc_source_ext = ".Rmd"
    question_source_ext = ".Rmd"
    
    build_reading_questions(3, answer_key = FALSE, moodle_xml = FALSE)
    
    week_n = 5
    
    require(rmd.utils)
    source(find_file("gather_assignment_paths.R"))
    
    
    answer_key = TRUE
    answer_key = FALSE
    moodle_xml = TRUE
    
    out_fmt = "%s_week_%0.2d"
    moodle_xml_out_fmt = "%s_%0.2d_moodle_questions"
    moodle_xml_out_dir = here::here("docs", "moodle_quiz_questions")
    # out_fmt = "602_week_%2$0.2d_%1$s"
    
    sprintf(out_fmt, assignment_prefix, week_n)
  }
  
  
  assignment_name = sprintf(out_fmt, assignment_prefix, week_n)
  
  a_f = gather_assignment_paths(
    week_n = week_n, 
    assignment_prefix = assignment_prefix,
    out_dir = out_dir,
    out_fmt = out_fmt,
    source_dir_fmt  = "%s_week_%0.2d",
    source_file_fmt = "%s_week_%0.2d"
  )
  
  file.path(
    out_dir,
    sprintf(out_fmt, assignment_prefix, week_n))
  
  key_target_file = NULL
  
  if(answer_key) key_target_file = a_f$key_target_file
  
  build_assignment_doc(
    doc_source_file = a_f$doc_source_file,
    question_source_files = a_f$question_source_files,
    doc_target_file = a_f$doc_target_file,
    key_target_file = key_target_file
  )
  
  
  if (moodle_xml)
  {
    # out_name = 
    # sprintf(moodle_xml_out_fmt, assignment_prefix, week_n)
    
    build_moodle_xml_questions(
      question_source_files = a_f$question_source_files,
      assignment_name = "",
      xml_output_path = moodle_xml_out_dir,
      xml_output_filename = sprintf(moodle_xml_out_fmt, assignment_prefix, week_n)
    )
    
    
    
  }
}


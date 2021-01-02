require(here)
require(exams)

source(list.files(path = here::here(), pattern = "rmd_functions.R", recursive = TRUE, full.names = TRUE))

# ---- assignment questions for web ----


# ---- Lab Assignments ----
{
  {
    build_moodle_questions(
      "week_00_objectives_assessment", 
      assignment_base_dir = file.path("assignments", "eco_634"),
      # question_numbers = 5,
      # separate_question_files = TRUE)
      separate_question_files = FALSE)
    
    build_moodle_questions(
      "week_01_r_foundations_1", 
      assignment_base_dir = file.path("assignments", "eco_634"),
      # question_numbers = 5,
      separate_question_files = TRUE)
    # separate_question_files = FALSE)
    # separate_question_files = TRUE)
    
    build_moodle_questions(
      "week_02_r_foundations_2", 
      assignment_base_dir = file.path("assignments", "eco_634"),
      # question_numbers = 5,
      # separate_question_files = TRUE)
      separate_question_files = FALSE)
    # separate_question_files = TRUE)
    
    
    build_moodle_questions(
      "lab_03_data_exploration_deterministic_functions", 
      assignment_base_dir = file.path("assignments", "eco_634"),
      # question_numbers = 5,
      separate_question_files = TRUE)
      # separate_question_files = FALSE)
    # separate_question_files = TRUE)

        build_moodle_questions(
      "lab_03_data_exploration_deterministic_functions", 
      assignment_base_dir = file.path("assignments", "eco_634"),
      # question_numbers = 5,
      # separate_question_files = TRUE)
      separate_question_files = FALSE)
    # separate_question_files = TRUE)
    
    
  }
  
  
  
  
}

# ---- Lecture assignments ----
{
  {
    build_moodle_questions(
      "week_01_data_camp_intro_to_r", 
      assignment_base_dir = file.path("assignments", "eco_602"),
      separate_question_files = FALSE)
    # question_numbers = 5,
    # separate_question_files = TRUE)    
    
  }
  {
    build_moodle_questions(
      "week_03_data_exploration_deterministic_functions", 
      assignment_base_dir = file.path("assignments", "eco_602"),
      separate_question_files = FALSE)
    # question_numbers = 5,
    # separate_question_files = TRUE)    
    
  }
  
}



# ---- Reading/lecture questions ----
{
  
  rq = function(
    week, sq = FALSE, 
    base_dir =
      file.path("assignments", "reading_questions")) {
    build_moodle_questions(
      sprintf("week_%0.2d", week),
      # sprintf("602_week_%0.2d_reading_questions", week),
      assignment_base_dir = base_dir,
      separate_question_files = sq)
  }
  
  rq(4)
  
  
  build_moodle_questions(
    "week_03", 
    assignment_base_dir = file.path("assignments", "reading_questions"),
    separate_question_files = FALSE)
  # question_numbers = 5,
  # separate_question_files = TRUE)    
  
}

# ---- in-class questions ----
{
  
  
  build_moodle_questions(
    "week_01_group_model_thinking", 
    assignment_base_dir = file.path("assignments", "in_class"),
    separate_question_files = FALSE)
  # question_numbers = 5,
  
}


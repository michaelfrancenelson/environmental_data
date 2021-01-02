build_moodle_xml_questions = function(
  assignment_name, 
  assignment_base_dir = "assignments", 
  moodle_source_subdir = "moodle",
  question_numbers = NA, 
  separate_question_files = FALSE,
  source_file_extension = ".Rmd")
{
  # source(here::here("rmd_tools", "find_file.R"))
  
  
  
  if(FALSE)
  {
    rm(list = ls())
  
    # assignment_rmd_filename = "week_03_data_exploration_deterministic_functions.Rmd"
    # 
    # assignment_name = "week_03_data_exploration_deterministic_functions.Rmd"
    # assignment_base_dir = "assignments"
    # moodle_source_subdir = "moodle"
    # 
    # assignment_base_dir = "assignments"
    # moodle_source_subdir = "moodle"
    
    # assignment_name = "week_00_objectives_assessment.Rmd"
    # assignment_name = "lab_00_objectives_assessment"
    
    assignment_name = "lab_00"
    assignment_base_dir = file.path("assignments", "eco_634")
    moodle_source_subdir = "moodle"
    separate_question_files = FALSE
    question_numbers = NA
    
    
    get_question_files("lab_00")
    get_question_files("lab_02")
    
    # question_numbers = 5
    # separate_question_files = TRUE)
  }
  paths = get_question_files(assignment_name, assignment_base_dir, moodle_source_subdir)
  
  question_basenames = tools::file_path_sans_ext(basename(paths$question_files))
  question_filenames = paths$question_files
  
  build_ex = function(f, name = NULL)
  {
    exams::exams2moodle(
      file = f,
      name = name,
      dir = paths$assignment_dir,
      edir = paths$exercise_dir,
      iname = FALSE,
      testid = TRUE,
      verbose = TRUE,
      mchoice = list(shuffle = TRUE),
      schoice = list(shuffle = TRUE))
  }
  
  
  if (separate_question_files)
  {
    for (i in 1:length(question_filenames))
    {
      build_ex(question_filenames[i], name = question_basenames[i])
    }
  } else {
    build_ex(question_filenames, name = assignment_name)
  }
}

get_question_files = function(
  assignment_rmd_filename,
  assignment_base_dir = "assignments",
  moodle_source_subdir = "moodle"
)
{
  
  if (FALSE)
  {
    assignment_rmd_filename = "week_03_data_exploration_deterministic_functions"
    assignment_rmd_filename = "lab_00_objectives_assessment"
    assignment_rmd_filename = "lab_00"
    
    assignment_rmd_filename = "lab_03"
    assignment_base_dir = "assignments"
    moodle_source_subdir = "moodle"
  }
  
  # source(here::here("rmd_tools", "find_file.R"))
  # assignment_base = gsub(".Rmd", "", assignment_rmd_filename)
  
  potential_dirs = list.files(
    path = here::here(assignment_base_dir),
    # path = here::here(assignment_base_dir),
    pattern = assignment_rmd_filename, 
    # pattern = assignment_base, 
    recursive = TRUE, 
    include.dirs = TRUE, 
    full.names = TRUE,
  )
  # potential_dirs = find_file(assignment_rmd_filename, return_all = TRUE)
  
  assignment_base = tools::file_path_sans_ext(assignment_rmd_filename)
  
  # Exclude filename matches - we are only interested in matching a directory name
  potential_dirs = potential_dirs[dir.exists(potential_dirs)]
  # potential_dirs = potential_dirs[sapply(potential_dirs, function(p) identical(basename(p), assignment_base))]
  
  if (length(potential_dirs) == 0)
    cat(sprintf("No matching folder for assignment '%1$s' found...", assignment_rmd_filename))
    # cat(sprintf("No assignment folder called '%1$s' found...", assignment_base))
  
  if (length(potential_dirs) > 1)
  {
    cat("Duplicate assignment folders found:\n")
    for (i in 1:length(potential_dirs)) cat(sprintf("%s: %s\n", i, potential_dirs[i]))
    cat(sprintf("Try using a different assignment base directory to limit duplicates", assignment_base))
  }
  stopifnot(length(potential_dirs) == 1)
  
  assign_dir = potential_dirs
  
  cat(sprintf("Assignment folder '%1$s' found at location:\n     '%2$s'", assignment_base, assign_dir))
  
  exercise_dir = file.path(assign_dir, moodle_source_subdir)
  question_files = list.files(path = exercise_dir, pattern = ".Rmd", full.names = TRUE)
  
  return(list(
    question_files = question_files, 
    assignment_dir = assign_dir, 
    exercise_dir = exercise_dir))
}

if (!("here" %in% library()$results)) 
  cat("\nPackage 'here' is required for moodle question tools")

source(here::here("tools", "find_file.R"))

build_web_questions = function(
  assignment_filename,
  out_filename = NULL,
  assignment_base_dir = "assignments",
  moodle_source_subdir = "moodle",
  dir_out = here::here("docs"),
  write_html = TRUE,
  include_header = TRUE)
{
  
  # ---- testing arguments ----  
  if(FALSE)
  {
    dir_out = "docs"
    out_filename = NULL
    write_html = TRUE
    assignment_base_dir = "assignments"
    moodle_source_subdir = "moodle"
    include_header = TRUE
    assignment_filename = "week_03_data_exploration_deterministic_functions"
    include_header = FALSE
    
    build_web_questions(assignment_filename = assignment_filename, write_html = FALSE)
  }
  
  if (!("here" %in% library()$results)) 
    cat("\nPackage 'here' is required for function 'build_web_questions()'")
  
  # Attempt to locate the directory holding the assignment files
  question_paths = 
    get_question_source_files(
      assignment_filename, 
      assignment_base_dir, 
      moodle_source_subdir)
  
  question_files = question_paths$question_files
  question_markdown_header = "# Question %0.2d"
  
  # Use the markdown header from the first question for the entire question set:
  header_lines = get_rmd_header(question_files[1])
  header_lines = substitute_title(header_lines, paste0("Questions for assignment ", assignment_filename))
  
  out = ifelse(
    include_header,
    header_lines,
    c())
  if (include_header) out = header_lines else out = c()
  
  for (i in 1:length(question_files))
  {
    file_lines = readLines(question_files[i])
    
    q_title = get_rmd_title(file_lines = file_lines)
    
    out = c(
      out,      
      c(
        sprintf(fmt = question_markdown_header, i),
        q_title,
        # Remove duplicated CSS chunk names
        gsub("r CSS", "r", get_question_body(question_files[i]))
      )
    )
  }
  
  if (include_header) out = c(header_lines, out)
  
  if (write_html)
  {
    out_filename = 
      ifelse(
        is.null(out_filename),
        paste0(assignment_filename, "_questions"),
        out_filename)
    cat(sprintf("Writing questions to file %s: ", out_filename))
    
    tmp_stem = paste0(sample(letters, 15, replace = TRUE), collapse = "")
    tmp_file = file.path(dir_out, paste0(tmp_stem, ".Rmd"))
    writeLines(out, tmp_file)
    build_doc(file_stem = tmp_stem, dir_out = dir_out, filename_out = out_filename)
    file.remove(tmp_file)
  }
  invisible(out)
}

substitute_title = function(header_lines, new_title, title_prefix = "title:")
{
  title_line = which(grepl(title_prefix, header_lines))
  header_lines[title_line] = sprintf("%s %s", title_prefix, new_title)
  return(header_lines)
}


get_html_title = function(filename, title_node = "title")
{
  require(rvest)
  file_html = read_html(filename)
  return(html_text(html_node(file_html, title_node)))
  
  if (FALSE)
  {
    library(rvest)
    movie <- read_html("https://en.wikipedia.org/wiki/The_Lego_Movie")
    cast <- html_nodes(movie, "tr:nth-child(8) .plainlist a")
    html_text(html_node(movie, "title"))
    html_text(cast)
    html_name(cast)
    html_attrs(cast)
    html_attr(cast, "href")
    
  }
}

get_rmd_title = function(
  filename, file_lines = NULL, 
  title_prefix = "title:",
  yaml_header_delimiter = "----")
{
  if (FALSE)
  {
    file_lines = NULL
    filename = "C:/Users/michaelnelso/git/eco_602_634_2020/assignments/eco_602/week_03_data_exploration_deterministic_functions/moodle/Q1_histograms_elevation.Rmd"
    file_lines = readLines(filename)
    title_prefix = "title:"
  }
  
  if (is.null(file_lines)) file_lines = readLines(filename)
  
  header_lines = get_rmd_header(NULL, file_lines = file_lines)
  title_line = header_lines[grepl(title_prefix, header_lines)]
  title = gsub("\"", "", trimws(gsub(title_prefix, "", title_line)))
  return(title)
}

get_question_body = function(
  filename, 
  file_lines = NULL, 
  q_header = "Question", 
  delimiter = "========", 
  sol_header = "Solution")
{
  if (FALSE)
  {
    q_header = "Question"
    delimiter = "========" 
    sol_header = "Solution"
    
    file_lines = NULL
    filename = question_files[1]
  }
  
  if (is.null(file_lines)) file_lines = readLines(filename)
  
  # Find adjacent lines matching the `exams` package question and solution section delimiters
  delimiter_lines = which(grepl(delimiter, file_lines))
  question_lines = which(grepl(q_header, file_lines))
  soln_lines = which(grepl(sol_header, file_lines))
  
  q_line = question_lines[question_lines %in% (delimiter_lines - 1)]
  s_line = soln_lines[ soln_lines %in% (delimiter_lines - 1)]
  
  if (length(q_line) != 1 | length(s_line) != 1)
    cat(sprintf(
      "Could not locate the Moodle Question and Solution delimiters in file: %s",
      filename))
  return(file_lines[(q_line + 2) : (s_line - 1)])
}



# Find moodle quiz question source files associated
# with an assignment.
# Searches for the source Rmd file for the assignment
# to locate individual moodle quiz question files.
get_question_source_files = function(
  assignment_rmd_filename,
  assignment_base_dir = "assignments",
  moodle_question_subdir = "moodle",
  search_path = NULL
)
{
  if (FALSE)
  {
    assignment_base_dir = file.path("assignments", "eco_602")
    moodle_question_subdir = "moodle"
    assignment_rmd_filename = "week_03_data_exploration_deterministic_functions.Rmd"
    search_path = here::here("assignments", "eco_602")
  }
  
  assn_file_basename = tools::file_path_sans_ext(assignment_rmd_filename)
  
  rmd_path = find_file(
    assignment_rmd_filename,
    search_path = search_path)
  
  question_path = 
    file.path(dirname(rmd_path), moodle_question_subdir)
  stopifnot(dir.exists(question_path))
  
  source_files = find_file(
    filename = ".Rmd", 
    search_path = question_path, 
    return_all = TRUE, 
    duplicated_files_error = FALSE)
    
  return(list(
    question_source_files = source_files, 
    assignment_dir = dirname(rmd_path),
    questions_dir = question_path))  
}





build_moodle_questions_xml = function(
  assignment_name, 
  assignment_base_dir = "assignments", 
  moodle_source_subdir = "moodle",
  question_numbers = NA, 
  separate_question_files = FALSE)
{
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

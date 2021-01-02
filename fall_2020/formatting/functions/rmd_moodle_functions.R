require(here)

build_web_questions = function(
  assignment_filename,
  out_filename = NULL,
  assignment_base_dir = "assignments",
  moodle_source_subdir = "moodle",
  dir_out = here::here("docs"),
  write_html = TRUE,
  include_header = TRUE)
{

  # ---- testint arguments ----
  if(FALSE)
  {
    dir_out = "docs"
    out_filename = NULL
    write_html = TRUE
    assignment_base_dir = "assignments"
    moodle_source_subdir = "moodle"
    include_header = TRUE
    # assignment_filename = "week_03_data_exploration_deterministic_functions"
    assignment_filename = "week_03_data_exploration_determinsitc_functions.Rmd"
    include_header = FALSE

    build_web_questions(assignment_filename = assignment_filename, write_html = FALSE)
  }

  # Attempt to locate the directory holding the assignment files
  question_paths =
    get_question_files(
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

# get_question_files = function(
#   assignment_rmd_filename,
#   assignment_base_dir = "assignments",
#   moodle_source_subdir = "moodle"
# )
# {
#   if (FALSE)
#   {
#     assignment_rmd_filename = "week_03_data_exploration_deterministic_functions.Rmd"
#     assignment_base_dir = "assignments"
#     moodle_source_subdir = "moodle"
#   }
#
#   assignment_name = gsub(".Rmd", "", assignment_rmd_filename)
#
#   potential_dirs = list.files(
#     path = here::here(assignment_base_dir),
#     pattern = assignment_name,
#     recursive = TRUE,
#     include.dirs = TRUE,
#     full.names = TRUE,
#   )
#
#   # Exclude filename matches - we are only interested in matching a directory name
#   potential_dirs = potential_dirs[dir.exists(potential_dirs)]
#   potential_dirs = potential_dirs[sapply(potential_dirs, function(p) identical(basename(p), assignment_name))]
#
#   if (length(potential_dirs) == 0)
#     cat(sprintf("No assignment folder called '%1$s' found...", assignment_name))
#
#   if (length(potential_dirs) > 1)
#   {
#     cat("Duplicate assignment folders found:\n")
#     for (i in 1:length(potential_dirs)) cat(sprintf("%s: %s\n", i, potential_dirs[i]))
#     cat(sprintf("Try using a different assignment base directory to limit duplicates", assignment_name))
#   }
#   stopifnot(length(potential_dirs) == 1)
#
#   assign_dir = potential_dirs
#
#   cat(sprintf("Assignment folder '%1$s' found at location:\n     '%2$s'", assignment_name, assign_dir))
#
#   exercise_dir = file.path(assign_dir, moodle_source_subdir)
#   question_files = list.files(path = exercise_dir, pattern = ".Rmd", full.names = TRUE)
#
#   return(list(question_files = question_files, assignment_dir = assign_dir, exercise_dir = exercise_dir))
# }


build_moodle_questions = function(
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


build_assignment = function(file_stem, file_prefix = NULL, assignment_dir = here::here("docs", "assignments"))
{
  file_in = paste0(file_stem, ".Rmd")
  assignment_rmd = list.files(path = here::here(), pattern = file_in, recursive = TRUE, full.names = TRUE)

  # Make sure the file is found and that there is no duplicate assignment source
  if (length(assignment_rmd) == 0)
    cat(sprintf("Assignment source file '%s' not found.", file_in))

  if (length(assignment_rmd) > 1)
  {
    cat(sprintf("Multiple assignment source files with name '%s' found.", file_in))
  }

  stopifnot(length(assignment_rmd) == 1)

  cat(sprintf("Assignment source file found:%s", assignment_rmd))

  file_out = ifelse(
    is.null(file_prefix),
    sprintf("%2$s.html", file_prefix, file_stem),
    sprintf("%1$s_%2$s.html", file_prefix, file_stem)
  )

  rmarkdown::render(
    input = assignment_rmd,
    output_file = file_out,
    output_dir = assignment_dir)
}

build_web_questions_ = function(
  assignment_filename,
  out_filename = NULL,
  assignment_base_dir = "assignments",
  moodle_source_subdir = "moodle",
  include_header = TRUE)
{
  if(FALSE)
  {
    # assignment_filename = "week_02_r_foundations_2"
    assignment_filename = "lab_02_r_fundamentals_2"
    assignment_base_dir = "assignments"

    assignment_filename = "week_02"

    assignment_base_dir = file.path("assignments", "eco_602")
    assignment_filename = "week_01_data_camp_intro_to_r"

    assignment_base_dir = file.path("assignments", "eco_602")
    assignment_filename = "week_03_data_exploration_deterministic_functions"

    assignment_base_dir = file.path("assignments", "eco_634")
    assignment_filename = "lab_03"


    moodle_source_subdir = "moodle"
    file_out = here::here("test.Rmd")

    build_web_questions(assignment_filename, out_filename = file_out, assignment_base_dir = assignment_base_dir, moodle_source_subdir = moodle_source_subdir)
  }

  question_paths =
    get_question_files(
      assignment_filename,
      assignment_base_dir,
      moodle_source_subdir)

  question_files = question_paths$question_files
  question_markdown_header = "# Question %0.2d"
  document_header = get_rmd_header(question_files[1])
  document_lines = c()

  for (i in 1:length(question_files))
  {
    document_lines = c(
      document_lines,
      c(
        sprintf(fmt = question_markdown_header, i),
        gsub("r CSS", "r", get_question_body(question_files[i]))
      )
    )
  }
  if (include_header) document_lines = c(document_header, document_lines)
  if (!is.null(out_filename)) writeLines(document_lines, out_filename)
  invisible(document_lines)
}

#'
#'
#' @export

add_moodle_quiz_questions = function(
  question_source_files,
  tmp_dir = here::here(),
  tmp_prefix = NULL,
  include_solution = FALSE,
  include_metadata = FALSE)
{

  if (FALSE)
  {
    tmp_dir = here::here()
    include_solution = FALSE
    include_metadata = FALSE
    tmp_prefix = NULL

    question_source_files = find_file("Q",extension = ".Rmd", search_path = find_file("lab_05", directory = TRUE), return_all = TRUE)

    add_moodle_quiz_questions(question_source_files[1])

  }

  if (is.null(tmp_prefix))
  {
    tmp_fmt = file.path(tmp_dir, "moodle_quiz_q_%0.2d.Rmd")
  } else
  {
    tmp_fmt = file.path(tmp_dir, paste0(tmp_prefix, "_q_%0.2d.Rmd"))
  }

  
  
  
  question_file_lines = format_moodle_web_questions(question_source_files)

  n_q = length(question_file_lines)
  tmp_files = sprintf(tmp_fmt, 1:n_q)

  child_chunk_fmt = "```{r child='%s', echo=FALSE, include=FALSE}\n```\n\n"
  tmp_rm_chunk_fmt = "```{r echo=FALSE, results = 'hide'}\nfile.remove(\"%s\")\n```\n\n"

  for (i in 1:n_q)
  {
    writeLines(question_file_lines[[i]], tmp_files[i])
    cat(sprintf(child_chunk_fmt, tmp_files[i]))
    cat(sprintf(tmp_rm_chunk_fmt, tmp_files[i]))
  }

}



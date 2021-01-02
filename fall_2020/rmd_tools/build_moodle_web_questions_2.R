  # ---- testing arguments ----  
  if(FALSE)
  {
    assignment_filename = "week_03_data_exploration_deterministic_functions.Rmd"
    assignment_filename = "week_03_data_exploration_deterministic_functions.Rmd"
    out_filename = NULL
    search_path = here::here("assignments", "eco_602")
    moodle_source_subdir = "moodle"
    dir_out = here::here("docs")
    write_html = FALSE
    include_header = TRUE
    rmd_tools_dir = "rmd_tools"  
    question_number_fmt = "## Question %1$s: %2$s"
    
    assignment_name = NULL
    assignment_name = "Data Exploration and Determinsitic Functions"
    build_answer_key = TRUE
    
    assignment_filename = "lab_04_uncertainty_and_error.Rmd"
    search_path = here::here("assignments", "eco_634")
    include_header = FALSE
    write_html = FALSE
    cat_results = FALSE
    write_tmp = TRUE
    tmp_filename = NA
    build_answer_key = TRUE
    include_metadata = TRUE
    build_answer_key = FALSE
    include_metadata = FALSE
    
    
    
    
    rmd_tools_dir = "rmd_tools"  
    
    
    # include_header = FALSE
    if (FALSE)
      build_moodle_web_questions(
        assignment_filename = assignment_filename,
        search_path = search_path, write_html = FALSE)
  }
  
  if (!("here" %in% library()$results)) 
    cat("\nPackage 'here' is required for 'build_moodle_web_questions()'")
  
  # ---- required functions ----
  {
    depends_on = c(
      "find_file",
      # "get_rmd_header",
      # "get_rmd_header_attr",
      # "substitute_rmd_header_attr",
      "get_moodle_question_source_files",
      # "get_moodle_question_body",
      # "build_html_doc"
    )
    
    for (d in depends_on)
    {
      if (!(d %in% ls()))
        source(here::here(rmd_tools_dir, paste0(d, ".R")))
    }
  }  
  
  # Set assignment name
  if (is.null(assignment_name)) assignment_name = basename(tools::file_path_sans_ext(assignment_filename))
  
  
  
  
  
  # Attempt to locate the question source files
question_source_files = 
    get_moodle_question_source_files(
      assignment_filename, 
      search_path = search_path, 
      moodle_source_subdir)
  
  
  # assignment_filename,
  # out_filename = NULL,
  # search_path = "assignments",
  # assignment_name = NULL,
  # assignment_base_dir = "assignments",
  # moodle_source_subdir = "moodle",
  # dir_out = here::here("docs"),
  # write_html = TRUE,
  # include_header = TRUE,
  rmd_tools_dir = "rmd_tools",
  # question_number_fmt = "## Question %1$s: %2$s",
  # cat_results = FALSE,
  # write_tmp = FALSE,
  build_answer_key = FALSE,
  include_metadata = FALSE,
  
  

#   
#   
#   
#   
#   
#   i = 1
#   source_lines_i[[i]]
#   format_question_source(source_lines[[i]])
#   format_question_source(source_lines[[i]], build_answer_key = build_answer_key, include_metadata = include_metadata)
#   format_question_source(source_lines[[i]], build_answer_key = TRUE)
#   
#   
#   
#   
#   
#   if (!include_metadata)
#     
#     
#     source_lines_2 = lapply(source_lines, function(x) trim_moodle_question_section(x, section_name = "Meta-information", allow_final = ))
#   
#   
#   cat(source_lines_2[[1]], sep = "\n")
#   
#   
#   
#   header_lines = get_rmd_header(question_source_files$question_source_files[1])
#   header_lines = substitute_rmd_header_attr(
#     header_lines, attr_prefix = "title:",
#     new_attr = assignment_name)
#   
#   out_body = c()
#   
#   for (i in 1:length(question_source_files$question_source_files))
#   {
#     file_lines = readLines(question_source_files$question_source_files[i])
#     
#     q_title = get_rmd_header_attr(file_lines = file_lines, header_prefix = "title:")
#     title_line = sprintf(question_number_fmt, i, q_title)
#     
#     q_body = get_moodle_question_body(question_source_files$question_source_files[i])
#     
#     # Remove duplicated CSS chunk names
#     q_body = gsub("r CSS", "r", q_body)
#     
#     q_i = c(title_line, q_body)
#     
#     if (build_answer_key)
#     {
#       q_sol = get_moodle_question_body(
#         question_source_files$question_source_files[i],
#         start_header = "Solution", end_header = "Meta-information")
#       
#       q_i = c(q_i,'<div class="redborder">', "### Solution\n", q_sol, "", "</div>\n")
#     }
#     
#     out_body = c(out_body, q_i)
#   }
#   
#   if (include_header) out = header_lines else out = c()
#   
#   if (cat_results)
#     cat(header_lines, out_body, sep = "\n")
#   
#   out = c(out, out_body)
#   
#   if (write_tmp)
#   {
#     tmp = tempfile(tmpdir = here::here(), fileext = ".Rmd")
#     writeLines(out, tmp)
#   }
#   
#   if (write_tmp) return(tmp)
#   
#   invisible(out)
# }


format_question_source = function(lines_i, build_answer_key = FALSE, include_metadata = FALSE)
{
  
  if (FALSE)
  {
    lines_i = source_lines[[1]]
  }
  
  metadata_line_indices = get_moodle_question_section_line_indices(lines_i)
  solution_line_indices = get_moodle_question_section_line_indices(lines_i, section_name = "Solution")
  
  metadata = ""
  solution = ""
  
  if (include_metadata)
  {
    #Insert line breaks into metadata section
    metadata = insert_line_breaks(lines_i[metadata_lines])
    
  }
  
  if (build_answer_key)
  {
    solution = lines_i[solution_lines]
  }
  
  lines_out = c(lines_i[-c(metadata_line_indices, solution_line_indices)], solution, metadata)
  return(lines_out)
}




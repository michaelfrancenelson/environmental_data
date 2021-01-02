build_html_doc = function(
  rmd_source_filename,
  output_dir = here::here(),
  search_path = NULL,
  filename_out = NULL,
  type = NULL,
  html_ext = ".html")
{
  
  # ---- testint arguments ----
  if (FALSE)
  {
    output_dir = here::here()
    html_ext = ".html"

    
    rmd_source_filename = "week_03_data_exploration_deterministic_functions.Rmd"
    search_path = here::here("assignments", "eco_602")
    
    rmd_source_filename = here::here("assignments/eco_602/week_03_data_exploration_deterministic_functions/week_03_data_exploration_deterministic_functions.Rmd")
    search_path = NULL
    
    filename_out = NULL
  }
  
  source_filename = ifelse(
    is.null(search_path),
    rmd_source_filename,
    find_file(
      filename = rmd_source_filename, 
      search_path = search_path, 
      return_all = FALSE, 
      duplicated_files_error = TRUE)
  )
  
  out_filename = 
    ifelse(
      is.null(out_filename),
      paste0(basename(tools::file_path_sans_ext(source_filename)), html_ext),
      out_filename)
  
  rmarkdown::render(
    input = source_filename, 
    output_file = out_filename,
    output_dir = output_dir,
    output_format = "html_document") 

  return(TRUE)
}
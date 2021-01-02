
make_directory_links = 
  function(
    directory, files, 
    get_title_from_source = TRUE,
    source_search_path = NULL, 
    source_extension = ".Rmd", 
    number_entries = FALSE, 
    number_prefix = "")
  {
    require(rmd.utils)
    nl = "\n"
    if (FALSE)
    {
      
      files = list.files(here::here("docs", "lecture_notes"))
      files = list.files(here::here("docs", "lecture_notes"))
      
      files = 
        list.files(here::here(
          "docs", "assignments", "eco_602", "reading_questions"),
          full.names = FALSE)
      
      directory = here::here(
        "docs", "assignments", "eco_602", "reading_questions")
      
      
      source_search_path = here::here("assginments")
      
      source_search_path = NULL
      get_title_from_source = TRUE
      
      source_extension = ".Rmd"
      number_entries = FALSE
      number_prefix = ""
      
      get_title_from_source = FALSE
      
      i = 3
      i = 2
      i = length(files)
    }
    out = c("<ul>")
    
    cat("<ul>")
    for (i in 1:length(files))
    {
      f = files[i]
      fname = tit = basename(f)
      
      f_path = file.path(directory, fname)
      
      if (get_title_from_source)
      {
        f_base = basename(tools::file_path_sans_ext(f))
        
        hush = function(code)
        {
          sink("NULL") # use /dev/null in UNIX
          tmp = code
          sink()
          return(tmp)
        }
        
        
        source_file =
          hush(
            find_file(
              paste0(f_base, ".Rmd"),
              extension = ".Rmd",
              error_if_none = FALSE,
              exact_match = TRUE)
          )
        
        if (!is.null(source_file))
          # if (!is.null(source_file))
        {
          tit = get_rmd_header_attr(source_file, header_prefix = "subtitle:")
        }
      }
      
      if (number_entries) tit = paste0(number_prefix, i, " : ", tit)
      
      link_i = sprintf(fmt = '<li><a href="%1$s/%2$s " target="_blank">%3$s</a></li>',
                       directory, fname, tit)
      
      cat(link_i)
      # cat("\n")
      cat(nl)
      
      out = c(out, link_i)
    }
    out = c(out, "</ul>")
    cat("</ul>")
    return(out)
  }

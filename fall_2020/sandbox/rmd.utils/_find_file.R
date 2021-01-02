
if(FALSE)
{
  filename = "lab_01_r_fundamentals_1"
  search_path = NULL
  return_all = FALSE
  duplicated_files_error = FALSE
  verbose=FALSE
  directory = FALSE
  extension = ".Rmd"
}

find_file = function(
  filename, 
  search_path = NULL,
  return_all = FALSE, 
  duplicated_files_error = FALSE,
  verbose=FALSE,
  directory = FALSE,
  extension = NULL,
  null_if_none = FALSE)
{
  
  if (!("here" %in% library()$results)) 
    cat("\nPackage 'here' is required for function 'find_file()'")
  require(here)
  
  if (is.null(search_path))  search_path = here::here()
  
  if (directory)
  {
    matches =
      list.files(
        path = search_path, 
        pattern = filename,
        recursive = TRUE, 
        full.names = TRUE,
        include.dirs = TRUE)  
    
    matches = matches[dir.exists(matches)]
  } else
    matches = 
      list.files(
        path = search_path, 
        pattern = filename, 
        recursive = TRUE, 
        full.names = TRUE)
  
  if (!is.null(extension))
    matches = matches[grepl(extension, matches)]
  
  if (length(matches) == 0) 
  {
      if(verbose)
    cat(sprintf("File '%s' not found", filename))
    
    if (!null_if_none)
      stopifnot(length(matches) > 0) else
        return(NULL)
  }
  
  if (length(matches) > 1)
  {
    if(verbose)
      cat(sprintf(
        "Warning: duplicate files matching filename: '%1$s' were found:",
        filename))
    sapply(matches, function(f) cat(sprintf("\n%s", f)))
    
    if (duplicated_files_error & !return_all)
    {
      if(verbose)
        cat("\nError: Duplicate files were found with option 'duplicated_files_error = TRUE'")
      stopifnot(length(matches) == 1)
    }
  }
  
  if (return_all) return(matches)
  
  return(matches[1])
}

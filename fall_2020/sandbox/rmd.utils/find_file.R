#' Find a file
#'
#' Locate a file somewhere within the directory structure of an RProject.
#'
#'
#' @param filename Name of the file to search for
#' @param search_path Optional path within which to search
#' @param return_all Return all matches?
#' @param duplicated_files_error Throw an error if more than one matching file found?
#' @param verbose Print messages?
#' @param directory Search for a directory instead of a file?
#' @param extension Search for files with this extension only?
#' @param error_if_none Throw an error if no matches are found?  If FALSE, the function returns a NULL value.
#'
#' @return The absolute path to the file, if it was found.
#' @export

find_file = function(
  filename,
  search_path = NULL,
  return_all = FALSE,
  duplicated_files_error = FALSE,
  verbose=FALSE,
  directory = FALSE,
  extension = NULL,
  error_if_none = TRUE)
{

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


  # If no matches are found:
  if (length(matches) == 0)
  {
    if(verbose) print(sprintf("File '%s' not found", filename))

    # if a file not found error is requested (the default)
    if (error_if_none)
    {
      stopifnot(length(matches) > 0)
    }
    return(NULL)
  }


  # Check for matching file extension
  if (!is.null(extension))
    matches = matches[grepl(extension, matches)]


  # If duplicate matches are found:
  if (length(matches) > 1)
  {
    if(verbose)
    {
      print(sprintf(
        "Duplicate files matching filename: '%1$s' were found:",
        filename))
      sapply(matches, function(f) print(sprintf("%s", f)))
    }
    # if(verbose)
    #   cat("\nError: Duplicate files were found with option 'duplicated_files_error = TRUE'")

    if (duplicated_files_error)
    {
      print("Duplicate files found")
      sapply(matches, function(f) print(sprintf("%s", f)))
      stopifnot(length(matches) == 1)
    }

    if (return_all) return(matches)
  }

  return(matches[1])
}

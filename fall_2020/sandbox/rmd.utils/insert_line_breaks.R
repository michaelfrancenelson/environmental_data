#' Insert newline characters between lines
#'
#' Adds a new line character in between lines contained in a character vector
#'
#' @param input_lines lines in-between which to insert newline characters
#' @param lb new line character to insert
#'
#' @export


insert_line_breaks = function(input_lines, lb = "\n")
{
  if (FALSE)
  {
    input_lines = input_lines[[1]][metadata_line_indices]
  }

  l = length(input_lines)

  2 * (1:l) - 1
  2 * (1:(l - 1))

  lines_out =
    vector(
      mode = "character",
      length = 2 * l)

  insertion_indices =
    2 * (1:l)

  original_entity_indices =
    2 * (1:l) - 1

  lines_out[insertion_indices] = "\n"
  lines_out[original_entity_indices] = input_lines

  return(lines_out)
}

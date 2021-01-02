require(here)
require(rvest)

source(here::here("rmd_tools", "find_file.R"))

source(here::here("formatting", "functions", "rmd_moodle_functions.R"))

find_file_exists = function(filename, error = FALSE)
{
  matching_files = list.files(path = here::here(), pattern = filename, recursive = TRUE, full.names = TRUE)
  if (length(matching_files) == 0)
    if (error) stopifnot(FALSE) else return(NULL)
  return(matching_files[1])
}




# find_file = function(filename)
# {
#   matching_files = list.files(path = here::here(), pattern = filename, recursive = TRUE, full.names = TRUE)
#
#   if (length(matching_files) == 0) cat(sprintf("File '%s' not found", filename))
#   stopifnot(length(matching_files) > 0)
#
#   return(matching_files[1])
# }

# for compatability with older scripts
find_img = function(filename) { find_file(filename) }

colorize <- function(x, color) {
  if (knitr::is_latex_output()) {
    sprintf("\\textcolor{%s}{%s}", color, x)
  } else if (knitr::is_html_output()) {
    sprintf("<span style='color: %s;'>%s</span>", color, x)
  } else x
}

bold_col <- function(x, color) {
  if (knitr::is_latex_output()) {
    sprintf("\\textbf{\\textcolor{%s}{%s}}", color, x)
  } else if (knitr::is_html_output()) {
    sprintf("<span style='color: %s;'>%s</span>", color, x)
  } else x
}

html_output_only = function(x)
{
  if (knitr::is_html_output()) {
    sprintf("<span>%s</span>", x)
  }
}

expandable_html_image = function(
  filename, click_message = "[Click to expand image]",
  thumb_width = 240, img_width = 60,
  cat_result = TRUE,
  moodle_quiz = FALSE, bg_alpha = 0.4)
{
  if (FALSE)
  {
    # filename = "github_desktop_sign_in.PNG"
    # filename = "github_desktop_sign_in.PNG"
    filename = "mfn_github_profile_arrow.png"
    #
    click_message = "[Click to expand image]"
    #
    # img_width = 70
    img_width = 29
    # img_width = NULL
    thumb_width = 200
    # expandable_html_image(filename, click_message, img_width)
    expandable_html_image("mfn_github_profile_arrow.png")
    expandable_html_image("mfn_github_profile_arrow.png", img_width = 29)
  }

  exams::include_supplement(filename, dir = here::here(), recursive = TRUE)

  href_image = paste0("img", sample(9999999, 1))
  href_close = paste0(sample(letters, 12), collapse = "")
  rmd_filename = sprintf(fmt = '![](%s)', filename)

  html_fmt = paste(
    '\n<a href="#%4$s"></a>',
    '<a class="thumb%6$s" href="#%3$s">',
    '  <figure>',
    '%2$s',
    '    <figcaption>%1$s</figcaption>',
    '  </figure>',
    '</a>',
    '<div class="lb%5$s" id="%3$s">',
    '%2$s',
    '  <a class="lb-close%5$s" href="#%4$s"></a>',
    '</div>\n',
    sep = "\n")

  html_out = sprintf(
    html_fmt,
    click_message,
    rmd_filename,
    href_image,
    href_close,
    img_width,
    thumb_width)

  # format text numberings:
  # 1  click message
  # 2  image file name, formatted for markdown image inclusion
  # 3  href code for image
  # 4  href code for close
  # 5  lightbox image width
  # 6  thumb width

  lb_out = build_lightbox(img_width, moodle_question = moodle_quiz, bg_alpha = bg_alpha)
  thumb_out = build_thumb(thumb_width)
  out = paste("\n<style>\n", thumb_out, lb_out, "\n</style>\n", html_out)

  if(cat_result)
    cat(out)
  invisible(out)
}


build_thumb = function(thumb_width, cat_result = TRUE)
{
  thumb = sprintf(
    paste(
      '\n.thumb%1$s img {',
      'width: %1$spx;',
      'border: 4px solid green;',
      'box-shadow: 0px 0px 1.5vw rgba(0,0,0,1);',
      '}\n', sep = "\n"),
    thumb_width)

  return(thumb)
}

build_lightbox = function(img_width, moodle_question = FALSE, bg_alpha = 0.4)
{

  lb_pos = ifelse(
    moodle_question,
    'left: 0;',
    'right: 0; left: 0; bottom: 0; top: 0;'
  )

  lb = sprintf(
    fmt = paste(
      '\n.lb%2$s {',
      'position: fixed;',
      'top: -100%1$s;',
      'width: 100%1$s;',
      'background: rgba(0,0,0,%3$s);',
      # 'border: 1px solid green;',
      # 'background: rgba(0,0,0,0.4);',
      '}\n', sep = "\n"),
    "%", img_width, bg_alpha)

  lb_fmt = paste(
    '\n.lb%2$s img {',
    'margin: auto;',
    'position: absolute; object-fit: contain;',
    '%3$s',
    'border: 5px solid blue;',
    # 'right: 0; left: 0; bottom: 0; top: 0;',
    # 'left: 0;',
    'width: %2$s%1$s;',
    '}\n', sep = "\n")

  lb_img = sprintf(
    fmt = lb_fmt,
    "%",
    img_width,
    lb_pos)

  lb_close = sprintf(
    fmt = paste(
      '\n.lb-close%2$s {',
      'display: block; position: absolute;',
      'width: 100%1$s; height: 100%1$s;',
      '}\n', sep = "\n"),
    "%", img_width)

  lb_target = sprintf(
    fmt = paste(
      '\n.lb%2$s:target {',
      'opacity: 1; top: 0; bottom: 0;',
      'border: 3px solid green;',
      '}\n', sep = "\n"),
    "%", img_width)
  out = paste("\n", lb, lb_img, lb_close, lb_target)
  return(out)
}

build_popup = function(filename, thumb_width = 250, cat_output = TRUE, recursive = FALSE)
{
  # Check whether units were given for the thumbnail width.  If not, default to pixels.
  suppressWarnings(
    {
      thumb_width = ifelse(
        is.na(as.numeric(thumb_width)),
        thumb_width,
        paste0(thumb_width, "px"))

    }
  )

  # option to search project path for the file.  This doesn't work well with moodle quiz questions.
  if (recursive)
  {
    candidate_files = list.files(path = here::here(), pattern = filename, recursive = TRUE, full.names = TRUE)
    stopifnot(length(candidate_files) > 0)
    filename = candidate_files[1]
  }

  fmt_popup =
    '<a target="_blank" href="%1$s"><img src="%1$s" style="width:%2$s"></a>'
  out_popup = sprintf(fmt_popup, filename, thumb_width)
  if(cat_output) cat(out_popup)
  invisible(out_popup)
}

build_popup_figure = function(filename, thumb_width = 250, caption = "[click to embiggen]", cat_output = TRUE, recursive = FALSE)
{
  # Check whether units were given for the thumbnail width.  If not, default to pixels.
  suppressWarnings(
    {
      thumb_width = ifelse(
        is.na(as.numeric(thumb_width)),
        thumb_width,
        paste0(thumb_width, "px"))
    }
  )

  # # option to search project path for the file.  This doesn't work well with moodle quiz questions.
  # if (recursive)
  # {
  #   candidate_files = list.files(path = here::here(), pattern = filename, recursive = TRUE, full.names = TRUE)
  #   stopifnot(length(candidate_files) > 0)
  #   filename = candidate_files[1]
  # }
  #
  exams::include_supplement(filename, dir = here::here(), recursive = TRUE)


  fmt_popup =
    '<a target="_blank" href="%1$s"><figure ><img src="%1$s" style="width:%2$s"><figcaption>%3$s</figcaption></figure></a>'
  out_popup = sprintf(fmt_popup, filename, thumb_width, caption)
  if(cat_output) cat(out_popup)
  invisible(out_popup)
}

get_rmd_header = function(filename, file_lines = NULL)
{
  if (is.null(file_lines)) file_lines = readLines(filename)
  header_symbols = which(grepl("---", file_lines))
  return(file_lines[header_symbols[1]:header_symbols[2]])
}

build_doc = function(
  file_stem,
  dir_out,
  base_path = here::here(),
  filename_out = NULL,
  type = NULL)
{

  if (FALSE)
  {
    file_stem ="week_03_data_exploration_determinstic_functions"
    dir_out = here::here("docs", "assignments")
    base_path = here::here("assignments", "eco_602")
    filename_out = NULL
    type = "html"
  }
  render_file = list.files(
    path = base_path,
    pattern = paste0(file_stem, ".Rmd"), recursive = TRUE, full.names = TRUE)

  if (length(render_file) == 0)
    cat(sprintf("No source file with name '%1$s.%2$s' found.", file_stem, "Rmd"))
  if (length(render_file) > 1)
    cat(sprintf("Duplicate source files with name '%1$s.%2$s' were found.", file_stem, "Rmd"))

  stopifnot(length(render_file) == 1)

  output_file =
    sprintf(
      "%1$s.%2$s",
      paste0(file.path(
        dir_out,
        ifelse(
          is.null(filename_out),
          file_stem,
          filename_out
        )
      )),
      type)

  output_file_stem =
    sprintf(
      "%1$s",
      paste0(file.path(
        dir_out,
        ifelse(
          is.null(filename_out),
          file_stem,
          filename_out
        )
      )))

  if (is.null(type))
  {
    rmarkdown::render(
      input = render_file,
      output_file = paste0(output_file_stem, ".html")
    )
    return(TRUE)
  }
  if (type == "html")
  {
    rmarkdown::render(
      input = render_file,
      output_file = paste0(output_file_stem, ".html"),
      # output_file,
      output_format = "html_document")
    return(TRUE)
  }
  if (type == "pdf")
  {
    rmarkdown::render(
      input = render_file,
      output_file = paste0(output_file_stem, ".pdf"),
      output_format = "pdf_document",
      output_options = list("toc: TRUE", "number_sections: TRUE"))
    return(TRUE)
  }
  if (type == "beamer")
  {
    rmarkdown::render(
      input = render_file,
      output_file = paste0(output_file_stem, ".pdf"))
  }
}

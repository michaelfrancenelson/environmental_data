
ass_dir = here::here("assignments", "in_class")

ass_dir = here::here("assignments", "in_class")

list.dirs(path = ass_dir, full.names = FALSE, recursive = FALSE)

fname = list.dirs(path = ass_dir, full.names = TRUE, recursive = FALSE)[1]

basename(fname)
dirname(fname)



basedir(fname)
for (fname in list.dirs(path = ass_dir, full.names = TRUE, recursive = FALSE))
{
  file.rename(
    from = fname, 
    to = 
      file.path(
        dirname(fname),
        paste0("in_class_", 
               basename(fname))))
  
  
}


search_path = "C:/Users/michaelnelso/git/eco_602_634_2020/assignments/labs/lab_05_uncertainty_samples_populations/moodle"

files = list.files(search_path,
                   pattern = ".Rmd", full.names = TRUE)
i = 1
fname = files[1]


for (i in 2:length(files))
{
  
  fname = files[i]  
  fdir = dirname(fname)
  
  b_fname = basename(fname)
  
  
  q_search_fmt = "Q[0-9][0-9]"
  
  name_fmt = "Q_%0.2d_lab_%0.2d"
  sprintf(name_fmt, i, 5)
  
  gsub("Q[0-9][0-9]", "Q_", b_fname, perl = TRUE)
  new_name =
    file.path(
      fdir, 
      gsub(q_search_fmt, sprintf(name_fmt, i, 5), b_fname, perl = TRUE))
  
  fname
  new_name
  
  file.rename(
    from = fname, 
    to = new_name) 
  
  
}

for (fname in list.dirs(path = ass_dir, full.names = TRUE, recursive = FALSE))
{
  
  # ass_dir_i = 
  list.files(path = fname)
  
  fname
  
  b_rmds = list.files(fname, include.dirs = FALSE, pattern = ".Rmd", full.names = TRUE)
  b_name = basename(fname)  
  b_dir = dirname(fname)
  
  b_name
  b_dir
  b_rmds
  
  b_rmds
  
  i = 1
  rmd_name = b_rmds[i]
  
  
  for (i in 1:length(b_rmds))
  {
    rmd_name
    dirname(rmd_name)  
    file.rename(
      from = rmd_name, 
      to = 
        file.path(
          dirname(fname),
          paste0("in_class_", 
                 basename(fname))))
  }
  
  
}


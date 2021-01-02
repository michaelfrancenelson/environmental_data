rwalk_1 = function(n)
{
  coord = c(0, 0)
  dat_out = data.frame(matrix(0, nrow = n, ncol = 3))
  for (i in 1:n)
  {
    index = sample(2, 1)
    direction = sample(c(1, -1), 1)
    
    coord[index] = coord[index] + direction
    dat_out[i, ] = c(i, coord)
  }
  # dat_out[, 1] = 1:n
  names(dat_out) = c("step", "x", "y")
  return(dat_out)
}

if(FALSE)
{
  i = 1

  rwalk_1(100)
  
  n = 1000
  plot(y ~ x, data = rwalk_1(n), type = "l", ann = FALSE, frame = TRUE, axes = FALSE, asp = 1)
  
  }
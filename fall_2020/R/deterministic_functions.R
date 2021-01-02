# R functions for deterministic functions and data exploration


# ---- functions for fitting a linear model ----
if (TRUE)
{
  get_y_intercept = 
    function(x1, y1, slope) 
      return(-(x1 * slope) + y1)
  
  linear = 
    function(x, yint, slope) 
      return(yint + x * slope)
  
  
  line_point_slope = function(x, x1, y1, slope)
  {
    return(linear(x, get_y_intercept(x1, y1, slope), slope))
  }
}



# ---- line_point_slope_example ----
if (FALSE)
{
  # Plot some data and use line_point_slope to visually 
  # estimate parameters for fitting a linear model.
  plot(Petal.Width ~ Petal.Length, data = iris)
  
  # A plausible center point might be (4.0, 1.2):
  curve(line_point_slope(x, x1 = 4.0, y1 = 1.2, slope = 0.1), add = TRUE)
  
  # Try a different slope:
  curve(line_point_slope(x, x1 = 4.0, y1 = 1.2, slope = 0.4), col = 2, add = TRUE)
}

# ---- functions_for_fitting_a_logistic_model ----
if (TRUE)
{
  get_logistic_param_a = function(slope, midpoint)
  {
    b = slope / 4
    return (-midpoint * (slope / 4))
  }
  
  get_logistic_param_b = function(slope)
  {
    return (slope / 4)
  }
  
  logistic = function(x, a, b)
  {
    val = exp(a + b * x)
    return(val / (1 + val))
  }
  
  logistic_midpoint_slope = function(x, midpoint, slope)
  {
    b = get_logistic_param_b(slope)
    a = get_logistic_param_a(slope, midpoint)
    return(logistic(x, a, b))
  }
  
  if (FALSE)
  {
    
    slope = 10
    midpoint = 5
    curve(logistic(x, get_logistic_param_a(slope, midpoint) , get_logistic_param_b(slope)), from = 0, 15)
    curve(logistic_midpoint_slope(x, 10, 4), add = T, from = 0, to = 20)
  }
}


# ---- logistic_example ----
if (FALSE)
{
  # Adapted from kevin's old lab exercises
  # source("https://michaelfrancenelson.github.io/eco_602_634_2020/mc_garigal_data/biostats_mcGarigal.R")
  
  dat_dir = "https://michaelfrancenelson.github.io/eco_602_634_2020/data/"
  
  birds_dat = read.csv(file.path(dat_dir, "birds_dat_1.csv"))
  
  names(birds_dat)
  
  plot(BRCR ~ basal_area, data = birds_dat)
  plot(BRCR > 0 ~ basal_area, data = birds_dat)
  
  midpoint = 50
  slope = 1
  
  curve(logistic_midpoint_slope(x, midpoint, slope), add = T)
  curve(logistic_midpoint_slope(x, 40, 0.5), add = T)
  
  plot(BRCR > 0 ~ log_basal_area, data = birds_dat)
  curve(logistic_midpoint_slope(x, 3.5, 20), add = T)
  
  
  
  
}


# ---- Ricker function ----
if (TRUE)
{
  ricker = function(x, a, b)
  {
    return (a * (x ^ (-b * x)))
  }
  
  if (FALSE)
  {
  x = seq(0, 3, length.out = 1000)  
  plot(x, ricker(x, 2, 3), type = "l")  
  }
  
}
#' Parameterize a linear function with a point and slope
#'
#' Calculates the value of y for a linear function, given the coordinates
#' of a known point (x1, y1) and the slope of the line.
#' 
#' @param x Value of x for the output coordinate
#' @param x1 x-coordinate of the calibration point
#' @param y1 y-coordinate of the calibration point
#' @param slope slope of the calibration point
#' 
#' @return y-coordinate for the input x
#' @export


# Calculates the value of y for a linear function, given the coordinates
# of a known point (x1, y1) and the slope of the line.
line_point_slope = function(x, x1, y1, slope)
{
  get_y_intercept = 
    function(x1, y1, slope) 
      return(-(x1 * slope) + y1)
  
  linear = 
    function(x, yint, slope) 
      return(yint + x * slope)
  
  return(linear(x, get_y_intercept(x1, y1, slope), slope))
}



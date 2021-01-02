#' Ricker function
#' 
#' 
#' @param x input value
#' @param a a-parameter for the Ricker function
#' @param b b-parameter for the Ricker function
#' 
#' @return 
#' @export


ricker_fun = function(x, a, b) { return(a * x * exp(-b * x))}
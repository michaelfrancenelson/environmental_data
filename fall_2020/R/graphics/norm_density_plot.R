if(FALSE)
{
  dev.off()  
  norm_density_plot(4.3, mean = 5)
  
  cdf_fmt = "x: %1$s cm\nquantile: %2$s"
  norm_cdf_plot(10.2, mean = 11.2, sd = 3, title_fmt = cdf_fmt)
  
}




norm_density_plot = function(
  x_1, mean = 0, sd = 1,
  xmin = mean -3 * sd, xmax = mean + 3 * sd, len = 1000,
  fill_cdf = rgb(0, 0.3, 0.8, 0.15), 
  digits = 2, lty_density = 2, 
    x_lab = "x", y_lab = "f(x)",
   title_fmt = 
    paste0(
      "x = %1$s\n",
      "probability density (height of the curve at x) = %2$s\n",
      "cumulative density (area of shaded region) = %3$s"))
{
  require(ggplot2, quietly = TRUE)
  
  norm_dat = data.frame(
    x = x1 <- seq(xmin, xmax, length.out = len),
    y0 = 0 * x1,
    y1 = dnorm(x1, mean = mean, sd = sd)
  ); rm(x1)
  
  y_intercept = dnorm(x_1, mean = mean, sd = sd)
  
  return(
    ggplot(norm_dat) +
      geom_line(aes(x, y1)) +
      geom_ribbon(
        data = subset(norm_dat, x < x_1),
        mapping = aes(x = x, ymin = y0, ymax = y1),
        fill = fill_cdf) +
      geom_hline(yintercept = y_intercept, lty = lty_density) + 
      ylab(y_lab) + xlab(x_lab) +
      ggtitle(paste0(
        sprintf(
          title_fmt,
          round(x_1, digits),
          round(y_intercept, digits),
          round(pnorm(x_1, mean = mean, sd = sd), digits)
        )))
  )
}

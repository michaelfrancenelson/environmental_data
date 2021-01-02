if(FALSE)
{
  source(here::here("function_scripts", "norm_density_plot.R"))
  source(here::here("function_scripts", "norm_quantile_plot.R"))
  source(here::here("function_scripts", "norm_cdf_plot.R"))
  dev.off() 
  norm_density_plot(0.3)
  norm_cdf_plot(0.3)
  norm_quantile_plot(0.3)
  
  cdf_fmt = "x: %1$s\nquantile: %2$s"
  norm_cdf_plot(10.2, mean = 11.2, sd = 3, title_fmt = cdf_fmt)

  x1 = 10.0
  x2 = 0.3
  
  plot_grid(
    norm_density_plot(x1),
    norm_cdf_plot(x1),
    norm_quantile_plot(pnorm(x1)),
    norm_density_plot(x2),
    norm_cdf_plot(x2),
    norm_quantile_plot(pnorm(x2))
    
  )
  
    
}

norm_cdf_plot = function(
  x_1, mean = 0, sd = 1,
  xmin = mean -3 * sd, xmax = mean + 3 * sd, len = 1000,
  fill_cdf = rgb(0, 0.3, 0.8, 0.15),
  x_lab = "x", y_lab = "cumulative density",
  digits = 2, lty_density = 2, 
  title_fmt = "x: %1$s\nquantile: %2$s")
{
  require(ggplot2, quietly = TRUE)
  
  norm_dat = data.frame(
    x = x1 <- seq(xmin, xmax, length.out = len),
    y0 = 0 * x1,
    y1 = pnorm(x1, mean = mean, sd = sd)
  ); rm(x1)
  
  y_intercept = pnorm(x_1, mean = mean, sd = sd)
  
  return(
    ggplot(norm_dat) +
      geom_line(aes(x, y1)) +
      geom_segment(
        mapping = aes(
          x = x_1, xend = x[1], 
          y = y_intercept, yend = y_intercept),
        arrow = arrow(length = unit(0.03, "npc"))) +
      geom_segment(
        mapping = aes(
          x = x_1, xend = x_1, 
          y = y_intercept, yend = 0),
        arrow = arrow(length = unit(0.03, "npc"))) +
      ylab(y_lab) + xlab(x_lab) +
      ggtitle(paste0(
        sprintf(
          title_fmt, 
          round(x_1, digits),
          round(pnorm(x_1, mean = mean, sd = sd), digits)
          # round(pnorm(x_1, mean = mean, sd = sd), digits),
          )))
  )
}

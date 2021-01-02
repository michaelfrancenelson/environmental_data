if(FALSE)
{
require(cowplot)
 dev.off() 
  x1 = 10.0
  x2 = 0.3
  
  
  dnorm(x1)
  pnorm(x1)
  qnorm(pnorm(x1))
  
  
  qnorm(pnorm(x2))
  
  norm_quantile_plot(0.75, mean = 12, sd = 1)
  
  
  plot_grid(
    norm_density_plot(x1),
  norm_cdf_plot(x1),
  norm_quantile_plot(pnorm(x1)),
    norm_density_plot(x2),
  norm_cdf_plot(x2),
  norm_quantile_plot(pnorm(x2))
    
  )
}

norm_quantile_plot = function(
  p_1, mean = 0, sd = 1,
  xmin = 0.001, xmax = 0.999, len = 1000,
  fill_cdf = rgb(0, 0.3, 0.8, 0.15), 
  digits = 2, lty_density = 2,
  y_lab = "x", x_lab = "quantile",
  title_fmt = "quantile: %1$s\nx: %2$s\n")
{
  require(ggplot2, quietly = TRUE)
  
    norm_dat = data.frame(
    x = x1 <- seq(xmin, xmax, length.out = len),
    y1 = qnorm(x1, mean = mean, sd = sd)
  ); rm(x1)
  
  y_intercept = qnorm(p_1, mean = mean, sd = sd)
 
  return(
    ggplot(norm_dat) +
      geom_line(aes(x, y1)) +
      geom_segment(
        mapping = aes(
          x = p_1, xend = x[1], 
          y = y_intercept, yend = y_intercept),
        arrow = arrow(length = unit(0.03, "npc"))) +
      geom_segment(
        mapping = aes(
          x = p_1, xend = p_1, 
          y = y_intercept, yend = y1[1]),
        arrow = arrow(length = unit(0.03, "npc"))) +
      ylab(y_lab) + xlab(x_lab) +
      ggtitle(paste0(
        sprintf(
          title_fmt, 
          round(p_1, digits), 
          round(y_intercept, digits))))
  )
}

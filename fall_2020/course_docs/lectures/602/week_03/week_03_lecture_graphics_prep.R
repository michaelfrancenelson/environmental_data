# devtools::install_github("gadenbuie/ggpomological")
# devtools::install_github("hrbrmstr/hrbrthemes")
# devtools::install_github("vankesteren/firatheme")
# devtools::install_github('cttobin/ggthemr')
# require(ggpomological)
# require(hrbrthemes)
# require(ggtmemr)
# tt = theme_ipsum()
# tt = theme_gray()
# tt = theme_modern_rc()

# tt = theme_ipsum_ps()
# tt = theme_ipsum_tw()
# tt = theme_ipsum_gs()
# tt = theme_ipsum(grid="Y")

require(latex2exp)
require(ggplot2)
# tt = theme_gray()
tt = 
  # tt + 
  theme(
    legend.text = element_text(size = 22),
    title = element_text(
      size = 30, 
      vjust = 0.5, 
      face = "italic"),
    legend.position = "bottom"
  )

# ---- lecture_1_function_types_intuition ---s-
{
  source(here::here("formatting", "functions", "rmd_functions.R"))
  source(find_file("deterministic_functions.R"))
  
  
  x = seq(-15, 13, length.out = 1000)
  parms = list(a = 1.1, b = 2, c = -70, d = 2)
  y = parms$a * (x ^ 3) + parms$b * (x ^ 2) + parms$c * x + parms$b
  g_fun_degree_3 = ggplot(data.frame(x, y), aes(x, y)) + geom_line() + xlab("x") + 
    ggtitle(TeX(sprintf('$\\f(x)= %1$s x^3 + %2$s x^2 - %3$sx + %4$s$', parms$a, parms$b, -1 * parms$c, parms$d)
    ))
  
  parms = list(a = 2, b = 100, c = 29)
  x = seq(-10, 15, length.out = 1000)
  y = parms$a * (x ^ 3) + parms$b * x + parms$c
  g_monotonic_1 = ggplot(data.frame(x, y), aes(x, y)) + 
    geom_line() +
    xlab("x") +
    ggtitle(TeX(sprintf('$\\f(x)= %1$sx^3 + %2$sx + %1$s$', parms$a, parms$b, parms$c)))
  
  parms = list(a = 2.2, b = 100, c = 29)
  x = seq(-5, 30, length.out = 1000)
  y = atan(x)
  g_asympt_1 = ggplot(data.frame(x, y), aes(x, y)) + 
    geom_line() +
    xlab("x") +
    ggtitle('f(x)= atan(x)')
  
  
  g_exp = 
    ggplot(data.frame(
      x4 <- seq(0.0001, 5, length.out = 1000),
      y = exp(x4)),
      aes(x4, y)) + 
    geom_line() +
    xlab("x") +
    ggtitle(TeX('$\\f(x) = e ^ x = exp(x)'))
  
  g_exp_2 = 
    ggplot(data.frame(
      x4a <- seq(0.0001, 5, length.out = 1000),
      y = exp(-x4a)),
      aes(x4a, y)) + 
    geom_line() +
    xlab("x") +
    ggtitle(TeX('$\\f(x) = e ^ x = exp(-x)'))
  
  
  g_log = 
    ggplot(data.frame(
      x5 <- seq(0.0001, 5, length.out = 1000),
      y = log(x5)),
      aes(x5, y)) + 
    geom_line() +
    xlab("x") +
    ggtitle(TeX('$\\f(x) = ln(x)'))
  
  
  
  parms = list(a = 2.1, b = 3)
  g_ricker = 
    ggplot(
      data.frame(
        x6 <- seq(0, 3, length.out = 1000),
        y = ricker(x6, parms$a, parms$b)),
      aes(x = x6, y = y)) +
    geom_line() +
    xlab("x") +
    ggtitle(TeX(sprintf('$\\f(x) = %1$s x ^ {-%2$s x}', parms$a, parms$b)))
  
  
  slope = 8
  midpoint = 5.7
  
  
  parms = list(
    a = round(get_logistic_param_a(slope, midpoint), 1),
    b = round(get_logistic_param_b(slope), 1))
  val = sprintf("exp(%1$s + %2$s x)", parms$a, parms$b)
  g_logit = 
    ggplot(
      data.frame(
        x7 <- seq(0, 10, length.out = 1000),
        y = logistic(x7, parms$a, parms$b)),
      aes(x = x7, y = y)) +
    geom_line() +
    xlab("x") +
    ggtitle(TeX(sprintf('$\\f(x) = \\frac{%1$s}{1 + %1$s}$', val)))
  
  g_rational = ggplot(
    data.frame(
      x_rat <- seq(-1, 1.2, length.out = 1000),
      y = 1 / x_rat),
    aes(x = x_rat, y = y)) +
    geom_line() +
    xlab("x") +
    ylim(-25, 27) +
    ggtitle(TeX('$\\f(x) = \\frac{1}{x}$'))
  
  g_rational_2 = ggplot(
    data.frame(
      x_rat2 <- seq(-1, 1.2, length.out = 1000),
      y = 1 / (x_rat + x_rat2 ^ 2)),
    aes(x = x_rat, y = y)) +
    geom_line() +
    xlab("x") +
    ylim(-60, 57) +
    ggtitle(TeX('$\\f(x) = \\frac{1}{x + x^2}$'))
  
  
  g_step =
    ggplot(
      data.frame(
        xstep <- 1:10, 
        y <- log(xstep), 
        xend = xstep + 1, 
        yend = y),
      aes(x = xstep, y, xend = xend, yend = yend)) +
    geom_segment() + 
    geom_point() +
    geom_point(mapping = aes(x = xend, y = y), pch = 1) +
    ggtitle(TeX('$\\f(x) = \\log(floor(x))$')) +
    xlab("x")
  
  g_step
  
  if (FALSE)
  {
    plot(x, atan(x))
    g_rational
    g_rational_2
    g_fun_degree_3 + tt
    g_monotonic_1 + tt
    g_asympt_1 + tt
    g_logit
  }
  }
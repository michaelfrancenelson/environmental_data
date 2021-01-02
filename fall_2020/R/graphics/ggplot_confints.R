ggplot_confints = function(n_ints, n_samp, p_val)
{
  # z_val = signif(qnorm(1 - (p_val * 0.5)), 3)
  out = data.frame()
  t_val = signif(qt(1 - (p_val * 0.5), df = n_ints), 3)
  
  for (i in 1:n_ints)
  {
    figs = 4
    x_i = rnorm(n_samp)
    m_i = signif(mean(x_i), figs)
    ssd_i = sd(x_i)
    se_i = ssd_i / sqrt(n_samp)
    # err_i = se_i * z_val
    err_i = se_i * t_val
    upper = signif(m_i + err_i, figs)
    lower = signif(m_i - err_i, figs)
    ci  = c(lower, upper)
    contains_mean = (lower <= 0) & (upper >= 0)
    out = rbind(
      out, 
      data.frame(
        x = i, 
        mean = m_i,
        ssd = ssd_i,
        se = se_i,
        err = err_i,
        ci = ci,
        sample = i,
        contains_mean = contains_mean)
    )
  }
  return(out)
}

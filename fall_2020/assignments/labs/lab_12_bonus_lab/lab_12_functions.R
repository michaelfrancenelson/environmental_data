# ---- test_data ----
if(FALSE)
{
  rm(list = ls())
  
  birds = read.csv(here::here("data", "bird.sub.csv"), header = TRUE)
  hab = read.csv(here::here("data", "hab.sub.csv"), header = TRUE)
  birdhab = merge(hab, birds, by = c('basin', 'sub'))
  
  y_intercept = 0.099
  
  slopes = c(0.005, 0.006, 0.008)
  slopes = seq(-0.01, 0.01, length.out = 15)
  sample_sizes = 5:50
  sigmas = 0.141
  n_sims = 10
  x = birdhab$ls
  alpha = c(0.05, 0.01, 0.001)
  slope = slopes[1]
  sigma = sigmas[1] 
}

linear = function(x, y_int, slope) { return (y_int + x * slope)}

linear_simulator = function(x, y_int, slope, sigma)
{
  linear = function(x, y_int, slope) { return (y_int + x * slope)}
  return(rnorm(
    n = length(x),
    mean = linear(x, y_int, slope),
    sd = sigma))
}

linear_sim_fit = function(x, y_int, slope, sigma)
{
  y_sim = linear_simulator(
    x = x,
    y_int = y_int,
    slope = slope,
    sigma = sigma
  )
  return(lm(y_sim ~ x))
}  


p_val_lm_sim = function(x, y_int, slope, sigma, n_sims)
{
  p_vals = numeric(n_sims)
  for (i in 1:n_sims)
  {
    fit_sim = linear_sim_fit(
      x, y_int = y_intercept, 
      slope = slope, sigma = sigma)
    p_vals[i] = summary(fit_sim)$coefficients[2, 'Pr(>|t|)']
  }
  return(p_vals)
}

power_sim_lm = function(
  y_intercept, slopes, 
  sigmas, n_sims,
  sample_sizes = NULL,
  x_min = NULL, x_max = NULL,
  x = NULL,
  alpha = 0.05)
{
  sim_x = TRUE
  if (!is.null(x))
  {
    sim_x = FALSE
    sample_sizes = length(x)
  }
  # p_vals = numeric(n_sims)
  power_out = array(
    0, 
    dim = c(
      length(sigmas), 
      length(slopes), 
      length(sample_sizes), 
      length(alpha)))
  
  for (sg in 1:length(sigmas))
  {
    for (sl in 1:length(slopes))
    {
      for (sz in 1:length(sample_sizes))
      {
        if (sim_x)
        {
          sample_size = sample_sizes[sz]
          x = seq(x_min, x_max, length.out = sample_size)    
        }
        
        p_vals = p_val_lm_sim(
          x, y_int = y_int,
          slope = slopes[sl], sigma = sigmas[sg],
          n_sims = n_sims)
        
        power_out[sg, sl, sz, ] = 
          sapply(alpha, function(x) sum(p_vals < x) / n_sims)
      }
    }
  }
  return(list(
    alpha = alpha, 
    sample_size = sample_sizes,
    slope = slopes,
    sigma = sigmas,
    power = power_out))
}

# ---- test_cases ----
if (FALSE)
{
  
  fit_1 = lm(BRCR~ls, data = birdhab)
  sd_obs = summary(fit_1)$sigma
  int_obs = coefficients(fit_1)[1]
  slope_obs = coefficients(fit_1)[2]
  
  
  
  n_effect_sizes = 20
  effect_sizes_1 = seq(-.01, .01, length.out = n_effect_sizes)
  
  n_sims = 100
  
  aa = power_sim_lm(
    x = birdhab$ls,
    n_sims = n_sims,
    y_intercept = int_obs,
    # sigmas = 0.0001,
    sigmas = sd_obs,
    slopes = effect_sizes_1)
  
  
  aa
  # 
  # 
  # 
  # dim(power_out)
  # power_out[1, , , 1]
  # power_out[1, 8, , 1]
  # image(power_out[1, , , 2])
  # 
  # 
  # 
  # 
  
}

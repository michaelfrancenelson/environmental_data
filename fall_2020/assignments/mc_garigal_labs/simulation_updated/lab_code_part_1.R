birds = read.csv(here::here("data", "bird.sub.csv"), header = TRUE)
hab = read.csv(here::here("data", "hab.sub.csv"), header = TRUE)
birdhab = merge(hab, birds, by = c('basin', 'sub'))


plot(
  birdhab$ls, birdhab$BRCR, 
  xlab = "late-successional forest extent",
  ylab = "Brown Creeper abundance",
  pch = 19)

fit_1 = lm(BRCR~ls, data = birdhab)

linear = function(x, y_intercept, slope) { return (y_intercept + x * slope)}

linear_simulator = function(x, y_intercept, slope, st_dev)
{
  linear = function(x, y_intercept, slope) { return (y_intercept + x * slope)}
  return(rnorm(n = length(x), mean = linear(x, y_intercept, slope), sd = st_dev))
}

linear_sim_fit = function(x, y_int, slope, st_dev)
{
  y_sim = linear_simulator(
    x = x,
    y_int = y_int,
    slope = slope,
    st_dev = st_dev
  )
  return(lm(y_sim ~ x))
}


fit_1_summary = summary(fit_1)
fit_1_coefs = coefficients(fit_1)
str(fit_1_coefs)

sd_obs = fit_1_summary$sigma
int_obs = fit_1_coefs[1]
slope_obs = fit_1_coefs[2]


pf(1, 10, 12)

df1 = 10
df2 = 15

x = seq(0.01, 5, length.out = 100)

dat = data.frame(x = x, y = df(x, df1, df2))
require(ggplot2)

f_obs = 4.3

ggplot(dat, aes(x, y)) +
  geom_line() +
  geom_ribbon(
    data = subset(dat, x < f_obs),
    aes(x = x, ymin = 0, ymax = y), fill = adjustcolor("steelblue", 0.3))


1 - pf(f_obs, df1, df2)
pf(f_obs, df1, df2, lower.tail = FALSE)


require(palmerpenguins)
fit_1 = lm(body_mass_g ~ sex, data = penguins)
anova(fit_1)

pf(q = 72.961, df1 = 1, df2 = 331, lower.tail = FALSE)

fit_2 = lm(body_mass_g ~ sex * species, data = penguins)
anova(fit_2)


pf(q = 8.757, df1 = 2, df2 = 327, lower.tail = FALSE)

838278 / 95727


pf(q = 1, df1 = 2, df2 = 327, lower.tail = FALSE)

qf(p = 0.5, df1 = 2, df2 = 327)



pnorm(1)


# ---- r effect_size_simulation_save_file ----

if(FALSE)
{
  alpha = 0.05
  n_sims = 1000
  p_vals = numeric(n_sims)
  n_effect_sizes = 80
  effect_sizes = seq(from = -0.01, to = 0.01, length.out = n_effect_sizes)
  effect_power = numeric(length(effect_size))
  
  for(j in 1:length(effect_sizes))
  {
    for(i in 1:n_sims)
    {
      fit_sim = linear_sim_fit(
        x = birdhab$ls,
        y_int = int_obs,
        slope = effect_sizes[j],
        st_dev = sd_obs
      )
      
      p_vals[i] = summary(fit_sim)$coefficients[2, 'Pr(>|t|)']
    }
    
    effect_power[j] = sum(p_vals < alpha) / n_sims
  }
  
  effect_size_sim = list(effect_sizes = effect_sizes, effect_power = effect_power)
  save(effect_size_sim, 
       file = here::here("data", "sim_output_effect_size.RData"))
  
  plot(effect_size_sim$effect_sizes, effect_power, type = 'l', xlab = 'Effect size', ylab = 'Power')
  abline(v = coef(fit_1)[2], lty = 2, col = 'red')
}




# ---- sample_size_simulation_save_file ----
if (FALSE)
{
  alpha = 0.05
  n_sims = 1000
  p_vals = numeric(n_sims)
  sample_sizes = seq(10, 50)
  sim_output_1 = numeric(length(sample_sizes))
  
  for(j in 1:length(sample_sizes))
  {
    
    x_vals = seq(0, 100, length.out = sample_sizes[j])
    
    for(i in 1:n_sims)
    {
      fit_sim = linear_sim_fit(
        x = x_vals,
        y_int = int_obs,
        slope = slope_obs,
        st_dev = sd_obs
      )
      p_vals[i] = summary(fit_sim)$coefficients[2, 'Pr(>|t|)']
    }
    sim_output_1[j] = sum(p_vals < alpha) / n_sims
  }
  sample_size_sim = list(
    sample_sizes = sample_sizes, 
    sample_powers = sim_output_1)
  save(sample_size_sim, 
       file = here::here("data", "sim_output_sample_size.RData"))
  
  plot(sample_size_sim$sample_sizes, sample_size_sim$sample_powers, type = 'l', xlab = 'Effect size', ylab = 'Power')
  abline(v = nrow(birdhab), lty = 2, col = 'red')
}



# ---- sample_size_effect_size_simulation_save_file ----
if (FALSE)
{
  
  alpha = 0.05
  n_sims = 50
  p_vals = numeric(n_sims)
  n_effect_sizes = 10
  effect_sizes = seq(from = -0.01, to = 0.01, length.out = n_effect_sizes)
  sample_sizes = seq(10, 50)
  
  sim_output_sample_size_effect_size = matrix(nrow = length(effect_sizes), ncol = length(sample_sizes))
  
  
  before = Sys.time()
  for(k in 1:length(effect_sizes))
  {
    effect_size = effect_sizes[k]
    print(paste0("computing effect size ", k," of ", length(effect_sizes)))
    for(j in 1:length(sample_sizes))
    {
      x_vals = seq(0, 100, length.out = sample_sizes[j])
      
      for(i in 1:n_sims)
      {
        fit_sim = linear_sim_fit(
          x = x_vals,
          y_int = int_obs,
          slope = effect_size,
          st_dev = sd_obs
        )
        p_vals[i] = summary(fit_sim)$coefficients[2, 'Pr(>|t|)']
      }
      sim_output_sample_size_effect_size[k, j] = sum(p_vals < alpha) / n_sims
      print(paste0("    sample size = ", sample_sizes[j]))
    }
  }
  after = Sys.time()
  
  after - before
  
  
  sim_output_sample_size_effect_size = list(data = sim_output_sample_size_effect_size, effect_sizes = effect_sizes, sample_sizes = sample_sizes)
  
  save(sim_output_sample_size_effect_size, effect_sizes, sample_sizes, 
       file = here::here("data", "sim_output_sample_size_effect_size.RData"))
  
  image(sim_output_sample_size_effect_size$data)
  
  contour(
    x = sim_output_sample_size_effect_size$effect_sizes, 
    y = sim_output_sample_size_effect_size$sample_sizes, 
    z = sim_output_sample_size_effect_size$data,
    xlab = "effect size",
    ylab = "sample size",
    main = "Contour Plot of Statistical Power")
  
  
 
  
  
  rgl::persp3d(
    x = sim_output_sample_size_effect_size$effect_sizes, 
    y = sim_output_sample_size_effect_size$sample_sizes, 
    z = sim_output_sample_size_effect_size$data,
    col = 'lightblue',
    xlab = "effect size",
    ylab = "sample size",
    zlab = "power")
  writeWebGL(
    dir = here::here("docs", "webGL"), 
    filename = here::here(
      "docs", "webGL",
      "effect_sample_size_power_sim_plot.html"),
    width = 1200, height = 1200
  )
}


# ---- sample_size_sd_simulation ----
if (FALSE)
{
  
  alpha = 0.05
  n_sims = 1000
  p_vals = numeric(n_sims)
  n_sds = 100
  pop_sds = seq(from = 0.1, to = 1.5, length.out = n_sds)
  sample_sizes = seq(10, 50)
  
  sim_output_sample_size_sd = matrix(nrow = length(pop_sds), ncol = length(sample_sizes))
  
  for(k in 1:length(pop_sds))
  {
    pop_sd_k = pop_sds[k]
    print(paste0("computing population s.d. ", k," of ", length(pop_sds)))
    for(j in 1:length(sample_sizes))
    {
      x_vals = seq(0, 100, length.out = sample_sizes[j])
      
      for(i in 1:n_sims)
      {
        fit_sim = linear_sim_fit(
          x = x_vals,
          y_int = int_obs,
          slope = slope_obs,
          st_dev = pop_sd_k
        )
        p_vals[i] = summary(fit_sim)$coefficients[2, 'Pr(>|t|)']
      }
      sim_output_sample_size_sd[k, j] = sum(p_vals < alpha) / n_sims
      print(paste0("    sample size = ", sample_sizes[j]))
    }
  }
  
  
  sim_output_sample_size_sd = 
    list(
      data = sim_output_sample_size_sd, 
      pop_sds = pop_sds, 
      sample_sizes = sample_sizes)
  
  save(sim_output_sample_size_sd2, 
       file = here::here(
         "data",
         "sim_output_sample_size_sd.RData"))
  
  
  
  image(sim_output_sample_size_sd)
  contour(
    x = pop_sds,
    y = sample_sizes, 
    z = sim_output_sample_size_sd,
    xlab = "population s.d.",
    ylab = "sample size",
    main = "Contour Plot of Statistical Power")
  rgl::persp3d(
    x = pop_sds,
    y = sample_sizes, 
    z = sim_output_sample_size_sd,
    xlab = "s.d.",
    ylab = "n",
    zlab = "power",
    col = 'lightblue')
  
  rgl::writeWebGL(
    dir = here::here("docs", "webGL"), 
    filename = here::here(
      "docs", "webGL",
      "sd_sample_size_power_sim_plot.html"),
    width = 1200, height = 1200
  )
  
  
  
}
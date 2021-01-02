# ---- test_data ----

birds = read.csv(here::here("data", "bird.sub.csv"), header = TRUE)
hab = read.csv(here::here("data", "hab.sub.csv"), header = TRUE)
birdhab = merge(hab, birds, by = c('basin', 'sub'))

fit_1 = lm(BRCR~ls, data = birdhab)
sd_obs = summary(fit_1)$sigma
int_obs = coefficients(fit_1)[1]
slope_obs = coefficients(fit_1)[2]

n_sims = 30
sim_1 = power_sim_lm(y_intercept = int_obs, slopes = slope_obs, sigmas = sd_obs, alpha = c(0.1, 0.05, 0.01, 0.001), x_min = 0, x_max = 100, n_sims = n_sims, sample_sizes = 5:40)



image(sim_1$power[1, 1, , ])

plot(sim_1$sample_size, sim_1$power[1, 1, , 4], type = "l", ylim = c(0, 1), xlab = "sample size", ylab = "statistical power")

matplot(x = sample_sizes, y = powers, type = "l")

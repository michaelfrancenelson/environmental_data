# ---- test_data ----

birds = read.csv(here::here("data", "bird.sub.csv"), header = TRUE)
hab = read.csv(here::here("data", "hab.sub.csv"), header = TRUE)
birdhab = merge(hab, birds, by = c('basin', 'sub'))

fit_1 = lm(BRCR~ls, data = birdhab)
sd_obs = summary(fit_1)$sigma
int_obs = coefficients(fit_1)[1]
slope_obs = coefficients(fit_1)[2]


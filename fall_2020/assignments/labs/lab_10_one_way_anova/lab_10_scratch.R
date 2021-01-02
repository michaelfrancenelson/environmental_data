rm(list = ls())


rope = read.csv(here::here("data", "rope.csv"))
rope$rope.type = factor(rope$rope.type)

n_obs = nrow(rope)
n_groups = length(levels(rope$rope.type))

ss_tot = sum((rope$p.cut - mean(rope$p.cut))^2)
df_tot = n_obs - 1

agg_resids = aggregate(
  x = rope$p.cut,
  by = list(rope$rope.type),
  FUN = function(x) (x - mean(x)))

str(agg_resids)

agg_sq_resids = aggregate(
  x = rope$p.cut,
  by = list(rope$rope.type),
  FUN = function(x) sum((x - mean(x))^2))


str(agg_sq_resids)

ss_within = sum(agg_sq_resids$x)
df_within = n_obs - n_groups

ss_among = ss_tot - ss_within
df_among = n_groups - 1

ms_within = ss_within / df_within
ms_among  = ss_among  / df_among

f_ratio = ms_among / ms_within
f_pval = 1 - pf(f_ratio, df_among, df_within)



boxplot(body_mass_g ~ sex * species, data = penguins)

agg_mean = aggregate(
  penguins$body_mass_g,
  by = list(penguins$species, penguins$sex), FUN = function(x) mean(x))

agg_count = aggregate(
  penguins$body_mass_g,
  by = list(penguins$species, penguins$sex), FUN = function(x) length(x))



plot(agg_mean$x)


abline(h = mean(agg_mean$x))


sum((agg_1$x - mean(agg_1$x))^2) / 2


agg_1



anova(lm(body_mass_g ~ sex * species, data = penguins))
anova(lm(body_mass_g ~ sex * species, data = penguins))






par(mfrow = c(1, 2))
boxplot(penguins$body_mass_g)
boxplot(body_mass_g ~ species, data = penguins)



fit_sp = lm(body_mass_g ~ species, data = penguins)
anova(fit_sp)



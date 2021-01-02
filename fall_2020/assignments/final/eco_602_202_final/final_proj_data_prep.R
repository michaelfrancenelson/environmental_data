dat_all = read.csv(here::here("data", "ATLANTIC_TR_all_data.csv"))
dat_means = read.csv(here::here("data", "ATLANTIC_TR_means.csv"))



head(dat_means[order(dat_means$N_body_mass, decreasing = TRUE), ])

dat1 = subset(dat_means, (N_body_mass > 500) & (N_body_length > 500))

dat1

dat = subset(dat_all, ID_sp == 192)
dat = subset(dat_all, ID_sp %in% c(64))
dat = subset(dat_all, ID_sp %in% c(21, 23))
dat = subset(dat_all, ID_sp %in% c(209, 210))

names(dat)

dat_cleaned = dat[, c("genus", "binomial", "body_mass", "body_length", "age", "sex", "longitude", "latitude", "status"), ]

dat_cleaned = subset(dat_cleaned, (!is.na(body_length)) & (!is.na(body_mass)))


head(dat_cleaned)
dim(dat_cleaned)

write.csv(dat_cleaned, file = here::here("data", "delomys.csv"))
write.csv(dat_cleaned, file = here::here("docs", "data", "delomys.csv"))



dat2 = subset(dat, !is.na(body_mass))
nrow(dat2)



head(dat2)



dat2 = subset(dat2, age != "juvenille")

hist(dat2$body_mass)


boxplot(dat2$body_mass ~ dat2$sex)
boxplot(dat2$body_mass ~ dat2$age * dat2$sex)
boxplot(dat2$body_mass ~ dat2$age)
boxplot(dat2$body_mass ~ dat2$age * dat2$ID_sp)
plot(dat2$body_length ~ dat2$body_mass)




dat = read.csv(here::here("data", "delomis"))
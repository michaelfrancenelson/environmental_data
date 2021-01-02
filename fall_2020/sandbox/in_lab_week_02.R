read.csv("https://michaelfrancenelson.github.io/eco_602_634_2020/data/bird.sta.csv")



dat_bird = read.csv(here("data", "bird.sta.csv"))
dat_habitat = read.csv(here("data", "hab.sta.csv"))
dat_all = merge(dat_bird, dat_habitat)




plot(ba.tot ~ elev, data = dat_all)
plot(ba.tot ~ elev, data = dat_all, pch = 16)
plot(ba.tot ~ elev, data = dat_all, pch = 16, cex = 0.1)
plot(ba.tot ~ elev, data = dat_all, pch = 16, cex = 0.1, col = 1)
plot(ba.tot ~ elev, data = dat_all, pch = 16, cex = 0.1, col = 2)
plot(ba.tot ~ elev, data = dat_all, pch = 16, cex = 0.1, col = "red")
plot(ba.tot ~ elev, data = dat_all, pch = 16, cex = 0.1, col = 3)
plot(ba.tot ~ elev, data = dat_all, pch = 16, cex = 0.1, col = "blue")



png(filename = "sample_cloud_points_green_gray.png", width = 1200, height = 900, res = 120)
plot(
  ba.tot ~ elev, 
  data = dat_all, pch = 16, cex = 0.01,   col = rgb(0,0,0, 0.1),
     main = "Basal area plot: green and gray cloud effect",
     xlab = "elevation",
     ylab = "basal area")
for (i in seq(4.5, 0.01, length.out = 30))
  points(
    ba.tot ~ elev, data = dat_all, 
    pch = 16, cex = i, 
    col = rgb(0,00,0, 0.008))
for (i in seq(3.5, 0.01, length.out = 30))
  points(
    ba.tot ~ elev, data = dat_all, 
    pch = 16, cex = i, 
    col = rgb(0,0.2,0, 0.008))
dev.off()


points(ba.tot ~ elev, data = dat_all, pch = 16, cex = 0.2, col = rgb(0,0,0, 0.05))
points(ba.tot ~ elev, data = dat_all, pch = 16, cex = 0.3, col = rgb(0,0,0, 0.02))
points(ba.tot ~ elev, data = dat_all, pch = 16, cex = 0.5, col = rgb(0,0,0, 0.03))
points(ba.tot ~ elev, data = dat_all, pch = 16, cex = 1.5, col = rgb(0,0,0, 0.04))
points(ba.tot ~ elev, data = dat_all, pch = 16, cex = 3.5, col = rgb(0,0,0, 0.01))
points(ba.tot ~ elev, data = dat_all, pch = 1, cex = 1.1, col = rgb(0,0,0, 0.1))

dev.off()




points(ba.tot ~ elev, data = dat_all, pch = 16, cex = 0.2, col = rgb(1,0,0, 0.2))



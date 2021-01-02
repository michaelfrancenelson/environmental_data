require()
source(here::here("rmd_tools", "find_file.R"))
require(magick)
require(cowplot)
require(ggplot2)
acorn_img = image_read(find_file("emojipng.com-8994842.png"))


png("tooo_many_acorns.png", width = 1500, height = 1200)
plot_grid(
  g_acorn,g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn,
  g_acorn,g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn,
  g_acorn,g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn,
  g_acorn,g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn,
  g_acorn,g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn,
  g_acorn,g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn,
  g_acorn,g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn,
  g_acorn,g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn,
  g_acorn,g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn,
  g_acorn,g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn,
  g_acorn,g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn,
  g_acorn,g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn,
  g_acorn,g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn,
  g_acorn,g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn,
  g_acorn,g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn,
  g_acorn,g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn,
  g_acorn,g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn,
  g_acorn,g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn,
  g_acorn,g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn,
  g_acorn,g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn,
  g_acorn,g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn,
  g_acorn,g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn,
  g_acorn,g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn,
  g_acorn,g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn,
  g_acorn,g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn,
  g_acorn,g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn,
  g_acorn,g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn,
  g_acorn,g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn,
  g_acorn,g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn,
  g_acorn,g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn,
  g_acorn,g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn,
  g_acorn,g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn,
  g_acorn,g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn,
  g_acorn,g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn,
  g_acorn,g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn,
  g_acorn,g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn,
  g_acorn,g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn,
  g_acorn,g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn,
  g_acorn,g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn,
  g_acorn,g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn,
  g_acorn,g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn,
  g_acorn,g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn,
  g_acorn,g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn,
  g_acorn,g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn,
  g_acorn,g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn,
  g_acorn,g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn,
  g_acorn,g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn,
  g_acorn,g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn, g_acorn,
  nrow = 24)
dev.off()
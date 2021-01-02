# ---- bird_data_files ----

if (FALSE)
{
  require(ggplot2)  
  require(data.table)
  require(cowplot)
  
  # Original data files:
  dat_dir_mg = "https://michaelfrancenelson.github.io/eco_602_634_2020/mc_garigal_data/"
  dat_dir = "https://michaelfrancenelson.github.io/eco_602_634_2020/data/"
  birds = fread(file.path(dat_dir_mg,"bird.sta.csv"))
  hab = fread(file.path(dat_dir_mg, "hab.sta.csv"))
  
  
  species_names = names(birds)[nchar(names(birds)) == 4]
  apply(birds[, -(1:2)], 2, sum)
  
  
  # Having the beginning R students deal with merging 
  # large files will distract from
  # the data exploration goals of the activity.
  
  habitat_dt = hab[, .(basin, sub, sta, lat, long, slope, aspect, basal_area = ba.tot)]
  birds_dt = birds[, .(BRCR, PSFL, RUHU, WIWR, WIWA, BGWA, GRJA)]
  birds_dt = birds[, which(names(birds) %in% species_names), with = FALSE]
  
  
  dat_all = data.table(merge(hab, birds))
  habitat_dt = dat_all[, .(basin, sub, sta, lat, long, slope, aspect, basal_area = ba.tot)]
  birds_dt   = dat_all[, .(BRCR, PSFL, RUHU, WIWR, WIWA, BGWA, GRJA)]
  
  dat_birds_1 = data.table(habitat_dt, birds_dt)
  dat_birds_1[, log_basal_area := log(basal_area)]
  
  # Save a reduced version for the first few uses in class.
  fwrite(dat_birds_1, here::here("docs", "data", "birds_dat_1.csv"))
  
  
  nm = species_names[1]
  gg_objs = list()
  
  pdf(file = here::here("Test.pdf"))
  for (i in 1:length(species_names))
  {
    nm = species_names[i]
    print(nm)
    dat_birds_1[, count := dat_birds_1[, which(names(dat_birds_1) == nm), with = FALSE]] 
    dat_birds_1[, pres := as.numeric(count > 0)]
    
    print(dat_birds_1[, sum(pres)])
    
    
    
    
    print(ggplot(dat_birds_1, aes(x = basal_area, y = count)) + 
      geom_point() + ggtitle(paste0(nm, " count")))
    
    print(ggplot(dat_birds_1, aes(x = basal_area, y = pres)) + 
      geom_point() + ggtitle(paste0(nm, " presence")))
    
    print(ggplot(dat_birds_1, aes(x = log_basal_area, y = pres)) + 
      geom_point() + ggtitle(paste0(nm, " presence")))
    
    
     gg_objs[[i]] = 
      ggplot(dat_birds_1, aes(x = basal_area, y = pres)) + 
      geom_point() + ggtitle(nm)
  }
  dev.off()
  
  pdf(file = here::here("Test.pdf"))
  {
    for(i in 1:length(gg_objs))
    {
      
      gg = gg_objs[[i]]
    # for(gg in gg_objs)
      print(gg)
    }
  }
  dev.off()  
  
  i = 56
  gg_objs[[i]]
  
  
  
  eval(species_names[1])
  
  dat_birds_1$eval(species_names[1])
  dat_birds_1$eval(species_names[1])
  
  dat_birds_1$parse(text = species_names[1])
  (eval(parse(text = species_names[1])))
  
  dat_birds_1$eval(parse(text = eval(species_names[1])))
  
  plot_grid(    
    ggplot(dat_birds_1, aes(x = basal_area, y = as.numeric(BRCR > 0))) + geom_point(),
    ggplot(dat_birds_1, aes(x = basal_area, y = as.numeric(PSFL > 0))) + geom_point(),
    ggplot(dat_birds_1, aes(x = basal_area, y = as.numeric(RUHU > 0))) + geom_point(),
    ggplot(dat_birds_1, aes(x = basal_area, y = as.numeric(WIWR > 0))) + geom_point(),
    ggplot(dat_birds_1, aes(x = basal_area, y = as.numeric(WIWA > 0))) + geom_point(),
    ggplot(dat_birds_1, aes(x = basal_area, y = as.numeric(BGWA > 0))) + geom_point(),
    ggplot(dat_birds_1, aes(x = basal_area, y = as.numeric(GRJA > 0))) + geom_point()
  )
  
  
}



require(raster)
require(rgdal)
require(data.table)


"C:/Users/michaelnelso/git/eco_602_634_2020/course_docs/lectures"

county_dir = "C:/data/TIGER/counties"
county_dir = file.path("C:", "data", "TIGER", "counties")





nlcd_dir = "C:/data/nlcd"



# Using sp packages
{
  counties_spdf = readOGR(file.path(county_dir, "tl_2010_us_county10.shp"))
  proj4string(counties_spdf)
  
  
  class(counties_spdf$GEOID10)
  
  nlcd_data = raster(file.path(nlcd_dir, "NLCD_Land_Cover_Change_Index_L48_20190424.img"))  
  
  nlcd_data[1:10, 1:10]
  
  proj4string(nlcd_data)
  proj4string(nlcd_data)
  
  counties_transformed = spTransform(counties_spdf, proj4string(nlcd_data))
  proj4string(counties_transformed)
                                     
  counties_transformed[1, ]

  extract(nlcd_data, counties_transformed[203, ], fun = table)
  
  plot(counties_transformed[203, ])
  
  
  
  
  
  
  
  
  crop_i = crop(nlcd_data, counties_transformed[203, ])
  table(crop_i[])
  
  levels(nlcd_data[])
  
  
  
  
  
  
  
  image(nlcd_data[1:500, 1:500])
  
  
    
  nlcd_data
  
  head(counties_spdf)
  
  extract()
  
  plot(counties_spdf[1, ])
  
  subset(
    counties_spdf, 
    STATEFP10 == "02")
  
  plot(
  subset(
    counties_spdf, 
    STATEFP10 == "02")
  )
  
  
  
  
  
  

}







plot(counties)



plot(geom_sf(counties[1:200, ]))



















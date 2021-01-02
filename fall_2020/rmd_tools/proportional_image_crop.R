proportional_image_crop = function(
  filename, 
  height_pct = 1, 
  width_pct = 1, 
  height_offset = 0, 
  width_offset = 0)
{
  
  suppressMessages(suppressWarnings(require(magick)))
  img = image_read(find_file(filename))
  return(
    image_crop(
      img,
      sprintf(
        fmt = "%1$sx%2$s+%3$s+%4$s", 
        image_info(img)$width * width_pct,
        image_info(img)$height * height_pct,
        image_info(img)$width * width_offset,
        image_info(img)$height * height_offset
      )
    )
  )
}


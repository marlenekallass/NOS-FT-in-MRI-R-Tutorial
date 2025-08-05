create_gif_from_plots = function(plots, filename = "animation.gif", width = 318, height = 362, res = 96, fps = 5) {
  source("functions/misc_utils.R")
  
  install_and_load(c("magick"))
  img_plot = image_graph(width = width, height = height, res = res)
  invisible(print(plots))
  dev.off()  # close the image device
  anime = image_animate(image_join(img_plot), fps = fps)
  image_write(anime, filename)
}

create_gif_from_plots = function(plots, filename = "animation.gif", width = 318, height = 362, res = 96, fps = 5) {
  source("functions/misc_utils.R")
  
  install_and_load(c("magick"))
  img_plot = image_graph(width = width, height = height, res = res)
  invisible(print(plots))
  dev.off()  # close the image device
  anime = image_animate(image_join(img_plot), fps = fps)
  image_write(anime, filename)
}

save_gg = function(p = last_plot(), path_figures = "../../figures", filename = 'figure.png',w = 80, h = 80) {
  
  path_out = file.path(path_figures, filename)
  ggsave(path_out, plot=p, device="png", width=w, height=h,units = 'mm')
}

save_figure = function(p, path_figures =  "../../figures", filename = 'figure.png',w = 800, h = 800){
  
  path_out = file.path(path_figures, filename)
  png(path_out, width=w, height=h, bg = "transparent")
  replayPlot(p)
  dev.off()
  
}
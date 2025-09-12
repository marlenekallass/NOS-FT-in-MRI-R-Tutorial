create_gif_from_plots = function(plots, filename = "animation.gif", width = 318, height = 362, res = 96, fps = 5) {
  source("functions/misc_utils.R")
  
  install_and_load(c("magick"))
  img_plot = image_graph(width = width, height = height, res = res, bg = "transparent")
  invisible(print(plots))
  dev.off()  # close the image device
  anime = image_animate(image_join(img_plot), fps = fps)
  image_write(anime, filename)
}

save_gg = function(p = last_plot(), path_figures = "../../figures", filename = 'figure.png',w = 80, h = 80) {
  
  path_out = file.path(path_figures, filename)
  ggsave(path_out, plot=p, device="png", width=w, height=h,units = 'mm')
}

save_figure = function(p, path_figures =  "../../figures", filename = 'figure.png',w = 80, h = 80){
  
  path_out = file.path(path_figures, filename)
  png(path_out, width=w, height=h, bg = "transparent")
  replayPlot(p)
  dev.off()
  
}

# Note that for this particular function
# I was fully reliant on ChatGPT. 3D plots are hard
plot_complex_imageline_3D = function(eye =  list(x=1.2, y=-1.8, z=1.5) ,img_rec){
  
  
  
  # normalize img_rec by its maximum magnitude
  img_rec_norm = img_rec / max(abs(img_rec))
  
  df = data.frame(
    Index = 1:length(img_rec_norm),
    Re = Re(img_rec_norm),
    Im = Im(img_rec_norm),
    mag = abs(img_rec_norm)
  )
  
  # greyscale
  grey_vals = scales::rescale(df$mag, to=c(0,1))
  colors = gray(grey_vals)
  
  # scatter plot
  p = plot_ly(df, x=~Index, y=~Re, z=~Im,
              type="scatter3d", mode="markers",
              marker=list(color=colors, size=4,
                          line=list(color="black", width=1))) %>%
    layout(
      scene = list(
        xaxis = list(title="Column", range = c(1,length(img_rec)),  showticklabels=FALSE),
        yaxis = list(title="Re", range=c(-1,1), zeroline=TRUE, zerolinecolor="black",nticks = 5),
        zaxis = list(title="Im", range=c(-1,1), zeroline=TRUE, zerolinecolor="black",nticks = 5),
        camera = list(eye = eye)
      )
    )
  
  # curtain
  verts = rbind(
    cbind(df$Index, 0, 0),      # x-axis
    cbind(df$Index, df$Re, df$Im)
  )
  
  n = length(df$Index)
  i = j = k = c()
  for (idx in 1:(n-1)) {
    a  = idx-1
    b  = idx
    c1 = n + idx - 1
    c2 = n + idx
    i = c(i, a, b)
    j = c(j, b, c2)
    k = c(k, c1, c1)
  }
  
  # add end caps
  i = c(i, 0, n-1)
  j = c(j, n, 2*n-1)
  k = c(k, n, 2*n-1)
  
  p = p %>%
    add_trace(
      type="mesh3d",
      x=verts[,1], y=verts[,2], z=verts[,3],
      i=i, j=j, k=k,
      opacity=0.3,
      intensity=rep(1, nrow(verts)),
      colorscale=list(c(0,"blue"), c(1,"blue")),
      showscale=FALSE,
      showlegend=FALSE
    )
  
  
  # equal aspect
  p = p %>% layout(scene = list(aspectmode="cube"))
  
  # Add invisible point so gridlines always show
  p = p %>% add_trace(
    type="scatter3d",
    mode="markers",
    x=0, y=0.5, z=0.5,
    marker=list(size=0, color="rgba(0,0,0,0)", line=list(width=0, color="rgba(0,0,0,0)")),
    showlegend=FALSE
  )
  
  
  
  
  return(p)
  
}

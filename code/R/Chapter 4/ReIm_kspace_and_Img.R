#if (!requireNamespace("BiocManager", quietly = TRUE))
#  install.packages("BiocManager")
#BiocManager::install("rhdf5")

library(rhdf5)
library(ggplot2)
source("functions/ft_functions.R")
path_figures = "../../figures/recon"

filename = 'data/k_slice.h5'

# Load real and imaginary parts separately and combine
k_real <- h5read(filename, "K_slice_real")
k_imag <- h5read(filename, "K_slice_imag")
k_slice <- k_real + 1i * k_imag


image(log(1+abs(k_fake)),
      col = gray.colors(256, start = 0, end = 1), 
      axes = FALSE, asp = 1)


filename = "k_imag.png"
image(abs(k_imag),
      col = gray.colors(256, start = 0, end = 1), 
      axes = FALSE, asp = 1)


img_slice <- fftshift(fft(fftshift(k_slice), inverse = TRUE))

img_plot <- t(img_slice)[, nrow(img_slice):1]


image(abs(Re(img_plot)),
      col = gray.colors(256, start = 0, end = 1), 
      axes = FALSE, asp = 1)


image(Im(img_plot),
col = gray.colors(256, start = 0, end = 1), 
axes = FALSE, asp = 1)

img_plot_2 = img_plot[1:(nrow(img_plot)/2),(ncol(img_plot)/2):ncol(img_plot)]

df = as.data.frame(as.table(Im(img_plot)))
ggplot(df, aes(Var1, Var2, fill = Freq)) +
  geom_raster() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", name = "") +
  theme_void()+
  coord_fixed(ratio = 0.75)

df = as.data.frame(as.table(Re(img_plot)))
ggplot(df, aes(Var1, Var2, fill = Freq)) +
  geom_raster() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", name = "") +
  theme_void()+
  coord_fixed(ratio = 0.75)

img_filtered = img_plot
img_filtered[abs(img_plot)<max(abs(bg))] = 0

bg=  img_plot[1:(nrow(img_plot)/2),(ncol(img_plot)/2):ncol(img_plot)]
df = as.data.frame(as.table(Arg(img_filtered)/pi))

#df = as.data.frame(as.table(Arg(img_filtered)))
ggplot(df, aes(Var1, Var2, fill = Freq)) +
  geom_raster() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", name = "",limits = c(-1,1)) +
  theme_void()+
  coord_fixed(ratio = 0.75)


image(Arg(img_plot),
col = gray.colors(256, start = 0, end = 1), 
axes = FALSE, asp = 1)


img_slice <- fftshift(fft(fftshift(k_real), inverse = TRUE))

img_plot <- t(img_slice)[, nrow(img_slice):1]

filename = "img_mag_re_k.png"
path_out = file.path(path_figures, filename)
png(path_out, width=800, height=800, bg = "transparent")
par(mar = c(0,0,0,0), oma = c(0,0,0,0))
image(abs(img_plot),
      col = gray.colors(256, start = 0, end = 1), 
      axes = FALSE, asp = 1)
dev.off()

img_slice <- fftshift(fft(fftshift(k_imag), inverse = TRUE))

img_plot <- t(img_slice)[, nrow(img_slice):1]

filename = "img_mag_im_k.png"
path_out = file.path(path_figures, filename)
png(path_out, width=800, height=800, bg = "transparent")
par(mar = c(0,0,0,0), oma = c(0,0,0,0))
image(abs(img_plot),
      col = gray.colors(256, start = 0, end = 1), 
      axes = FALSE, asp = 1)
dev.off()

k_fake = fftshift(fft(fftshift(
  abs(fftshift(fft(fftshift(k_slice), inverse = TRUE))))))


filename = "k_fake.png"
path_out = file.path(path_figures, filename)
png(path_out, width=800, height=800, bg = "transparent")
par(mar = c(0,0,0,0), oma = c(0,0,0,0))
image(abs(k_fake),
      col = gray.colors(256, start = 0, end = 1), 
      axes = FALSE, asp = 1)
dev.off()

img_rec = fftshift(fft(fftshift(abs(k_fake)), inverse = TRUE))

img_plot <- t(img_rec)[, nrow(img_rec):1]

filename = "img_from_fake_k.png"
path_out = file.path(path_figures, filename)
png(path_out, width=800, height=800, bg = "transparent")
par(mar = c(0,0,0,0), oma = c(0,0,0,0))
image(Im(img_plot),
      col = gray.colors(256, start = 0, end = 1), 
      axes = FALSE, asp = 1)
dev.off()

df = as.data.frame(as.table(asinh(Re(k_fake))))
ggplot(df, aes(Var1, Var2, fill = Freq)) +
  geom_raster() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", name = "",guide = "none") +
  theme_void()+
  coord_fixed(ratio = 1)


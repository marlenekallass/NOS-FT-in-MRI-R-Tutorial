#if (!requireNamespace("BiocManager", quietly = TRUE))
#  install.packages("BiocManager")
#BiocManager::install("rhdf5")

library(rhdf5)
source("functions/ft_functions.R")
path_figures = "../../figures/recon"

filename = 'data/k_slice.h5'

# Load real and imaginary parts separately and combine
k_real <- h5read(filename, "K_slice_real")
k_imag <- h5read(filename, "K_slice_imag")
k_slice <- k_real + 1i * k_imag


filename = "k_real.png"
path_out = file.path(path_figures, filename)
png(path_out, width=800, height=800, bg = "transparent")
par(mar = c(0,0,0,0), oma = c(0,0,0,0))
image(abs(k_real),
      col = gray.colors(256, start = 0, end = 1), 
      axes = FALSE, asp = 1)
dev.off()


filename = "k_imag.png"
path_out = file.path(path_figures, filename)
png(path_out, width=800, height=800, bg = "transparent")
par(mar = c(0,0,0,0), oma = c(0,0,0,0))
image(abs(k_imag),
      col = gray.colors(256, start = 0, end = 1), 
      axes = FALSE, asp = 1)
dev.off()

img_slice <- fftshift(fft(fftshift(k_slice), inverse = TRUE))

img_plot <- t(img_slice)[, nrow(img_slice):1]


filename = "img_Re.png"
path_out = file.path(path_figures, filename)
png(path_out, width=800, height=800, bg = "transparent")
par(mar = c(0,0,0,0), oma = c(0,0,0,0))
image(abs(Re(img_plot)),
      col = gray.colors(256, start = 0, end = 1), 
      axes = FALSE, asp = 1)
dev.off()


filename = "img_Im.png"
path_out = file.path(path_figures, filename)
png(path_out, width=800, height=800, bg = "transparent")
par(mar = c(0,0,0,0), oma = c(0,0,0,0))
image(abs(Im(img_plot)),
col = gray.colors(256, start = 0, end = 1), 
axes = FALSE, asp = 1)
dev.off()


filename = "img_arg.png"
path_out = file.path(path_figures, filename)
png(path_out, width=800, height=800, bg = "transparent")
par(mar = c(0,0,0,0), oma = c(0,0,0,0))
image(Arg(img_plot),
col = gray.colors(256, start = 0, end = 1), 
axes = FALSE, asp = 1)
dev.off()

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


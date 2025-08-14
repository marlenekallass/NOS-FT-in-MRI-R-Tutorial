# Install packages if not yet installed
#install.packages("BiocManager")
#BiocManager::install("rhdf5")

library(rhdf5)
source("functions/ft_functions.R")
path_figures = "../../figures/recon"

filename = 'data/k_slice.h5'

# Load real and imaginary parts separately and combine
k_real = h5read(filename, "K_slice_real")
k_imag = h5read(filename, "K_slice_imag")
k_slice = k_real + 1i * k_imag

# Visualize k-space
filename = "k_space_mag.png"
path_out = file.path(path_figures, filename)
png(path_out, width=800, height=800, bg = "transparent")
par(mar = c(0,0,0,0), oma = c(0,0,0,0))
image(abs(k_slice),
      col = gray.colors(256, start = 0, end = 1), 
      axes = FALSE, asp = 1)
dev.off()

# Inverse FFT with shifting
img_slice = fftshift(fft(fftshift(k_slice), inverse = TRUE))

# Because of how the image() function interprets matrix indices
# we need to switch x and y, and flip the y axis
img_plot = t(img_slice)[, nrow(img_slice):1]

# Show the magnitude image
filename = "img_mag.png"
path_out = file.path(path_figures, filename)
png(path_out, width=800, height=800, bg = "transparent")
par(mar = c(0,0,0,0), oma = c(0,0,0,0))
image(abs(img_plot),
      col = gray.colors(256, start = 0, end = 1), 
      axes = FALSE, asp = 1)
dev.off()


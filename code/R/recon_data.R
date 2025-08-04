# Install packages if not yet installed
#install.packages("BiocManager")
#BiocManager::install("rhdf5")

library(rhdf5)
source("functions/ft_functions.R")

filename = 'data/k_slice.h5'

# Load real and imaginary parts separately and combine
k_real <- h5read(filename, "K_slice_real")
k_imag <- h5read(filename, "K_slice_imag")
k_slice <- k_real + 1i * k_imag

# Visualize k-space
image(abs(k_slice),
      col = gray.colors(256, start = 0, end = 1), 
      axes = FALSE, asp = 1)

# Inverse FFT with shifting
img_slice <- fftshift(fft(fftshift(k_slice), inverse = TRUE))

# Because of how the image() function interprets matrix indices
# we need to switch x and y, and flip the y axis
img_slice <- t(img_slice)[, nrow(img_slice):1]

# Show the magnitude image
image(abs(img_slice),
      col = gray.colors(256, start = 0, end = 1), 
      axes = FALSE, asp = 1)


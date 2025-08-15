## A short script for simulating frequency encoding ##

# We need this for the fftshift() function
source('functions/ft_functions.R')

# variable parameters in this script:
# n, integer > 0, adjust size of mat accordingly
# values in mat, but retain the zero padding
# delta_f

## 1. Simulate a simple image ##

# Simulate an object
n = 5
mat = matrix(c(0, 0, 0, 0,0,
               0, 0.6, 1, 0.3, 0,
               0, 0.3, 0.6, 1, 0,
               0, 1, 0.6, 0.3, 0,
               0, 0, 0, 0, 0), nrow = n, byrow = TRUE)


# Plot the object
mat_plot = t(apply(mat, 2, rev)) # for correct orientation

image(mat_plot, col=gray(seq(0, 1, length=256)),
      axes=FALSE, useRaster=TRUE,asp=1)


## 2. Simulate a frequency gradient ##

# Define a linearly increasing frequency
delta_f = 1
f_max = delta_f*(n-1)

freq_x = seq(0, f_max, length.out = n) 


## 3. Sample the signal ##

dt = 1 / (2*f_max)  #  Sampling rate

t_max = 1/delta_f*1/2 # Sampling time

time = seq(-t_max, t_max, dt)

n_samples = length(time)

signals_px = array(0, dim = c(n,n,n_samples))

for (row_idx in 1:n) {
  for (col_idx in 1:n){
  amp = mat[row_idx, col_idx]
  signals_px[row_idx,col_idx,] = amp * cos(2 * pi * freq_x[col_idx] * time)
  }
}

kspace = apply(signals_px, 3, sum)

# Visualize it as k-space line
k_space_plot = matrix(rep(kspace, 2), ncol = 2, byrow = FALSE)

image(k_space_plot, col=gray(seq(0, 1, length=256)),
      axes=FALSE, useRaster=TRUE,asp=1/(2*n_samples))


## 4. Fourier Transform the signal ##

# Original signal (we can only differentiate columns)
img_colsum = apply(mat, 2, sum)
img_colsum_plot = matrix(rep(img_colsum, 2), ncol = 2, byrow = FALSE)

image(img_colsum_plot, col=gray(seq(0, 1, length=256)),
      axes=FALSE, useRaster=TRUE,asp=1/(2*n))

# Perform Fourier transform
fft_result = abs(fft(fftshift(kspace)))

# Take only positive frequencies
img_rec = fft_result[1: (length(fft_result)/2+1)] 

image_rec_plot = matrix(rep(img_rec, 2), ncol = 2, byrow = FALSE)

image(image_rec_plot, col=gray(seq(0, 1, length=256)),
      axes=FALSE, useRaster=TRUE,asp=1/(2*length(img_rec)))




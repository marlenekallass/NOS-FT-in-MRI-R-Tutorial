### A script for simulating Frequency Encoding in MRI ###
### See simulated_mri.R for a more detailed/variable version and all plots ###

# We need this for the fftshift() function
source('functions/ft_functions.R')

# variable parameters in this script:
# n, integer > 0 and odd, adjust size of mat accordingly
# values in mat, but retain the zero padding
# delta_f, integer >0

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
delta_f = 3
f_max = delta_f*(n-1)

# We can start at zero because the magnitude is zero there anyway
freq_x = seq(0, f_max, length.out = n) 

# These parameters are just to have a nice time interval for plotting
n_turns = 2
period = 1 / delta_f
n_samples_per_turn = 100
n_samples_total = n_samples_per_turn*n_turns
time_max = n_turns * period 

# Time interval
time = seq(0, time_max, length.out = n_samples_total)

# Get individual signals, now with frequency depending on x-position (column)
signals = array(0, dim = c(n,n,length(time)))

for (row_idx in 1:n) {
  for (col_idx in 1:n){
  amp = mat[row_idx, col_idx]
  signals[row_idx,col_idx,] = amp * cos(2 * pi * freq_x[col_idx] * time)
  }
}


# This is the complete measured signal

signal_sum = apply(signals, 3, sum)

plot(time,signal_sum, type="l")


## 3. Sample signal ##

dt = 1 / (2*f_max)  # Nyquist sampling rate

# Make a symmetric time window around 0
t_max = 1/delta_f*1/2

sampled_time = seq(-t_max, t_max, dt)

n_samples = length(sampled_time)

sampled_signals = array(0, dim = c(n,n,n_samples))

for (row_idx in 1:n) {
  for (col_idx in 1:n){
  amp = mat[row_idx, col_idx]
  sampled_signals[row_idx,col_idx,] = amp * cos(2 * pi * freq_x[col_idx] * sampled_time)
  }
}

sampled_signal = apply(sampled_signals, 3, sum)

# Visualize it as k-space line
k_space_line = matrix(rep(sampled_signal, 2), ncol = 2, byrow = FALSE)

image(k_space_line, col=gray(seq(0, 1, length=256)),
      axes=FALSE, useRaster=TRUE,asp=1/(2*n_samples))


## 4. Fourier Transform the signal ##

# Original signal (we can only differentiate columns)
img_colsum = apply(mat, 2, sum)
image_line = matrix(rep(img_colsum, 2), ncol = 2, byrow = FALSE)

image(image_line, col=gray(seq(0, 1, length=256)),
      axes=FALSE, useRaster=TRUE,asp=1/(2*n))

# Perform Fourier transform
fft_result = abs(fft(fftshift(sampled_signal)))

# Take only positive frequencies

img_rec = fft_result[1: (length(fft_result)/2+1)] 

image_rec_plot = matrix(rep(img_rec, 2), ncol = 2, byrow = FALSE)

image(image_rec_plot, col=gray(seq(0, 1, length=256)),
      axes=FALSE, useRaster=TRUE,asp=1/(2*length(img_rec_plot)))




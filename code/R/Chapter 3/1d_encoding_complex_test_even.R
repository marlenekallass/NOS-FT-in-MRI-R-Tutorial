source("functions/ft_functions.R")
## 1. Simulate a simple image ##

# Simulate an object. Can be any n x n matrix with signal strengths >=0

n_object_px = 2
  imaging_object = matrix(c(0.6, 1,
                        0.3, 0.6), nrow = n_object_px, byrow = TRUE)
n_object_px =256
imaging_object = abs(test)

n_object_px = 2
imaging_object = matrix(c(0.6, 1,
                          0.3, 0.6), nrow = n_object_px, byrow = TRUE)
  



n_object_px = 2
imaging_object = matrix(c(0.6, 1,
                          0.3, 0.6), nrow = n_object_px, byrow = TRUE)


n_object_px = 3  
imaging_object = matrix(c(0.6, 1, 0.3,
                          0.3, 0.6, 1,
                          1,   0.6, 0.3), nrow = n_object_px, byrow = TRUE)

n_object_px = 7
imaging_object = matrix(c(0.6, 1, 0.3,0.6, 1, 0.3,1,
                          0.3, 0.6, 1,0.3, 0.6, 1,0.3,
                          1,   0.6, 0.3,1,1,   0.6, 0.3,
                          0.6, 1, 0.3,0.6, 1, 0.3,1,
                          0.3, 0.6, 1,0.3, 0.6, 1,0.3,
                          1,   0.6, 0.3,1,1,   0.6, 0.3,
                          0.3, 0.6, 1,0.3, 0.6, 1,0.3), nrow = n_object_px, byrow = TRUE)
n_object_px = 6
imaging_object = matrix(c(0.6, 1,0.3,0.3,1,1,
                          0.3, 0.6,1,0.1,0.3,0.6,
                          0.6,1,0.3,1,0.6,1,
                          0.3,1,1,1,0.3,0.3,
                          0.6,1,0.3,1,0.6,1,
                          0.6,1,0.3,1,0.6,1), nrow = n_object_px, byrow = TRUE)


n_object_px= 6
imaging_object = matrix(c( 0.6, 0.6,0.6, 0.3,0.3,0.3,
                           0.6, 0.6,0.6, 0.3,0.3,0.3,
                           0.6, 0.6,0.6, 0.3,0.3,0.3,
                0.3,0.3,0.3,1, 1, 1,
                0.3,0.3,0.3,1, 1, 1,
                0.3,0.3,0.3,1, 1, 1), nrow = n_object_px, byrow = TRUE)
n_object_px= 4
imaging_object = matrix(c( 0.6, 0.6,0.3,0.3,
                           0.6, 0.6,0.3,0.3,
                           0.3,0.3, 1, 1,
                           0.3,0.3, 1, 1), nrow = n_object_px, byrow = TRUE)
n_object_px= 8
imaging_object = matrix(c( 0.6, 0.6,0.6,0.6, 0.3,0.3,0.3,0.3,
                           0.6, 0.6,0.6,0.6, 0.3,0.3,0.3,0.3,
                           0.6, 0.6,0.6,0.6, 0.3,0.3,0.3,0.3,
                           0.6, 0.6,0.6,0.6, 0.3,0.3,0.3,0.3,
                           0.3,0.3,0.3,0.3,1, 1, 1,1,
                           0.3,0.3,0.3,0.3,1, 1, 1,1,
                           0.3,0.3,0.3,0.3,1, 1, 1,1,
                           0.3,0.3,0.3,0.3,1, 1, 1,1), nrow = n_object_px, byrow = TRUE)



pad_matrix = function(mat, padding_size) {
  kronecker(mat, matrix(1, nrow = padding_size, ncol = padding_size))
}

# Example 2×2 object
n_object_px = 6
imaging_object = matrix(c(0.01,0.01,0.01,0.01,0.01,0.01,
                          0.01,0.6, 0.3,1,0.3,0.01,
                          0.01,0.3, 1.0,0.3,0.6,0.01,
                         0.01,0.5,1,0.3,0.3,0.01,
                          0.01,1,1,0.3,0.3,0.01,
                         0.01,0.01,0.01,0.01,0.01,0.01),
                        nrow = n_object_px, byrow = TRUE)

n_object_px = 6
imaging_object = matrix(c(0.6, 1,0.3,0.3,1,1,
                          0.3, 0.6,1,0.1,0.3,0.6,
                          0.6,1,0.3,1,0.6,1,
                          0.3,1,1,1,0.3,0.3,
                          0.6,1,0.3,1,0.6,1,
                          0.6,1,0.3,1,0.6,1), nrow = n_object_px, byrow = TRUE)
n_object_px = 8
imaging_object = matrix(c( 0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.3,
                            0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.3,
                          0.3,0.3, 0.3, 1, 0.3,0.3,0.3,0.3,
                          0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.3,
                          0.3, 0.3, 0.3,0.3,0.3,0.3,0.3,0.3,
                          0.3, 0.3, 0.3,0.3,0.3,0.3,0.3,0.3,
                          0.3, 0.3, 0.3,0.3,0.3,0.3,0.3,0.3,
                          0.3, 0.3, 0.3,0.3,0.3,0.3,0.3,0.3), nrow = n_object_px, byrow = TRUE)


padding_size = 1
imaging_object_padded = pad_matrix(imaging_object, padding_size)

imaging_object_padded
n_object_px = n_object_px*padding_size
imaging_object = imaging_object_padded

n_object_px= 4
imaging_object = matrix(c( 0.6, 0.6,0.3,0.3,
                           0.6, 1,0.3,0.3,
                           0.3,1, 1, 1,
                           0.3,0.3, 1, 1), nrow = n_object_px, byrow = TRUE)
n_object_px= 3
imaging_object = matrix(c( 0.6, 0.6,0.3,
                           0.6, 1,0.3,
                           0.3,1, 1), nrow = n_object_px, byrow = TRUE)



#imaging_object = imaging_object*10
# Add some zero padding
# Makes things a little easier because we can ignore 
# the case where frequency = 0, because we have magnitude = 0 there

#padding_size = 4
n = n_object_px + padding_size*2
mat = matrix(0, nrow = n, ncol = n)  
mat[(1:nrow(imaging_object)) + padding_size, (1:ncol(imaging_object)) + padding_size] = imaging_object

# Get those entries which contain the object
object_idx =  which(mat != 0, arr.ind = TRUE)
object_idx = object_idx[order(object_idx[, 1], object_idx[, 2]), ]

# Plot the object
mat_plot = t(apply(mat, 2, rev)) # for correct orientation 

image(mat_plot, col=gray(seq(0, 1, length=256)),
      axes=FALSE, useRaster=TRUE,asp=1)



# Define a linearly increasing frequency
delta_f = 1
f_max = delta_f*(n-1)*1/2
freq_x = seq(-f_max, f_max, length.out =n)

# If n is even, we need to shift the gradient,
# So that the zero frequency exists
# Otherwise fft() is confused
if (n %% 2 == 0) {
  freq_x = freq_x - delta_f/2
  f_max = f_max+delta_f/2
}


## 4. Sample signal ##
dt = 1 / (2*f_max)  # Sampling rate

# Make a symmetric time window around 0
t_max = 1/delta_f*1/2-dt/2


#t_max = dt/2+(n/2-1)*dt

time_sampled = seq(-t_max, t_max, dt)


#time_sampled = seq(0,t_max*2,dt)

#time_sampled = seq(0,t_max*2,dt)
n_samples = length(time_sampled)


# Plot all the signals
signals_px = array(0, dim = c(n,n,n_samples))
#signals_px_plot = array(0, dim = c(n,n,n_samples_plot))


for (idx in 1:n_object_px^2) {
  row_idx = object_idx[idx, 1]
  col_idx = object_idx[idx, 2]
  amp = mat[row_idx, col_idx]
  signals_px[row_idx,col_idx,] = amp * (cos(2 * pi * freq_x[col_idx] * time_sampled)-i*sin(2 * pi * freq_x[col_idx] * time_sampled))
 # signals_px_plot[row_idx,col_idx,] = amp * (cos(2 * pi * freq_x[col_idx] * time_plot)-i*sin(2 * pi * freq_x[col_idx] * time_plot))
  
}

kspace = apply(signals_px, 3, sum)

# Visualize it in a familiar way
k_space_plot = matrix(rep(abs(kspace), 2), ncol = 2, byrow = FALSE)

image(k_space_plot, col=gray(seq(0, 1, length=256)),
      axes=FALSE, useRaster=TRUE,asp=1/(2*n_samples))

kspace_shifted = abs(fftshift1D(kspace))
k_space_plot = matrix(rep(abs(kspace_shifted), 2), ncol = 2, byrow = FALSE)

image(k_space_plot, col=gray(seq(0, 1, length=256)),
      axes=FALSE, useRaster=TRUE,asp=1/(2*n_samples))

## 5. Fourier Transform the signal ##

# Original signal
img_colsum = apply(mat, 2, sum)
img_colsum_plot = matrix(rep(img_colsum, 2), ncol = 2, byrow = FALSE)

image(img_colsum_plot, col=gray(seq(0, 1, length=256)),
      axes=FALSE, useRaster=TRUE,asp=1/(2*n/padding_size))

fft_result = fft(fftshift(kspace))
#fft_result = fft(kspace)
#img_rec = abs(fftshift(fft_result))
img_rec = abs(fftshift1D(fft(fftshift1D(kspace),inverse = TRUE)))
#img_rec = abs(fftshift(fft(fftshift(kspace),inverse = TRUE)))

#img_rec = abs(fftshift1D(fft(kspace,inverse = TRUE)))

#img_rec = fft_result
# the spectrum is mirrored, choose one half 
#img_rec = fft_result[1: ((n_samples+1)/2)] 

image_rec_plot = matrix(rep(img_rec, 2), ncol = 2, byrow = FALSE)

image(image_rec_plot, col=gray(seq(0, 1, length=256)),
      axes=FALSE, useRaster=TRUE,asp=1/(2*length(img_rec)))
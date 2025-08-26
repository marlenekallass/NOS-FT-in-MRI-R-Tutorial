### Simulate a full 2d k-space and reconstruct a simulated image ###
source('functions/ft_functions.R')
source('functions/misc_utils.R')
install_and_load("ggplot2")
## 1. Simulate the object ##

n = 6
mat = matrix(c(0,   0,   0,   0,   0, 0,
               0, 0.3,   1, 0.3, 0.6, 0,
               0, 0.6,   1,   1,   1, 0,
               0, 0.3, 0.6, 0.3, 0.3, 0,
               0, 0.3, 0.6,   1, 0.3, 0,
               0,   0,   0,   0,   0, 0), nrow = n, byrow = TRUE)

mat = apply(mat,2,rev)

# Plot object:

# Convert to data frame for ggplot
df = expand.grid(x = 1:ncol(mat_plot), y = 1:nrow(mat_plot))
df$val = as.vector(mat_plot)

# Plot in ggplot
ggplot(df, aes(x = x, y = y, fill = val)) +
  geom_raster(interpolate = FALSE) +
  scale_fill_gradient(low = "black", high = "white") +
  coord_fixed(expand = FALSE) +
  theme_void() +
  theme(legend.position = "none")


## 2. Define gradient ##

delta_f = 1
f_max = delta_f*(n-1)

# Gradient in space
freq_x = seq(0, f_max, length.out = n) 

# Sampling rate
dt = 1 / (2*f_max)  

# Sample a full period
t_max = 1/(2*delta_f)

time_sampled = seq(-t_max, t_max-dt, dt)

n_samples = length(time_sampled)

## 3. Measure the signal over time ##

# Initialize array
kspace = array(0,dim=c(n_samples,n_samples))
signals_px = array(0,dim=c(n,n))

# Vary "x-time" 
for (t_x in 1:n_samples){
  # Vary "y-time" 
  for (t_y in 1:n_samples) {
    
    # Loop over all pixel
    for (row_idx in 1:n) {
      for (col_idx in 1:n){
        
        # Get Amplitude from the image
        amp = mat[row_idx, col_idx]
        
        # Signal for that pixel at time [t_x,t_y]
        signals_px[row_idx,col_idx] = amp * cos(2 * pi * (freq_x[col_idx] * time_sampled[t_x]
                                         + freq_x[row_idx]*time_sampled[t_y]))

      }
    }
    
    # Sum individual signals, put in k-space matrix
    # y --> rows, x --> columns
    kspace[t_y,t_x] = sum(signals_px)
    
  }
}

## 4. Plot k-space ##

df = expand.grid(x = 1:ncol(kspace), y = 1:nrow(kspace))
df$val = as.vector(t(kspace))

# Plot in ggplot
ggplot(df, aes(x = x, y = y, fill = val)) +
  geom_raster(interpolate = FALSE) +
  scale_fill_gradient(low = "black", high = "white") +
  coord_fixed(expand = FALSE) +
  theme_void() +
  theme(legend.position = "none")


## 5. Reconstruct image ##

# Perform Fourier transform
fft_result = Re(fft(fftshift(kspace)))

# Take only positive frequencies
img_rec = fft_result[1: (ncol(fft_result)/2+1),1: (nrow(fft_result)/2+1)] 

#Plot the reconstructed image
df = expand.grid(x = 1:ncol(img_rec), y = 1:nrow(img_rec))
df$val = as.vector(t(img_rec))

# Plot in ggplot
ggplot(df, aes(x = x, y = y, fill = val)) +
  geom_raster(interpolate = FALSE) +
  scale_fill_gradient(low = "black", high = "white") +
  coord_fixed(expand = FALSE) +
  theme_void() +
  theme(legend.position = "none")


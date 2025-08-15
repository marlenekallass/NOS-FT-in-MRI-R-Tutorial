### Simulate a full 2d k-space and reconstruct a simulated image ###

## 1. Simulate the object ##

n = 5
mat = matrix(c(0, 0, 0, 0,0,
               0, 0.6, 1, 0.3, 0,
               0, 0.3, 0.6, 1, 0,
               0, 1, 0.6, 0.3, 0,
               0, 0, 0, 0, 0), nrow = n, byrow = TRUE)

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
t_max = 1/delta_f*1/2
sampled_time = seq(-t_max, t_max, dt)

n_samples = length(sampled_time)

## 3. Measure the signal over time ##

# Initialize array
kspace = array(0,dim=c(n_samples,n_samples))

# Vary "x-time" 
for (t_x in 1:n_samples){
  # Vary "y-time" 
  for (t_y in 1:n_samples) {
    
    #Initialize vector
    amps = numeric(n^2)
    idx = 1
    
   
    # Loop over all pixel
    for (row_idx in 1:n) {
      for (col_idx in 1:n){
        
        # Get Amplitude from the image
        amp = mat[row_idx, col_idx]
        
        # Signal for that pixel at time [t_x,t_y]
        amps[idx] = amp * cos(2 * pi * (freq_x[col_idx] * sampled_time[t_x]
                                         + freq_x[row_idx]*sampled_time[t_y]))
        
        idx = idx+1
      }
    }
    
    # Sum individual signals, put in k-space matrix
    # y --> rows, x --> columns
    kspace[t_y,t_x] = sum(amps)
    
  }
}

## 4. Plot k-space ##

kspace_plot = t(apply(kspace, 2, rev)) 

df = expand.grid(x = 1:ncol(kspace_plot), y = 1:nrow(kspace_plot))
df$val = as.vector(kspace_plot)

# Plot in ggplot
ggplot(df, aes(x = x, y = y, fill = val)) +
  geom_raster(interpolate = FALSE) +
  scale_fill_gradient(low = "black", high = "white") +
  coord_fixed(expand = FALSE) +
  theme_void() +
  theme(legend.position = "none")


## 5. Reconstruct image ##

# Perform Fourier transform
fft_result = abs(fft(fftshift(kspace)))

# Take only positive frequencies
img_rec = fft_result[1: (ncol(fft_result)/2+1),1: (nrow(fft_result)/2+1)] 

# For correct orientation when plotting
img_rec_plot = t(apply(img_rec, 2, rev)) 

#Plot the reconstructed image
df = expand.grid(x = 1:ncol(img_rec_plot), y = 1:nrow(img_rec_plot))
df$val = as.vector(img_rec_plot)

# Plot in ggplot
ggplot(df, aes(x = x, y = y, fill = val)) +
  geom_raster(interpolate = FALSE) +
  scale_fill_gradient(low = "black", high = "white") +
  coord_fixed(expand = FALSE) +
  theme_void() +
  theme(legend.position = "none")


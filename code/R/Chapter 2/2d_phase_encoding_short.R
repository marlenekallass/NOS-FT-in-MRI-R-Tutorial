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


## 2. Define gradient in space and time ##

# Frequency
#delta_f_freq_encoding = 2

delta_f_max = 2

T_grad = 1/2*1/delta_f_max

delta_f_steps = delta_f_max/(n-1)

delta_f = seq(-delta_f_max,delta_f_max,delta_f_steps)

n_samples = length(delta_f)
# Our vector of gradient strengths (= phase encoding)
#delta_f = seq(-delta_f_max,delta_f_max,length.out = n_samples)

# Maximum frequency at pixel n
delta_f_FOV = (n-1)*delta_f

# How frequency varies in space (1 to n px) depending on gradient strength 
# We will use the same gradient strengths in x and y direction 
freq_x = t(sapply(delta_f_FOV, function(f) seq(0, f, length.out = n)))


## 3. Measure the signal (at T_grad) for each gradient strength ##

# Initialize array
kspace = array(0,dim=c(n_samples,n_samples))

# Vary x gradient
for (f_idx_x in 1:n_samples){
  # Vary y gradient
  for (f_idx_y in 1:n_samples) {
    
    #Initialize vector
    amps = numeric(n^2)
    idx = 1
    
    # Loop over all pixel
    for (row_idx in 1:n) {
      for (col_idx in 1:n){
        
        # Get Amplitude from the image
        amp = mat[row_idx, col_idx]
      
        # Get phase at time point T_grad
        # The frequencies from x and y gradients simply add up
        phase = 2* (freq_x[f_idx_x,col_idx]+freq_x[f_idx_y,row_idx])*T_grad
        
        # Amplitude at time point T_grad
        amps[idx] = amp*cos(pi*phase)
        idx = idx+1
        }
    }
    
    # Sum individual signals, put in k-space matrix
    # y --> rows, x --> columns
    kspace[f_idx_y,f_idx_x] = sum(amps)
    
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


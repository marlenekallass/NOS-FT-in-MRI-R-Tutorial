# Phase encoding gradient

delta_f_max = 2

T_grad = 1/(2*delta_f_max)

delta_f_steps = delta_f_max/(n-1)

delta_f = seq(-delta_f_max,delta_f_max,delta_f_steps)

n_samples = length(delta_f)

delta_f_FOV = (n-1)*delta_f

grad_phase = t(sapply(delta_f_FOV, function(f) seq(0, f, length.out = n)))

# Frequency encoding gradient
# Assuming delta_f_max is our maximum available "power"
grad_freq = grad_phase[n_samples,]

sampled_time = seq(-T_grad, T_grad, length.out = n_samples)

# Initialize array
kspace = array(0,dim=c(n_samples,n_samples))

# Phase encoding steps 
for (f_idx in 1:n_samples){
  ## Vary y gradient
  #for (f_idx_y in 1:n_samples) {
    
    #Initialize vector
    amps = array(0,dim=c(n^2,n_samples))
    idx = 1
    
    # Loop over all pixel
    for (row_idx in 1:n) {
      for (col_idx in 1:n){
        
        # Get Amplitude from the image
        amp = mat[row_idx, col_idx]
        
        # Get phase at time point T_grad
        # The frequencies from x and y gradients simply add up
        phase = 2*(grad_phase[f_idx,row_idx]*T_grad + grad_freq[col_idx]*sampled_time)
        
        # Amplitude at time point T_grad
        amps[idx,] = amp*cos(pi*phase)
        idx = idx+1
      }
    }
    
    # Sum individual signals, put in k-space matrix
    # y --> rows, x --> columns
    kspace[f_idx,] = colSums(amps)
    
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





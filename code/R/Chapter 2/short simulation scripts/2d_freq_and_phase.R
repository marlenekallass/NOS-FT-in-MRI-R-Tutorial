n = 6
mat = matrix(c(0,   0,   0,   0,   0, 0,
               0, 0.3,   1, 0.3, 0.6, 0,
               0, 0.6,   1,   1,   1, 0,
               0, 0.3, 0.6, 0.3, 0.3, 0,
               0, 0.3, 0.6,   1, 0.3, 0,
               0,   0,   0,   0,   0, 0), nrow = n, byrow = TRUE)

mat = apply(mat,2,rev)

image(t(mat), col=gray(seq(0, 1, length=256)),
      axes=FALSE, useRaster=TRUE,asp=1)

# Phase encoding gradient

delta_f_max = 1

T_grad = 1/(2*delta_f_max)

delta_f_step = delta_f_max/(n-1)

delta_f = seq(-delta_f_max,delta_f_max-delta_f_step/2,delta_f_step)

n_samples = length(delta_f)

f_max_vector = (n-1)*delta_f

grad_phase = t(sapply(f_max_vector, function(f) seq(0,f, length.out = n)))

# Frequency encoding gradient
# Assuming delta_f_max is our maximum available "power"
f_max = delta_f_max*(n-1)

# Gradient in space
grad_freq = seq(0, f_max, length.out = n) 

dt = 1 / (2*f_max)  


time_sampled = seq(-T_grad, T_grad-dt/2, dt)

# Initialize array
kspace = array(0,dim=c(n_samples,n_samples))
signals_px = array(0,dim=c(n,n,n_samples))

# Phase encoding steps 
for (f_idx in 1:n_samples){

    # Loop over all pixel
    for (row_idx in 1:n) {
      for (col_idx in 1:n){
        
        # Get Amplitude from the image
        amp = mat[row_idx, col_idx]
        
        # Get phase at time point T_grad
        # The frequencies from x and y gradients simply add up
        phase = 2*(grad_phase[f_idx,row_idx]*T_grad + grad_freq[col_idx]*time_sampled)
        
        # Amplitude at time point T_grad
        signals_px[col_idx,row_idx,] = amp*cos(pi*phase)
      }
    }
    
    # Sum individual signals, put in k-space matrix
    # y --> rows, x --> columns
    kspace[f_idx,] = apply(signals_px,3,sum)
    
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




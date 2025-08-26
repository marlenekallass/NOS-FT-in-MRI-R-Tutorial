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

mat = apply(mat,2,rev) # So row one = bottom left corner

# Plot object
df = expand.grid(x = 1:ncol(mat), y = 1:nrow(mat))
df$val = as.vector(t(mat))

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

T_grad = 1/(2*delta_f_max)

delta_f_step = delta_f_max/(n-1)

delta_f = seq(-delta_f_max,delta_f_max-delta_f_step/2,delta_f_step)

n_samples = length(delta_f)

# Maximum frequency at pixel n
f_max_vector = (n-1)*delta_f

# How frequency varies in space (1 to n px) depending on gradient strength 
# We will use the same gradient strengths in x and y direction 
freq_x = t(sapply(f_max_vector, function(f) seq(0, f, length.out = n)))


## 3. Measure the signal (at T_grad) for each gradient strength ##

# Initialize array
kspace = array(0,dim=c(n_samples,n_samples))
signals_px = array(0,dim=c(n,n))

# Vary x gradient
for (f_idx_x in 1:n_samples){
  # Vary y gradient
  for (f_idx_y in 1:n_samples) {
    
    # Loop over all pixel
    for (row_idx in 1:n) {
      for (col_idx in 1:n){
        
        # Get Amplitude from the image
        amp = mat[row_idx, col_idx]
      
        # Get phase at time point T_grad
        # The frequencies from x and y gradients simply add up
        phase = 2* (freq_x[f_idx_x,col_idx]+freq_x[f_idx_y,row_idx])*T_grad
        
        # Amplitude at time point T_grad
        signals_px[row_idx,col_idx] = amp*cos(pi*phase)
        }
    }
    
    # Sum individual signals, put in k-space matrix
    # y --> rows, x --> columns
    kspace[f_idx_y,f_idx_x] = sum(signals_px)
    
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


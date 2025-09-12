source('functions/plotting_functions.R')

i = complex(real = 0, imaginary = 1)
n = 6
mat = matrix(c(0,   0,   0,   0,   0, 0,
               0, 0.3,   1, 0.3, 0.6, 0,
               0, 0.6,   1,   1,   1, 0,
               0, 0.3, 0.6, 0.3, 0.3, 0,
               0, 0.3, 0.6,   1, 0.3, 0,
               0,   0,   0,   0,   0, 0), nrow = n, byrow = TRUE)

mat = apply(mat,2,rev)


# Phase encoding gradient
delta_f = 1

t_max = 1/(2*delta_f)


f_max = delta_f*n/2

grad_freq = seq(-f_max,delta_f*(n/2-1),length.out = n_samples)
dt = 1 / (2*f_max)  #  Sampling rate

time_sampled = seq(-t_max,t_max-dt/2,dt)

n_samples = length(time_sampled)

# See what happens if we slightly miss our sampling window

# Initialize array
kspace = array(0,dim=c(n_samples))
signals_px = array(0, dim = c(n,n,n_samples))


  # Loop over all pixel
  for (row_idx in 1:n) {
    for (col_idx in 1:n){
      
      # Get Amplitude from the image
      amp = mat[row_idx, col_idx]
      
      # Get phase at time point T_grad
      # The frequencies from x and y gradients simply add up
      phase_offset = 1
      phase = 2*grad_freq[col_idx]*time_sampled - phase_offset
      
      # Amplitude at time point T_grad
      signals_px[row_idx,col_idx,] = amp*(cos(pi*phase)-i*sin(pi*phase))
      
   
    }
  }
  
  # Sum individual signals, put in k-space matrix
  # y --> rows, x --> columns
  kspace =apply(signals_px,3,sum)
  



## 5. Reconstruct image ##

# Perform Fourier transform
fft_result = fftshift(fft(fftshift(kspace),inverse = TRUE))
img_rec = Re(fft_result)

#img_rec_plot = t(apply(img_rec, 2, rev)) 
#img_rec_plot = t(img_rec)
#Plot the reconstructed image
df = expand.grid(x = 1:length(img_rec), y = 1)
df$val = as.vector(t(img_rec))


# Plot in ggplot
ggplot(df, aes(x = x, y = y, fill = val)) +
  geom_raster(interpolate = FALSE) +
  scale_fill_gradient(low = "black", high = "white") +
  coord_fixed(expand = FALSE) +
  theme_void() +
  theme (legend.position = "none")



#library(plotly)
eye = list(x=1.2, y=-1.8, z=1.5)
plot_complex_imageline_3D(eye,fft_result)



# Phase encoding gradient

delta_f_max = 1

T_grad = 1/(2*delta_f_max)

delta_f_step = delta_f_max/(n-1)

delta_f = seq(-delta_f_max,delta_f_max-delta_f_step/2,delta_f_step)

n_samples = length(delta_f)

f_max_vector = (n-1)*delta_f

grad_phase = t(sapply(f_max_vector, function(f) seq(0, f, length.out = n)))

# Frequency encoding gradient
# Assuming delta_f_max is our maximum available "power"
f_max = delta_f_max*(n-1)

grad_freq = seq(0,f_max,length.out = n)

dt = 1/(2*f_max)

time_sampled = seq(-T_grad, T_grad-dt, dt)

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


# Mirrored image

img_rec = fft_result


#Plot the reconstructed image
df = expand.grid(x = 1:ncol(img_rec), y = 1:nrow(img_rec))
df$val = as.vector(t(img_rec))

freqs_fft = c(grad_freq, seq(-f_max+delta_f_max,-delta_f_max,delta_f_max))
labels_fft = as.character(freqs_fft)
labels_fft[which(freqs_fft == f_max)] = paste0("±", f_max)


# Plot in ggplot
ggplot(df, aes(x = x, y = y, fill = val)) +
  geom_raster(interpolate = FALSE) +
  scale_fill_gradient(low = "black", high = "white") +
  coord_fixed(expand = FALSE) +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none")+
  scale_x_continuous(
    breaks = (1:n_samples) ,   # ticks in middle of each pixel
    labels = labels_fft
  ) +
  xlab("Frequency [Hz]")

# Take only positive frequencies
img_rec = fft_result[1: (ncol(fft_result)/2+1),1: (nrow(fft_result)/2+1)] 


labels_fft = as.character(grad_freq)

#Plot the reconstructed image
df = expand.grid(x = 1:ncol(img_rec), y = 1:nrow(img_rec))
df$val = as.vector(t(img_rec))

# Plot in ggplot
ggplot(df, aes(x = x, y = y, fill = val)) +
  geom_raster(interpolate = FALSE) +
  scale_fill_gradient(low = "black", high = "white") +
  coord_fixed(expand = FALSE) +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none")+
  scale_x_continuous(
    breaks = (1:(n_samples/2+1)) ,   # ticks in middle of each pixel
    labels = labels_fft
  ) +
  xlab("Frequency [Hz]")


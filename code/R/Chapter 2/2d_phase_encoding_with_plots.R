### Simulate a full 2d k-space and reconstruct a simulated image ###

# Convert to data frame for ggplot
df = expand.grid(x = 1:ncol(mat_plot), y = 1:nrow(mat_plot))
df$val = as.vector(mat_plot)

# Plot in ggplot
p_object = ggplot(df, aes(x = x, y = y, fill = val)) +
  geom_raster(interpolate = FALSE) +
  scale_fill_gradient(low = "black", high = "white") +
  coord_fixed(expand = FALSE) +
  theme_void() +
  theme(legend.position = "none")
p_object

## 1. Phase encoding in both directions ##

# Pick some arbitrary frequency and gradient time
delta_f_freq_encoding = 1

period = 1 / delta_f_freq_encoding
T_grad = 2*period

# Number of samples depends on our image resolution
n_samples = (n-1)*2+1

# Choose sampling rate in a way that we always sample 1/2 period
delta_f_max = 1/2*delta_f_freq_encoding/T_grad 

# Gradient strength values
delta_f = seq(-delta_f_max,delta_f_max,length.out = n_samples)

delta_f_FOV = (n-1)*delta_f

# Frequencies over space in one direction
# We will use the same gradients in x and y direction 
freq_x = t(sapply(delta_f_FOV, function(f) seq(0, f, length.out = n)))

# Initialize array
kspace = array(0,dim=c(n_samples,n_samples))

for (f_idx_x in 1:n_samples){
  for (f_idx_y in 1:n_samples) {
  
  
  amps = numeric(n_object_px^2)
  for (px_idx in 1:n_object_px^2) {
    row_idx = object_idx[px_idx, 1]
    col_idx = object_idx[px_idx, 2]
    amp = mat[row_idx, col_idx]
    
    phase = 2* (freq_x[f_idx_x,col_idx]+freq_x[f_idx_y,row_idx])*T_grad
    amps[px_idx] = amp*cos(pi*phase)
    
  }
  
  kspace[f_idx_y,f_idx_x] = sum(amps)
  
}
}

kspace_plot = t(apply(kspace, 2, rev)) 

#Plot k-space
df = expand.grid(x = 1:ncol(kspace_plot), y = 1:nrow(kspace_plot))
df$val = as.vector(kspace_plot)

# Plot in ggplot
p_kspace = ggplot(df, aes(x = x, y = y, fill = val)) +
  geom_raster(interpolate = FALSE) +
  scale_fill_gradient(low = "black", high = "white") +
  #coord_fixed(expand = FALSE) +
  theme_void() +
 # labs(x = expression("Gradient strength"~Delta*f[x]~"[Hz]"), y =  expression("Gradient strength"~Delta*f[y]~"[Hz]")) +
  #labs(x = expression("Phase"~phi[x]~"["*pi*"]"), y =  expression("Phase"~phi[y]~"["*pi*"]")) +
  labs(x = expression("Spatial frequency"~k[x]~"[1/px]"), y =  expression("Spatial frequency"~k[y]~"[1/px]")) +
 # labs(x = expression("Time [s]"), y =  expression("Gradient strength"~Delta*f[y]~"[Hz]")) +
  #labs(x = expression("Spatial coordinate"~x), y =  expression("Spatial coordinate"~y)) +
 # labs(x = expression("Time"~t[x]~"[s]"), y =  expression("Time"~t[y]~"[s]")) +
   theme(
    legend.position = "none",
   axis.title.x = element_text(),
   axis.title.y = element_text(angle = 90)
  ) +
  annotate("segment", 
           x = 1, xend = ncol(kspace), 
           y = -0.3, yend = -0.3,
           arrow = arrow(length = unit(0.3, "cm")), colour = "black") +
  annotate("segment", 
           x = -0.3, xend = -0.3, 
           y = 1, yend = nrow(kspace),
           arrow = arrow(length = unit(0.3, "cm")), colour = "black")+
  coord_fixed(xlim = c(-0.5, ncol(kspace)+0.5 ), ylim = c(-0.5, nrow(kspace)+0.5), expand = FALSE) 
  

p_kspace

fft_result = abs(fft(fftshift(kspace)))

# Take only positive frequencies
img_rec = fft_result[1: (ncol(fft_result)/2+1),1: (nrow(fft_result)/2+1)] 

# For correct orientation when plotting
img_rec_plot = t(apply(img_rec, 2, rev)) 

#Plot the reconstructed image
df = expand.grid(x = 1:ncol(img_rec_plot), y = 1:nrow(img_rec_plot))
df$val = as.vector(img_rec_plot)


# Plot in ggplot
p_img_rec = ggplot(df, aes(x = x, y = y, fill = val)) +
  geom_raster(interpolate = FALSE) +
  scale_fill_gradient(low = "black", high = "white") +
  coord_fixed(expand = FALSE) +
  theme_void() +
  theme(legend.position = "none")

p_img_rec



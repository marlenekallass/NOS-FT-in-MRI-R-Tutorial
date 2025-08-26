### Simulate a full 2d k-space and reconstruct a simulated image ###
source('functions/misc_utils.R') #for install_and_load()
source('functions/ft_functions.R') #for fftshift()
source('functions/plotting_functions.R') 

install_and_load(c("ggplot2", "patchwork"))

# For saving plots
path_figures = "../../figures/Chapter 2/2D"
par(mar = c(0, 0, 0, 0))


n_object_px = 4
imaging_object = matrix(c(0.3,   1, 0.3, 0.6, 
                          0.6,   1,   1,   1, 
                          0.3, 0.6, 0.3, 0.3, 
                          0.3, 0.6,   1, 0.3), nrow = n_object_px, byrow = TRUE)


# Add some zero padding
# Makes things a little easier because we can ignore 
# the case where frequency = 0, because we have magnitude = 0 there

padding_size = 1
n = n_object_px + padding_size*2
mat = matrix(0, nrow = n, ncol = n)  
mat[(1:nrow(imaging_object)) + padding_size, (1:ncol(imaging_object)) + padding_size] = imaging_object

mat = apply(mat,2,rev) # We would like row 1 to correspond to bottom left of the image

# Get those entries which contain the object
object_idx =  which(mat != 0, arr.ind = TRUE)
object_idx = object_idx[order(object_idx[, 1], object_idx[, 2]), ]

# Convert to data frame for ggplot
df = expand.grid(x = 1:ncol(mat), y = 1:nrow(mat))
df$val = as.vector(t(mat))

# Plot in ggplot
ggplot(df, aes(x = x, y = y, fill = val)) +
  geom_raster(interpolate = FALSE) +
  scale_fill_gradient(low = "black", high = "white") +
  coord_fixed(expand = FALSE) +
  theme_void() +
  theme(legend.position = "none")


## 1. Phase encoding in both directions ##

# Pick some arbitrary frequency and gradient time
delta_f_max = 1.2

T_grad = 1/(2*delta_f_max)

# Number of samples depends on our image resolution
delta_f_step = delta_f_max/(n-1)

delta_f = seq(-delta_f_max,delta_f_max-delta_f_step/2,delta_f_step)


n_samples = length(delta_f) 

f_max_vector = (n-1)*delta_f

# Frequencies over space in one direction
# We will use the same gradients in x and y direction 
freq_x = t(sapply(f_max_vector, function(f) seq(0, f, length.out = n)))

# Initialize array
kspace = array(0,dim=c(n_samples,n_samples))
signals_px = array(0,dim=c(n,n))

for (f_idx_x in 1:n_samples){
  for (f_idx_y in 1:n_samples) {
  
  for (px_idx in 1:n_object_px^2) {
    row_idx = object_idx[px_idx, 1]
    col_idx = object_idx[px_idx, 2]
    amp = mat[row_idx, col_idx]
    
    phase = 2* (freq_x[f_idx_x,col_idx]+freq_x[f_idx_y,row_idx])*T_grad
    signals_px[col_idx,row_idx] = amp*cos(pi*phase)
    
  }
  
  kspace[f_idx_y,f_idx_x] = sum(signals_px)
  
}
}

#Plot k-space
df = expand.grid(x = 1:ncol(kspace), y = 1:nrow(kspace))
df$val = as.vector(t(kspace))

# Plot in ggplot
ggplot(df, aes(x = x, y = y, fill = val)) +
  geom_raster(interpolate = FALSE) +
  scale_fill_gradient(low = "black", high = "white") +
  theme_void() +
  labs(x = expression("Gradient strength"~Delta*f[x]~"[Hz]"), y =  expression("Gradient strength"~Delta*f[y]~"[Hz]")) +
 # labs(x = expression("Phase"~phi[x]~"["*pi*"]"), y =  expression("Phase"~phi[y]~"["*pi*"]")) +
 # labs(x = expression("Spatial frequency"~k[x]~"[1/px]"), y =  expression("Spatial frequency"~k[y]~"[1/px]")) +
 # labs(x = expression("Time [s]"), y =  expression("Gradient strength"~Delta*f[y]~"[Hz]")) +
 # labs(x = expression("Spatial coordinate"~x~"[px]"), y =  expression("Spatial coordinate"~y~"[px]")) +
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






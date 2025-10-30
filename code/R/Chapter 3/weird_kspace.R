i = complex(real = 0, imaginary = 1)
n = 6
mat = matrix(c(0,   0,   0,   0,   0, 0,
               0, 0.3,   1, 0.3, 0.6, 0,
               0, 0.6,   1,   1,   1, 0,
               0, 0.3, 0.6, 0.3, 0.3, 0,
               0, 0.3, 0.6,   1, 0.3, 0,
               0,   0,   0,   0,   0, 0), nrow = n, byrow = TRUE)

# Use this to create exactly the image from tutorial
random_phase = matrix(c( 7.989183, 6.808442, 6.189647, 7.040273, 5.300959, 6.786206,
                         9.788502, 8.044921, 7.133085, 9.376243, 7.513669, 6.221505,
                         9.317266, 9.250059, 6.287865, 9.812796, 9.671555, 9.483970,
                         5.235937, 8.606257, 9.379895, 9.245276, 6.843858, 8.501440,
                         6.014542, 8.588910, 9.310258, 9.779331, 8.318790, 9.248476,
                         6.830000, 7.932555, 9.129109, 5.463816, 6.260674, 5.947092), nrow = n, byrow = TRUE)


mat = apply(mat,2,rev)


# Phase encoding gradient
delta_f_max = 1

delta_f_steps = 2*delta_f_max/n # Double this 

T_grad = 1/(2*delta_f_max)

delta_f = seq(-delta_f_max,delta_f_max-delta_f_steps,delta_f_steps)

grad_phase = t(sapply(delta_f, function(f) seq(-n/2*f,(n/2-1)*f, length.out = n)))


n_samples = length(delta_f)


f_max = delta_f_max*n/2

grad_freq = seq(-f_max,delta_f_max*(n/2-1),length.out = n_samples)
dt = 1 / (2*f_max)  #  Sampling rate


time_sampled = seq(-T_grad,T_grad-dt/2,dt)


# See what happens if we slightly miss our sampling window

# Initialize array
kspace = array(0,dim=c(n_samples,n_samples))
signals_px = array(0, dim = c(n,n,n_samples))
random_phase = matrix(runif(n*n), n, n)

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
      
      # But we were inaccurate
      #phase = phase - runif(1)
      phase = phase - random_phase[row_idx,col_idx]

      
      # Amplitude at time point T_grad
      signals_px[row_idx,col_idx,] = amp*(cos(pi*phase)-i*sin(pi*phase))
      
    }
  }
  
  # Sum individual signals, put in k-space matrix
  # y --> rows, x --> columns
  kspace[f_idx,] =apply(signals_px,3,sum)
  
}


## 4. Plot k-space ##

#kspace_plot = t(apply(Arg(kspace), 2, rev)) 

kspace_plot =abs(kspace)

df = expand.grid(x = 1:ncol(kspace), y = 1:nrow(kspace))
df$val = as.vector(t(kspace_plot))
max_val = max(abs(df$val))

# Plot in ggplot
ggplot(df, aes(x = x, y = y, fill = val)) +
  geom_raster(interpolate = FALSE) +
  scale_fill_gradient(low = "black", high = "white") +
  theme_void() +
 # labs(x = expression("Time [s]"), y =  expression("Gradient strength"~Delta*f[y]~"[Hz]")) +
  theme(
    legend.position = "none",
    #axis.title.x = element_text(),
    #axis.title.y = element_text(angle = 90),
    #axis.text.y = element_text(),
   # axis.text.x = element_text(angle = 45)
    
  ) +
  coord_fixed(expand = FALSE)
#+
  # coord_fixed(xlim = c(-0.5, ncol(kspace)+0.5 ), ylim = c(-0.5, nrow(kspace)+0.5), expand = FALSE) +
 # scale_x_continuous(
  #  breaks = (1:n_samples) ,   # ticks in middle of each pixel
   # labels = round(time_sampled,2)
 # )+
  #scale_y_continuous(
   # breaks = (1:n_samples) ,   # ticks in middle of each pixel
  #  labels = round(delta_f,2)
 # )




## 5. Reconstruct image ##

# Perform Fourier transform
fft_result = fftshift(fft(fftshift(kspace),inverse = TRUE))

img_rec = abs(fft_result)

#img_rec_plot = t(apply(img_rec, 2, rev)) 
#img_rec_plot = t(img_rec)
#Plot the reconstructed image
df = expand.grid(x = 1:ncol(img_rec), y = 1:nrow(img_rec))
df$val = as.vector(t(img_rec))



labels_fft = as.character(grad_freq)


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



kspace2 =  fftshift(fft(fftshift(mat)))

tol = 1e-14

abs(kspace2-kspace)< tol
abs(abs(kspace2)-abs(kspace)) < tol




## Simulate a phase encoding gradient ##

source('functions/ft_functions.R')

# variable parameters in this script:
# n, integer > 0, adjust size of mat accordingly
# values in mat, but retain the zero padding
# delta_f

## 1. Simulate a simple image ##

# Simulate an object
n = 6
mat = matrix(c(0,   0,   0,   0,   0, 0,
               0, 0.3,   1, 0.3, 0.6, 0,
               0, 0.6,   1,   1,   1, 0,
               0, 0.3, 0.6, 0.3, 0.3, 0,
               0, 0.3, 0.6,   1, 0.3, 0,
               0,   0,   0,   0,   0, 0), nrow = n, byrow = TRUE)

# Plot the object
mat_plot = t(apply(mat, 2, rev)) # for correct orientation

image(mat_plot, col=gray(seq(0, 1, length=256)),
      axes=FALSE, useRaster=TRUE,asp=1)

### 1D Phase encoding ##

delta_f_max = 1

T_grad = 1/(2*delta_f_max)

delta_f_step = delta_f_max/(n-1)

delta_f = seq(-delta_f_max,delta_f_max-delta_f_step/2,delta_f_step)

n_samples = length(delta_f)

f_max_vector = (n-1)*delta_f

# How frequency varies in space (1 to n px) depending on gradient strength 
freq_x = t(sapply(f_max_vector, function(f) seq(0, f, length.out = n)))

kspace = numeric(n_samples)
signals_px = array(0, dim = c(n,n))

for (f_idx in 1:n_samples){
  
  
  
  for (row_idx in 1:n) {
    for (col_idx in 1:n) {
  
    amp = mat[row_idx, col_idx]
    
    phase = 2* freq_x[f_idx,col_idx]*T_grad
    signals_px[row_idx,col_idx] = amp*cos(pi*phase)
    idx = idx+1
    
    }
  }
  
  kspace[f_idx] = sum(signals_px)
  
}


fft_result = Re(fft(fftshift(kspace)))

# Take only positive frequencies
img_rec = fft_result[1: (length(fft_result)/2+1)] 

# For correct orientation when plotting
img_rec_plot = matrix(rep(img_rec, 2), ncol = 2, byrow = FALSE)

image(img_rec_plot, col=gray(seq(0, 1, length=256)),
      axes=FALSE, useRaster=TRUE,asp=1/(2*length(img_rec)))

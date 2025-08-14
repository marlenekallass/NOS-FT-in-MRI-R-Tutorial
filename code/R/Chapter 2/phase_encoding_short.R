## Simulate a phase encoding gradient ##
### 1D Phase encoding ##

delta_f_max = 1

T_grad = 1/(2*delta_f_max)

delta_f_steps = delta_f_max/(n-1)

delta_f = seq(-delta_f_max,delta_f_max,delta_f_steps)

n_samples = length(delta_f)

delta_f_FOV = (n-1)*delta_f

# How frequency varies in space (1 to n px) depending on gradient strength 
freq_x = t(sapply(delta_f_FOV, function(f) seq(0, f, length.out = n)))


kspace = numeric(n_samples)

for (f_idx in 1:n_samples){
  
  
  signals_px = numeric(n^2)
  idx = 1
  for (row_idx in 1:n) {
    for (col_idx in 1:n) {
  
    amp = mat[row_idx, col_idx]
    
    phase = 2* freq_x[f_idx,col_idx]*T_grad
    signals_px[idx] = amp*cos(pi*phase)
    idx = idx+1
    
    }
  }
  
  kspace[f_idx] = sum(signals_px)
  
}


fft_result = abs(fft(fftshift(kspace)))

# Take only positive frequencies
img_rec = fft_result[1: (length(fft_result)/2+1)] 

# For correct orientation when plotting
img_rec_plot = matrix(rep(img_rec, 2), ncol = 2, byrow = FALSE)

image(img_rec_plot, col=gray(seq(0, 1, length=256)),
      axes=FALSE, useRaster=TRUE,asp=1/(2*length(img_rec)))

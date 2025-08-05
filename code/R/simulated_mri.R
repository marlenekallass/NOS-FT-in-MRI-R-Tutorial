#A tiny MRI simulation script

#Needed
#A very simple image 
#x and y gradients
#k-space animation
#FT 

# 1. Simulate a simple image (3x3)

# Define grey values
mat <- matrix(c(0.5, 0.5, 0.05,
                0.05, 0.05, 0.5,
                0.5, 1, 0.05), nrow=3, byrow=TRUE)

base_mat <- matrix(c(0.5, 0.5, 0.05,
                     0.05, 0.05, 0.5,
                     0.5, 1, 0.05), nrow = 3, byrow = TRUE)

mat <- kronecker(base_mat, matrix(1, nrow = 100/3, ncol = 100/3))


mat <- matrix(rep(c(0.1, 1), each=5), nrow=100, ncol=100, byrow=TRUE)


mat = img_plot[100:149,150:199]


image(t(apply(mat, 2, rev)), col=gray(seq(0, 1, length=256)),
      axes=FALSE, useRaster=TRUE,asp=1)

# 2. Simulate a frequency gradient
n = dim(mat)[1]
g_max <- n
x_gradient <- seq(1, g_max, length.out = n)

# 3. Apply the gradient

freq_0 = 0
freq_x = freq_0 + x_gradient

n_turns = 80
period = 1 / max(freq_x)
steps_per_turn = 100
total_steps = steps_per_turn*n_turns+1
time_max = n_turns * period 
time = seq(-time_max, time_max, length.out = total_steps)

# Initialize array
signal_mat <- array(0, dim = c(n, n, length(time)))

# Fill array: arr[col, row, t] = mat[col, row] * cos(freq[col] * time[t])
for (col_idx in 1:n) {
  for (row_idx in 1:n) {
    signal_mat[row_idx, col_idx, ] <- mat[ row_idx, col_idx] * cos(2*pi*freq_x[col_idx] * time)
  }
}

signal_colsum <- apply(signal_mat, c(2, 3), sum)  # dim: (3 columns) × (time)


#plot
# Set up 1 row, 2 columns for plots
par(mfrow = c(1, 2))

# Colors for 9 curves
colors <- rainbow(n)

# Plot each arr[i,j,] vs time
plot(time, signal_colsum[1, ], type = "l", col = colors[1], ylim = range(signal_colsum), xlab = "Time", ylab = "Value")
k <- 2

for (col_idx in 2:n) {
 
    lines(time, signal_colsum[col_idx,], col = colors[col_idx])
    k <- k + 1
 #}
}
#legend("topright", legend = paste0("(", rep(1:3, each=3), ",", rep(1:3, 3), ")"), col = cols, lty = 1, cex = 0.7)

# Plot sum over all arr[i,j,] vs time
sum_signal <- apply(signal_mat, 3, sum)
plot(time, sum_signal, type = "l", xlab = "Time", ylab = "Sum")


fs <- 2 * max(freq_x)                # Sampling rate
dt <- 1 / fs                         # Time step

n_samples = ceiling(max(time)*2/dt)
if (n_samples %% 2 == 1) n_samples <- n_samples + 1
sampled_time <- seq(min(time), max(time), length.out=n_samples)
sampled_signal <- approx(time, sum_signal, xout = sampled_time)$y

plot(sampled_time, sampled_signal)



sampled_signal = sampled_signal[(n_samples/2-n+1):(n_samples/2+n)]
plot(sampled_signal)

fftshift1 = function(kspace) {  #
  # For better readability - could also be done via ncol() and nrow()
  rows = length(kspace) # Evaluate n of rows
 
  reshape_row = c((rows/2+1):rows, 1:(rows/2))  # rows/2+1 so it starts at first position second half
  # not last position of first half!
  # same here...
  kspace[reshape_row]               # reshape k-space
} # End of function fftshift()




img_recon = abs(fftshift1(fft(fftshift1(sampled_signal))))
img_recon = img_recon[(length(img_recon)/2+1):length(img_recon)]
plot(img_recon)

img_colsum <- colSums(mat)
plot(img_colsum)

### A script for simulating Frequency Encoding in MRI ###
source('functions/misc_utils.R')
source('functions/ft_functions.R')
install_and_load(c("ggplot2", "patchwork"))


## 1. Simulate a simple image ##

# Simulate an object. Can be any n x n matrix with signal strengths >=0
n_object_px <- 3  
imaging_object <- matrix(c(0.6, 1, 0.3,
                   0.3, 0.6, 1,
                   1,   0.6, 0.3), nrow = n_object_px, byrow = TRUE)

# Add some zero padding
padding_size = 1
n = n_object_px + padding_size*2
mat <- matrix(0, nrow = n, ncol = n)  
mat[(1:nrow(imaging_object)) + padding_size, (1:ncol(imaging_object)) + padding_size] <- imaging_object

# Get those entries which contain the object
object_idx =  which(mat != 0, arr.ind = TRUE)
object_idx <- object_idx[order(object_idx[, 1], object_idx[, 2]), ]

# Plot the object
mat_plot = t(apply(mat, 2, rev)) # for correct orientation 
image(mat_plot, col=gray(seq(0, 1, length=256)),
      axes=FALSE, useRaster=TRUE,asp=1)

## 2. Signal without gradient ##

freq_0 = 3 # some base frequency

# Select a good time interval for plotting
n_turns = 2
period = 1 / freq_0
n_samples_per_turn = 100
n_samples_total = n_samples_per_turn*n_turns
time_max = n_turns * period 
time = seq(0, time_max, length.out = n_samples_total)


# Generate signal plots for each pixel
colors <- scales::hue_pal()(n_object_px^2)
plot_list <- list()
signals <- array(0, dim = c(n,n,length(time)))

for (idx in 1:n_object_px^2) {
    row_idx <- object_idx[idx, 1]
    col_idx <- object_idx[idx, 2]
    amp <- mat[row_idx, col_idx]
    signals[row_idx,col_idx,] <- amp * cos(2 * pi * freq_0 * time)
    df <- data.frame(time,signal = signals[row_idx,col_idx,])
    
    p <- ggplot(df, aes(x = time, y = signal)) +
      geom_line(color = colors[idx], linewidth = 1.5) +
      ylim(-max(mat), max(mat)) +
      theme_minimal()
    
    plot_list[[idx]] <- p
}

grid_plot <- wrap_plots(plot_list, ncol = n_object_px, nrow = n_object_px)  # auto-adjusts layout
grid_plot

# Sum signal over all four components (from full `time`)
signal_sum <- apply(signals, 3, sum)
df_sum <- data.frame(time = time, value = signal_sum)
ggplot(df_sum, aes(x = time, y = value)) +
  geom_line(color = "black", size = 1.5) +
  ggtitle("Sum of all signals") +
  theme_minimal()

# The amplitude of signal_sum is just
amp_sum = sum(mat[object_idx])


## 3. Simulate a frequency gradient ##

# g_strength and g_max can be anything >0, but this is ensures integer values
g_strength = 1
g_max <- g_strength*(n-1)
freq_x <- seq(0, g_max, length.out = n)


# Plot all the signals
plot_list <- list()
signals <- array(0, dim = c(n,n,length(time)))

for (idx in 1:n_object_px^2) {
  row_idx <- object_idx[idx, 1]
  col_idx <- object_idx[idx, 2]
  amp <- mat[row_idx, col_idx]
  signals[row_idx,col_idx,] <- amp * cos(2 * pi * freq_x[col_idx] * time)
  df <- data.frame(time,signal = signals[row_idx,col_idx,])
  
  p <- ggplot(df, aes(x = time, y = signal)) +
    geom_line(color = colors[idx], linewidth = 1.5) +
    ylim(-max(mat), max(mat)) +
    theme_minimal()
  
  plot_list[[idx]] <- p
}

grid_plot <- wrap_plots(plot_list, ncol = n_object_px, nrow = n_object_px)  # auto-adjusts layout
grid_plot

# Sum over the columns
signal_sum_cols <- apply(signals, c(2, 3), sum)  # dim: [col_idx, time]

# Plot column sum
plot_list <- list()
for (idx in 1:n_object_px) {
  col_idx <- object_idx[idx, 2]
  df <- data.frame(time,signal = signal_sum_cols[col_idx,])
  
  p <- ggplot(df, aes(x = time, y = signal)) +
    geom_line(color = colors[idx], linewidth = 1.5) +
    theme_minimal() + 
  ylim(-max(signal_sum_cols), max(signal_sum_cols)) 
  plot_list[[idx]] <- p
}

grid_plot <- wrap_plots(plot_list, ncol = n_object_px)  # auto-adjusts layout
grid_plot


# Sum complete signal 

signal_sum <- apply(signals, 3, sum)
df_sum <- data.frame(time = time, value = signal_sum)
ggplot(df_sum, aes(x = time, y = value)) +
  geom_line(color = "black", size = 1.5) +
  ggtitle("Sum of all signals") +
  theme_minimal()

## 4. Sample signal ##

sample_factor = 2 # should be at least 2 for Nyquist

fs <- sample_factor * max(freq_x)/2 # max sampled frequency
dt <- 1 / (2*fs)  # sampling rate

# Adjust number samples according to sample factor
# so all frequencies in freq_x are correctly sampled
n_samples = (fs/g_strength)*2

# Make a symmetric array around 0
t_max <- n_samples/2 * dt - dt/2


sampled_time <- seq(-t_max, t_max, length.out = n_samples)

# Plot all the signals
sampled_signals <- array(0, dim = c(n,n,n_samples))

for (idx in 1:n_object_px^2) {
  row_idx <- object_idx[idx, 1]
  col_idx <- object_idx[idx, 2]
  amp <- mat[row_idx, col_idx]
  sampled_signals[row_idx,col_idx,] <- amp * cos(2 * pi * freq_x[col_idx] * sampled_time)
}

sampled_signal <- apply(sampled_signals, 3, sum)

# Visualize it in a familiar way
k_space_line <- matrix(rep(sampled_signal, 2), ncol = 2, byrow = FALSE)

image(k_space_line, col=gray(seq(0, 1, length=256)),
      axes=FALSE, useRaster=TRUE,asp=1/(2*n_samples))

## 5. Fourier Transform the signal ##

# Visualize it in a familiar way

img_colsum = apply(mat, 2, sum)
image_line <- matrix(rep(img_colsum, 2), ncol = 2, byrow = FALSE)

image(image_line, col=gray(seq(0, 1, length=256)),
      axes=FALSE, useRaster=TRUE,asp=1/(2*n))

fft_result = abs(fft(fftshift1(sampled_signal)))

# the spectrum is mirrored, choose one half 
img_rec = fft_result[0: (n_samples/2)+1]

image_rec_plot <- matrix(rep(img_rec, 2), ncol = 2, byrow = FALSE)

image(image_rec_plot, col=gray(seq(0, 1, length=256)),
      axes=FALSE, useRaster=TRUE,asp=1/(2*n))

### A script for simulating Frequency Encoding in MRI ###
source('functions/misc_utils.R')
source('functions/ft_functions.R')
install_and_load(c("ggplot2", "patchwork"))

path_figures = "../../figures/simulation"
par(mar = c(0, 0, 0, 0))

# variable parameters:
# values in imaging_object
# n_object_px, integer > 0, adjust imaging_object accordingly
# delta_f
# freq_0


## 1. Simulate a simple image ##

# Simulate an object. Can be any n x n matrix with signal strengths >=0
n_object_px = 3  
imaging_object = matrix(c(0.6, 1, 0.3,
                          0.3, 0.6, 1,
                          1,   0.6, 0.3), nrow = n_object_px, byrow = TRUE)


# Add some zero padding
# Makes things a little easier because we can ignore 
# the case where frequency = 0, because we have magnitude = 0 there

padding_size = 1
n = n_object_px + padding_size*2
mat = matrix(0, nrow = n, ncol = n)  
mat[(1:nrow(imaging_object)) + padding_size, (1:ncol(imaging_object)) + padding_size] = imaging_object

# Get those entries which contain the object
object_idx =  which(mat != 0, arr.ind = TRUE)
object_idx = object_idx[order(object_idx[, 1], object_idx[, 2]), ]

# Plot the object
mat_plot = t(apply(mat, 2, rev)) # for correct orientation 

image(mat_plot, col=gray(seq(0, 1, length=256)),
      axes=FALSE, useRaster=TRUE,asp=1)


## 2. Signal without gradient ##

freq_0 = 3 # Some base frequency

# These parameters are just to have a nice time interval for plotting
n_turns = 2
period = 1 / freq_0
n_samples_per_turn = 100
n_samples_total = n_samples_per_turn*n_turns
time_max = n_turns * period 
time = seq(0, time_max, length.out = n_samples_total)


# Generate signal plots for each pixel
colors = rainbow(n_object_px^2)
plot_list = list()
signals = array(0, dim = c(n,n,length(time)))

for (idx in 1:n_object_px^2) {
    row_idx = object_idx[idx, 1]
    col_idx = object_idx[idx, 2]
    amp = mat[row_idx, col_idx]
    signals[row_idx,col_idx,] = amp * cos(2 * pi * freq_0 * time)
    df = data.frame(time,signal = signals[row_idx,col_idx,])
    
    p = ggplot(df, aes(x = time, y = signal)) +
      geom_line(color = colors[idx], linewidth = 1.5) +
      ylim(-max(mat), max(mat)) +
      labs(x = NULL, y = NULL)+
      theme_minimal() +
      theme(
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        aspect.ratio = 1,
        panel.grid = element_blank()
      )
    
    plot_list[[idx]] = p
}

grid_plot = wrap_plots(plot_list, ncol = n_object_px, nrow = n_object_px)  # auto-adjusts layout
grid_plot

# This is the actual measured signal
signal_sum = apply(signals, 3, sum)
df_sum = data.frame(time = time, value = signal_sum)
ggplot(df_sum, aes(x = time, y = value)) +
  geom_line(color = "black", size = 1.5) +
  labs(x = 'Time', y = 'Amplitude')+
  theme_minimal()+
  theme(
    axis.text = element_blank()
  )

# The amplitude of the signal is just
sum(mat)


## 3. Simulate a frequency gradient ##

# Define a linearly increasing frequency
delta_f = 2
f_max = delta_f*(n-1)
freq_x = seq(0, f_max, length.out = n)

# Adapt time interval
n_turns = 0.75
period = 1 / delta_f
n_samples_per_turn = 100
n_samples_total = n_samples_per_turn*n_turns
time_max = n_turns * period 
time = seq(0, time_max, length.out = n_samples_total)


# Plot the individual signals, now with frequency depending on x-position
plot_list = list()
signals = array(0, dim = c(n,n,length(time)))

for (idx in 1:n_object_px^2) {
  row_idx = object_idx[idx, 1]
  col_idx = object_idx[idx, 2]
  amp = mat[row_idx, col_idx]
  signals[row_idx,col_idx,] = amp * cos(2 * pi * freq_x[col_idx] * time)
  df = data.frame(time,signal = signals[row_idx,col_idx,])
  
  p = ggplot(df, aes(x = time, y = signal)) +
    geom_line(color = colors[idx], linewidth = 1.5) +
    ylim(-max(mat), max(mat)) +
    labs(x = NULL, y = NULL)+
    theme_minimal() +
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      aspect.ratio = 1,
      panel.grid = element_blank(),
    ) 
  
  plot_list[[idx]] = p
}

grid_plot = wrap_plots(plot_list, ncol = n_object_px, nrow = n_object_px)  # auto-adjusts layout
grid_plot

# We now have n_object_px different frequency components
signal_sum_cols = apply(signals, c(2, 3), sum)  

# Plot the different frequency components
plot_list = list()
for (idx in 1:n_object_px) {
  col_idx = object_idx[idx, 2]
  df = data.frame(time,signal = signal_sum_cols[col_idx,])
  
  p = ggplot(df, aes(x = time, y = signal)) +
    geom_line(color = colors[idx], linewidth = 1.5) +
    theme_minimal() + 
    ylim(-max(signal_sum_cols), max(signal_sum_cols)) +
    labs(x = NULL, y = NULL)+
    theme_minimal() +
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      aspect.ratio = 1,
      panel.grid = element_blank()
    ) 
  
  plot_list[[idx]] = p
}

grid_plot = wrap_plots(plot_list, ncol = n_object_px)  
grid_plot


# Plot the complete measured sigal

signal_sum = apply(signals, 3, sum)
df_sum = data.frame(time = time, value = signal_sum)
ggplot(df_sum, aes(x = time, y = value)) +
  geom_line(color = "black", size = 1.5) +
  labs(x = 'Time', y = 'Amplitude')+
  theme_minimal()+
  theme(
  axis.text = element_blank()
  )


## 4. Sample signal ##

# We will now sample the signal at discrete time points
# We will choose these time points in a way that satisfies two conditions:
# I)  According to the Nyquist theorem, we need to sample at at least twice
#     the highest frequency component in the signal
# II) The length of time that we sample determines our resolution 
#     i.e. how well we can resolve frequencies between 0 and f_max.
#     Because in this simulation our object and gradient are discrete
#     we need to make sure we sample exactly the frequencies in freq_x.
#     This is not an issue in real life with continuous objects and signals


dt = 1 / (2*f_max)  # Nyquist sampling rate

# Make a symmetric time window around 0
t_max = 1/delta_f*1/2

sampled_time = seq(-t_max, t_max, dt)

n_samples = length(sampled_time)


sampled_time = seq(-t_max, t_max, length.out = n_samples)


# Plot all the signals
sampled_signals = array(0, dim = c(n,n,n_samples))

for (idx in 1:n_object_px^2) {
  row_idx = object_idx[idx, 1]
  col_idx = object_idx[idx, 2]
  amp = mat[row_idx, col_idx]
  sampled_signals[row_idx,col_idx,] = amp * cos(2 * pi * freq_x[col_idx] * sampled_time)
}

sampled_signal = apply(sampled_signals, 3, sum)

# Visualize it in a familiar way
k_space_line = matrix(rep(sampled_signal, 2), ncol = 2, byrow = FALSE)

image(k_space_line, col=gray(seq(0, 1, length=256)),
      axes=FALSE, useRaster=TRUE,asp=1/(2*n_samples))



## 5. Fourier Transform the signal ##

# Original signal
img_colsum = apply(mat, 2, sum)
image_line = matrix(rep(img_colsum, 2), ncol = 2, byrow = FALSE)

image(image_line, col=gray(seq(0, 1, length=256)),
      axes=FALSE, useRaster=TRUE,asp=1/(2*n))

fft_result = abs(fft(fftshift(sampled_signal)))

# the spectrum is mirrored, choose one half 
img_rec = fft_result[1: ((n_samples+1)/2)] 

image_rec_plot = matrix(rep(img_rec, 2), ncol = 2, byrow = FALSE)

image(image_rec_plot, col=gray(seq(0, 1, length=256)),
      axes=FALSE, useRaster=TRUE,asp=1/(2*length(img_rec)))

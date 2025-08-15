### Full script for simulating frequency encoding, with all figures ###
source('functions/misc_utils.R') #for install_and_load()
source('functions/ft_functions.R') #for fftshift()
install_and_load(c("ggplot2", "patchwork","latex2exp"))

# For saving plots
path_figures = "../../figures/Chapter 2"
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
signals_px = array(0, dim = c(n,n,length(time)))

for (idx in 1:n_object_px^2) {
    row_idx = object_idx[idx, 1]
    col_idx = object_idx[idx, 2]
    amp = mat[row_idx, col_idx]
    signals_px[row_idx,col_idx,] = amp * cos(2 * pi * freq_0 * time)
    df = data.frame(time,signal = signals_px[row_idx,col_idx,])
    
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

grid_plot = wrap_plots(plot_list, ncol = n_object_px, nrow = n_object_px)  
grid_plot

# This is the actual measured signal
signal_sum = apply(signals_px, 3, sum)
df_sum = data.frame(time = time, value = signal_sum)
ggplot(df_sum, aes(x = time, y = value)) +
  geom_line(color = "black", linewidth = 1.5) +
  labs(x = 'Time [s]', y = 'Signal')+
  theme_minimal()

## 3. Simulate a frequency gradient ##

# Define a linearly increasing frequency
delta_f = 1
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

grid_plot = wrap_plots(plot_list, ncol = n_object_px, nrow = n_object_px)  
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
  geom_line(color = "black", linewidth = 1.5) +
  labs(x = 'Time [s]', y = 'Signal')+
  theme_minimal()  

## 4. Sample signal ##

dt = 1 / (2*f_max)  # Sampling rate

# Make a symmetric time window around 0
t_max = 1/delta_f*1/2

time_sampled = seq(-t_max, t_max, dt)

n_samples = length(time_sampled)

n_samples_plot = 200

time_plot = seq(-t_max, t_max, length.out = n_samples_plot)


# Plot all the signals
signals_px = array(0, dim = c(n,n,n_samples))
signals_px_plot = array(0, dim = c(n,n,n_samples_plot))


for (idx in 1:n_object_px^2) {
  row_idx = object_idx[idx, 1]
  col_idx = object_idx[idx, 2]
  amp = mat[row_idx, col_idx]
  signals_px[row_idx,col_idx,] = amp * cos(2 * pi * freq_x[col_idx] * time_sampled)
  signals_px_plot[row_idx,col_idx,] = amp * cos(2 * pi * freq_x[col_idx] * time_plot)
  
  }

kspace = apply(signals_px, 3, sum)

# Visualize it in a familiar way
k_space_plot = matrix(rep(kspace, 2), ncol = 2, byrow = FALSE)

image(k_space_plot, col=gray(seq(0, 1, length=256)),
      axes=FALSE, useRaster=TRUE,asp=1/(2*n_samples))


## Display signal with sample points ##

# Total signal
signal_sum = apply(signals_px_plot, 3, sum)
df_sum = data.frame(time = time_plot, signal = signal_sum)

# Slowest and fasted signal

labels = c(
  "Measured signal",
  TeX("$\\cos(2\\pi\\cdot f_{\\max} \\cdot t)$"),
  TeX("$\\cos(2\\pi\\cdot \\Delta f \\cdot t)$")
)

colors = rainbow(2)

x_labels = seq(-t_max*delta_f*2, t_max*delta_f*2, 0.5)

p = ggplot(df_sum, aes(x = time, y = signal, color = "label1")) +
  geom_line(size = 1.5) +
  guides(color = guide_legend(position = "inside")) +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    legend.title = element_blank(),
    legend.position.inside = c(1, 1),
    legend.justification.inside = c("right", "top")
  ) +
  scale_color_manual(values = c("label1" = "black",
                                "label2" = colors[1],
                                "label3" = colors[2]),
                     breaks = c("label1", 
                                "label2",
                                "label3"),
                     labels = labels)+
  geom_hline(yintercept = 0, color = "black") +
  scale_x_continuous(
    breaks = seq(-t_max, t_max, length.out = length(x_labels)),
    labels = x_labels)  + 
  labs(x = expression(Phase~phi~"["*pi*"]"), y= 'Signal')



max_signal =  cos(2* pi * f_max * time_plot)
df_max = data.frame(time = time_plot, signal = max_signal)
p = p + geom_line(data = df_max, aes(x = time, y = signal, color = "label2"), 
                  size = 1,alpha = 0.3)

min_signal =  cos(2 * pi * delta_f * time_plot)
df_max = data.frame(time = time_plot, signal = min_signal)
p = p + geom_line(data = df_max, aes(x = time, y = signal, color = "label3"), size = 1,alpha = 0.3)


# Sampling lines

df_sampled = data.frame(time = time_sampled, signal = kspace)

p = p + geom_segment(aes(x = time, xend = time,
                         y = 0, yend = signal),
                     data = df_sampled,
                     linetype = "dashed", color = "red") +
  geom_point(aes(x = time, y = signal),
             data = df_sampled,
             color = "red", size = 2.5)
p



## Display the signal with subsignals ##

n_samples_plot = 200
t_max = 2*1/delta_f
time = seq(0,t_max, length.out = n_samples_plot)

signals = array(0, dim = c(n,n,n_samples_plot))

for (idx in 1:n_object_px^2) {
  row_idx = object_idx[idx, 1]
  col_idx = object_idx[idx, 2]
  amp = mat[row_idx, col_idx]
  signals[row_idx,col_idx,] = amp * cos(2 * pi * freq_x[col_idx] * time)
}

# Column and total signals
signal_sum_cols = apply(signals, c(2, 3), sum)  
signal_sum = apply(signals, 3, sum)

df_sum = data.frame(time = time, signal = signal_sum)

colors = rainbow(n_object_px)

# Total signal
p = ggplot(df_sum, aes(x = time, y = signal)) +
  geom_line(color = "black", size = 1.5) +
  labs(x = 'Time [t]', y = 'Signal')+
  theme_minimal()+
  theme(
    axis.text.y = element_blank()
  )


# Add the column signals
for (idx in 1:n_object_px) {
  col_idx = object_idx[idx, 2]
  df_signals = (data.frame(time = time, signal = signal_sum_cols[col_idx,]))
  p = p + geom_line(data = df_signals, aes(x = time, y = signal), color = colors[idx], size = 1,alpha = 0.3)
}


# Adjust the x-axis
# Time axis
time_labels = seq(0, t_max, length.out = 5)
label_vector = round(time_labels,1)
p + scale_x_continuous(
  breaks = time_labels,
  labels = label_vector,
  labs(x = 'Time [s]')) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  geom_vline(xintercept = t_max/2, linetype = "dashed", color = "black") +
  geom_vline(xintercept = t_max, linetype = "dashed", color = "black") +
  annotate("segment",
             x = 0-0.05*abs(t_max), xend = t_max+0.05*abs(t_max),
             y = min(signal_sum)-0.1*abs(min(signal_sum)), yend = min(signal_sum)-0.1*abs(min(signal_sum)),
             arrow = arrow(length = unit(0.2, "cm")))
  


# Phase axis
label_vector = seq(-2, 2, length.out = 5)
p + scale_x_continuous(
  breaks = time_labels,
  labels = label_vector)  + 
  labs(x = expression(Phase~phi~"["*pi*"]"))+
  theme(panel.grid = element_blank())+
  annotate("segment",
           x = 0-0.05*abs(t_max), xend = t_max+0.05*abs(t_max),
           y = min(signal_sum)-0.3*abs(min(signal_sum)), yend = min(signal_sum)-0.3*abs(min(signal_sum)),
           arrow = arrow(length = unit(0.2, "cm")))



## 5. Fourier Transform the signal ##

# Original signal
img_colsum = apply(mat, 2, sum)
img_colsum_plot = matrix(rep(img_colsum, 2), ncol = 2, byrow = FALSE)

image(img_colsum_plot, col=gray(seq(0, 1, length=256)),
      axes=FALSE, useRaster=TRUE,asp=1/(2*n))

fft_result = abs(fft(fftshift(kspace)))

# the spectrum is mirrored, choose one half 
img_rec = fft_result[1: ((n_samples+1)/2)] 

image_rec_plot = matrix(rep(img_rec, 2), ncol = 2, byrow = FALSE)

image(image_rec_plot, col=gray(seq(0, 1, length=256)),
      axes=FALSE, useRaster=TRUE,asp=1/(2*length(img_rec)))

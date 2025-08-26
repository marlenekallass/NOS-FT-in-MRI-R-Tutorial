### Full script for simulating frequency encoding, with all figures ###
source('functions/misc_utils.R') #for install_and_load()
source('functions/ft_functions.R') #for fftshift()
install_and_load(c("ggplot2", "patchwork","latex2exp"))

# For saving plots
path_figures = "../../figures/Chapter 2/frequency encoding"
par(mar = c(0, 0, 0, 0))

## 1. Simulate a simple image ##

# Simulate an object. Can be any n x n matrix with signal strengths >=0
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

# So that row 1 corresponds to bottom left
mat = apply(mat,2,rev)

# Get those entries which contain the object
object_idx =  which(mat != 0, arr.ind = TRUE)
object_idx = object_idx[order(object_idx[, 1], object_idx[, 2]), ]

# Plot the object
df = expand.grid(x = 1:n, y = 1:n)
df$val = as.vector(t(mat))

# Plot in ggplot
ggplot(df, aes(x = x, y = y, fill = val)) +
  geom_raster(interpolate = FALSE) +
  scale_fill_gradient(low = "black", high = "white") +
  theme_void() +
 labs(x = "x", y =  "y") +
   theme(
    legend.position = "none",
    axis.title.x = element_text(),
    axis.title.y = element_text(angle = 90)
  ) +
  annotate("segment", 
           x = 0.5, xend = n+0.5, 
           y = 0, yend = 0,
           arrow = arrow(length = unit(0.3, "cm")), colour = "black") +
  annotate("segment", 
           x = 0, xend = 0, 
           y = 0.5, yend = n+0.5,
           arrow = arrow(length = unit(0.3, "cm")), colour = "black")+
  coord_fixed(xlim = c(-0.2, n+0.5 ), ylim = c(-0.2, n+0.5), expand = FALSE) 



## 2. Signal without gradient ##

freq_0 = 3 # Some base frequency

# These parameters are just to have a nice time interval for plotting
n_turns = 2
period = 1 / freq_0
n_samples_per_turn = 100
n_samples_total = n_samples_per_turn*n_turns
t_max = n_turns * period 
time = seq(0, t_max, length.out = n_samples_total)

time = seq(0,t_max, length.out = 100)
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

wrap_plots(plot_list, ncol = n_object_px, nrow = n_object_px)  & 
  theme(plot.background  = element_rect(fill = "transparent", colour = NA),
  panel.background = element_rect(fill = "transparent", colour = NA))
  


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
t_max = n_turns * period 
time = seq(0, t_max, length.out = n_samples_total)


# Plot the individual signals, now with frequency depending on x-position
plot_list = list()
signals_px = array(0, dim = c(n,n,length(time)))

for (idx in 1:n_object_px^2) {
  row_idx = object_idx[idx, 1]
  col_idx = object_idx[idx, 2]
  amp = mat[row_idx, col_idx]
  signals_px[row_idx,col_idx,] = amp * cos(2 * pi * freq_x[col_idx] * time)
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
      panel.grid = element_blank(),
    ) 
  
  plot_list[[idx]] = p
}

wrap_plots(plot_list, ncol = n_object_px, nrow = n_object_px)  & theme(
  plot.background  = element_rect(fill = "transparent", colour = NA),
  panel.background = element_rect(fill = "transparent", colour = NA))
  

# We now have n_object_px different frequency components
signal_sum_cols = apply(signals_px, c(2, 3), sum)  

colors = rainbow(n_object_px)

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

wrap_plots(plot_list, ncol = n_object_px)  & theme(
  plot.background  = element_rect(fill = "transparent", colour = NA),
  panel.background = element_rect(fill = "transparent", colour = NA))
  

# Plot the complete measured sigal

signal_sum = apply(signals_px, 3, sum)
df = data.frame(time = time, value = signal_sum)
ggplot(df, aes(x = time, y = value)) +
  geom_line(color = "black", linewidth = 1.5) +
  labs(x = 'Time [s]', y = 'Signal')+
  theme_minimal()  

## 4. Sample signal ##

dt = 1 / (2*f_max)  # Sampling rate

# Make a symmetric time window around 0
t_max = 1/delta_f*1/2

time_sampled = seq(-t_max, t_max-dt, dt)

n_samples = length(time_sampled)

n_samples_plot = 200
t_max_plot = t_max 

time_plot = seq(-t_max_plot, t_max_plot, length.out = n_samples_plot)


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

df = expand.grid(x = 1:length(kspace), y = 1)
df$val = as.vector(kspace)

# Plot as kspace line
ggplot(df, aes(x = x, y = y, fill = val)) +
  geom_raster(interpolate = FALSE) +
  scale_fill_gradient(low = "black", high = "white") +
  coord_fixed(expand = FALSE) +
  theme_void() +
  theme(legend.position = "none")



## Display signal with sample points ##

labels = c(
  "Measured signal",
  TeX("$\\cos(2\\pi\\cdot f_{\\max} \\cdot t)$"),
  TeX("$\\cos(2\\pi\\cdot \\Delta f \\cdot t)$")
)

colors = rainbow(2)

x_labels = seq(-t_max_plot*delta_f*2, t_max_plot*delta_f*2, 0.5)

# Total signal
signal_sum = apply(signals_px_plot, 3, sum)
df = data.frame(time = time_plot, signal = signal_sum)

p = ggplot(df, aes(x = time, y = signal, color = "label1")) +
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
    breaks = seq(-t_max_plot, t_max_plot, length.out = length(x_labels)),
    labels = x_labels)  + 
  labs(x = expression(Phase~phi~"["*pi*"]"), y= 'Signal')

# Slowest and fasted signal

max_signal =  cos(2* pi * f_max * time_plot)
df = data.frame(time = time_plot, signal = max_signal)
p = p + geom_line(data = df, aes(x = time, y = signal, color = "label2"), 
                  size = 1,alpha = 0.3)

min_signal =  cos(2 * pi * delta_f * time_plot)
df = data.frame(time = time_plot, signal = min_signal)
p = p + geom_line(data = df, aes(x = time, y = signal, color = "label3"), size = 1,alpha = 0.3)


# Sampling lines
df = data.frame(time = time_sampled, signal = kspace)
p = p + geom_segment(aes(x = time, xend = time,
                         y = 0, yend = signal),
                     data = df,
                     linetype = "dashed", color = "red") +
  geom_point(aes(x = time, y = signal),
             data = df,
             color = "red", size = 2.5)
p


## Display the signal with subsignals ##

n_samples_plot = 200
t_max = 2*1/delta_f
time = seq(0,t_max, length.out = n_samples_plot)

signals_px = array(0, dim = c(n,n,n_samples_plot))

for (idx in 1:n_object_px^2) {
  row_idx = object_idx[idx, 1]
  col_idx = object_idx[idx, 2]
  amp = mat[row_idx, col_idx]
  signals_px[row_idx,col_idx,] = amp * cos(2 * pi * freq_x[col_idx] * time)
}

# Column and total signals
signal_sum_cols = apply(signals_px, c(2, 3), sum)  
signal_sum = apply(signals_px, 3, sum)


colors = rainbow(n_object_px)

# Total signal
df = data.frame(time = time, signal = signal_sum)
p = ggplot(df, aes(x = time, y = signal)) +
  geom_line(color = "black", size = 1.5) +
  labs(x = 'Time [t]', y = 'Signal')+
  theme_minimal()+
  theme(
    axis.text.y = element_blank()
  )


# Add the column signals
for (idx in 1:n_object_px) {
  col_idx = object_idx[idx, 2]
  df = (data.frame(time = time, signal = signal_sum_cols[col_idx,]))
  p = p + geom_line(data = df, aes(x = time, y = signal), color = colors[idx], size = 1,alpha = 0.3)
}


# Adjust the x-axis

# Time axis
labels_time = seq(0, t_max, length.out = 5)
label_vector = round(labels_time,1)
p + scale_x_continuous(
  breaks = labels_time,
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
  breaks = labels_time,
  labels = label_vector)  + 
  labs(x = expression(Phase~phi~"["*pi*"]"))+
  theme(panel.grid = element_blank())+
  annotate("segment",
           x = 0-0.05*abs(t_max), xend = t_max+0.05*abs(t_max),
           y = min(signal_sum)-0.3*abs(min(signal_sum)), yend = min(signal_sum)-0.3*abs(min(signal_sum)),
           arrow = arrow(length = unit(0.2, "cm")))



## 5. Fourier Transform the signal ##

# Shift kspace
kspace_shifted = fftshift(kspace)

df = expand.grid(x = 1:length(kspace_shifted), y = 1)
df$val = as.vector(kspace_shifted)

# Plot in ggplot
ggplot(df, aes(x = x, y = y, fill = val)) +
  geom_raster(interpolate = FALSE) +
  scale_fill_gradient(low = "black", high = "white") +
  coord_fixed(expand = FALSE) +
  theme_void() +
  theme(legend.position = "none")



# Original signal
img_colsum = apply(mat, 2, sum)

df = expand.grid(x = 1:length(img_colsum), y = 1)
df$val = as.vector(img_colsum)

# Plot in ggplot
ggplot(df, aes(x = x, y = y, fill = val)) +
  geom_raster(interpolate = FALSE) +
  scale_fill_gradient(low = "black", high = "white") +
  coord_fixed(expand = FALSE) +
  theme_void() +
  theme(legend.position = "none")

# Whole spectrum
fft_result = Re(fft(kspace_shifted))

df = expand.grid(x = 1:length(fft_result), y = 1)
df$val = as.vector(fft_result)

freqs_fft = c(seq(0,f_max,delta_f), seq(-f_max+delta_f,-delta_f,delta_f))
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

# Choose positive frequencies 
img_rec = fft_result[1: (n_samples/2+1)] 

df = expand.grid(x = 1:length(img_rec), y = 1)
df$val = as.vector(img_rec)

# Plot in ggplot
ggplot(df, aes(x = x, y = y, fill = val)) +
  geom_raster(interpolate = FALSE) +
  scale_fill_gradient(low = "black", high = "white") +
  coord_fixed(expand = FALSE) +
  theme_void() +
  theme(legend.position = "none")

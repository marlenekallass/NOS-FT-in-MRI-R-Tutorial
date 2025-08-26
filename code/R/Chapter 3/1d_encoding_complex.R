source("functions/ft_functions.R")
## 1. Simulate a simple image ##

# Simulate an object. Can be any n x n matrix with signal strengths >=0

n_object_px = 4
imaging_object = matrix(c(0.3,   1, 0.3, 0.6, 
                          0.6,   1,   1,   1, 
                          0.3, 0.6, 0.3, 0.3, 
                          0.3, 0.6,   1, 0.3), nrow = n_object_px, byrow = TRUE)




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



# Define a linearly increasing frequency
delta_f = 1
f_max = delta_f*(n-1)*1/2
freq_x = seq(-f_max, f_max, length.out = n)

# If n is even, we need to shift the gradient,
# So that the zero frequency exists
# Otherwise fft() is confused
if (n %% 2 == 0) {
 freq_x = freq_x - delta_f/2
  f_max = f_max+delta_f/2
}

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
colors = rainbow(n_object_px^2)

for (idx in 1:n_object_px^2) {
  row_idx = object_idx[idx, 1]
  col_idx = object_idx[idx, 2]
  amp = mat[row_idx, col_idx]
  signals[row_idx,col_idx,] = amp *( cos(2 * pi * freq_x[col_idx] * time)- i*sin(2*pi*freq_x[col_idx]*time))
#  df = data.frame(time,signal_real = Re(signals[row_idx,col_idx,]),signal_im = Im(signals[row_idx,col_idx,]))
  
  #p = ggplot(df, aes(x = time, y = signal_real)) +
   # geom_line(color = colors[idx], linewidth = 1.5) +
    #geom_line(aes(x = time, y = signal_im), color = colors[idx], linewidth = 0.5,alpha = 0.5) +
    #ylim(-max(mat), max(mat)) +
    #labs(x = NULL, y = NULL)+
    #theme_minimal() +
    #theme(
    #  axis.text = element_blank(),
    #  axis.ticks = element_blank(),
    #  aspect.ratio = 1,
    #  panel.grid = element_blank(),
#    ) 
  
 # plot_list[[idx]] = p
}

#grid_plot = wrap_plots(plot_list, ncol = n_object_px, nrow = n_object_px)  
#grid_plot

# We now have n_object_px different frequency components
signal_sum_cols = apply(signals, c(2, 3), sum)  

# Plot the different frequency components
plot_list = list()
for (idx in 1:n_object_px) {
  col_idx = object_idx[idx, 2]
  df = data.frame(time,signal_real = Re(signal_sum_cols[col_idx,]), signal_im = Im(signal_sum_cols[col_idx,]))
  
  p = ggplot(df, aes(x = time, y = signal_real)) +
    geom_line(color = colors[idx], linewidth = 1.5) +
    geom_line(aes(x = time, y = signal_im), color = colors[idx], linewidth = 0.5) +
    theme_minimal() + 
    ylim(-max(Re(signal_sum_cols)), max(Re(signal_sum_cols))) +
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
df_sum = data.frame(time = time, signal_real = Re(signal_sum), signal_im = Im(signal_sum))
ggplot(df_sum, aes(x = time, y = signal_real)) +
  geom_line(color = "black", linewidth = 1.5) +
  geom_line(aes(x = time,y = signal_im),color = "black", linewidth = 1.5)+
  labs(x = 'Time [s]', y = 'Signal')+
  theme_minimal()  

## 4. Sample signal ##
dt = 1 / (2*f_max)  # Sampling rate

# Make a symmetric time window around 0
t_max = 1/delta_f*1/2-dt/2

time_sampled = seq(-t_max, t_max, dt)


#time_sampled = seq(0,t_max*2,dt)

#time_sampled = seq(0,t_max*2,dt)
n_samples = length(time_sampled)


# Plot all the signals
signals_px = array(0, dim = c(n,n,n_samples))
#signals_px_plot = array(0, dim = c(n,n,n_samples_plot))


for (idx in 1:n_object_px^2) {
  row_idx = object_idx[idx, 1]
  col_idx = object_idx[idx, 2]
  amp = mat[row_idx, col_idx]
  signals_px[row_idx,col_idx,] = amp * (cos(2 * pi * freq_x[col_idx] * time_sampled)-i*sin(2 * pi * freq_x[col_idx] * time_sampled))
 # signals_px_plot[row_idx,col_idx,] = amp * (cos(2 * pi * freq_x[col_idx] * time_plot)-i*sin(2 * pi * freq_x[col_idx] * time_plot))
  
}

kspace = apply(signals_px, 3, sum)

# Visualize it in a familiar way
k_space_plot = matrix(rep(Im(kspace), 2), ncol = 2, byrow = FALSE)

image(k_space_plot, col=gray(seq(0, 1, length=256)),
      axes=FALSE, useRaster=TRUE,asp=1/(2*n_samples))

## 5. Fourier Transform the signal ##

# Original signal
img_colsum = apply(mat, 2, sum)
img_colsum_plot = matrix(rep(img_colsum, 2), ncol = 2, byrow = FALSE)

image(img_colsum_plot, col=gray(seq(0, 1, length=256)),
      axes=FALSE, useRaster=TRUE,asp=1/(2*n))

fft_result = fftshift1D(fft(fftshift1D(kspace),inverse = TRUE))
img_rec = abs(fft_result) 
# Need to do this because Im(fft_result) /= 0, 
# Because of our even n_samples!

image_rec_plot = matrix(rep(img_rec, 2), ncol = 2, byrow = FALSE)

image(image_rec_plot, col=gray(seq(0, 1, length=256)),
      axes=FALSE, useRaster=TRUE,asp=1/(2*length(img_rec)))
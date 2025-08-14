## OPTIONAL: Additional plots, execute after simulated_mri script##
library(latex2exp)

## Sampling the analogue signal ##
# Use t_max from the original script
time = seq(-t_max, t_max, length.out = 1000)

signals = array(0, dim = c(n,n,length(time)))

for (idx in 1:n_object_px^2) {
  row_idx = object_idx[idx, 1]
  col_idx = object_idx[idx, 2]
  amp = mat[row_idx, col_idx]
  signals[row_idx,col_idx,] = amp * cos(2 * pi * freq_x[col_idx] * time)
}


# Column and total signals

colors = rainbow(2)

signal_sum = apply(signals, 3, sum)
df_sum = data.frame(time = time, value = signal_sum)


labels <- expression(
  "Measured signal",
  cos(2 * pi * max(f) * t),
  cos(2 * pi * Delta * f * t)
)

labels <- c(
  "Measured signal",
  TeX("$\\cos(2\\pi\\cdot \\max(f) \\cdot t)$"),
  TeX("$\\cos(2\\pi\\cdot \\Delta f \\cdot t)$")
)

x_labels = seq(-t_max*delta_f*2, t_max*delta_f*2, 0.5)

p = ggplot(df_sum, aes(x = time, y = value, color = "label1")) +
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
  labs(x = expression(Phase~phi~"["*pi*"]"), y= 'Amplitude')





max_signal =  cos(2* pi * f_max * time)
df_max = data.frame(time = time, value = max_signal)
p = p + geom_line(data = df_max, aes(x = time, y = value, color = "label2"), 
                  size = 1,alpha = 0.3)

min_signal =  cos(2 * pi * delta_f * time)
df_max = data.frame(time = time, value = min_signal)
p = p + geom_line(data = df_max, aes(x = time, y = value, color = "label3"), size = 1,alpha = 0.3)


# Sampling lines

df_sampled = data.frame(sampled_time, sampled_signal)

p = p + geom_segment(aes(x = sampled_time, xend = sampled_time,
                         y = 0, yend = sampled_signal),
                     data = df_sampled,
                     linetype = "dashed", color = "red") +
  geom_point(aes(x = sampled_time, y = sampled_signal),
             data = df_sampled,
             color = "red", size = 2.5)
p


## Displaying the signal with all subsigals
# Finer time sample for plotting
dt = 1 /(2* max(freq_x[1:length(freq_x)-padding_size]))  # sampling rate
n_samples = n_object_px*sample_factor+1
n_samples = n_object_px*sample_factor
t_max = (n-1) * sample_factor/2* dt 
t_max = (n_samples/2-1)*dt + dt/2
time = seq(-t_max,t_max,sample_factor*t_max/100)

signals = array(0, dim = c(n,n,length(time)))

for (idx in 1:n_object_px^2) {
  row_idx = object_idx[idx, 1]
  col_idx = object_idx[idx, 2]
  amp = mat[row_idx, col_idx]
  signals[row_idx,col_idx,] = amp * cos(2 * pi * freq_x[col_idx] * time)
}

# Column and total signals
signal_sum_cols = apply(signals, c(2, 3), sum)  
signal_sum = apply(signals, 3, sum)

df_sum = data.frame(time = time, value = signal_sum)

colors = rainbow(n_object_px)

# Total signal
p = ggplot(df_sum, aes(x = time, y = value)) +
  geom_line(color = "black", size = 1.5) +
  labs(x = 'Time', y = 'Amplitude')+
  theme_minimal()+
  theme(
    axis.text.y = element_blank()
  )
p
# Add the column signals

for (idx in 1:n_object_px) {
  col_idx = object_idx[idx, 2]
  df = data.frame(time,signal = signal_sum_cols[col_idx,])
  df_signal = (data.frame(time = time, value = signal_sum_cols[col_idx,]))
  p = p + geom_line(data = df_signal, aes(x = time, y = value), color = colors[idx], size = 1,alpha = 0.3)
}

p = p +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  geom_vline(xintercept = -t_max, linetype = "dashed", color = "black") +
  geom_vline(xintercept = t_max, linetype = "dashed", color = "black") 
p

# Adjust the x-axis

label_vector = round(seq(0, 2*t_max, by = dt),1)
p + scale_x_continuous(
  breaks = seq(-t_max, t_max, by = dt),
  labels = label_vector,
  labs(x = 'Time [s]')
)

label_vector = seq(-2, 2, length.out = 5)
p + scale_x_continuous(
  breaks = seq(-t_max, t_max, length.out = 5),
  labels = label_vector
)  + 
  labs(x = expression(Phase~phi~"["*pi*"]"))

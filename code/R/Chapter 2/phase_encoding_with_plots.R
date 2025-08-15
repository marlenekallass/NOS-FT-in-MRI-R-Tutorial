source('functions/misc_utils.R') #for install_and_load()
source('functions/ft_functions.R') #for fftshift()
source('functions/plotting_functions.R') 

install_and_load(c("ggplot2", "patchwork"))

## Phase general gif ##

freq = 3
period = 1 / freq
t_max = period
time = seq(-t_max, t_max, length.out = 100)


fraction_labels <- c(
  "scriptstyle(0)",
  "scriptstyle('1/4')", 
  "scriptstyle('1/2')", 
  "scriptstyle('3/4')", 
  "scriptstyle(1)",
  "scriptstyle('5/4')", 
  "scriptstyle('3/2')", 
  "scriptstyle('7/4')", 
  "scriptstyle(2)"
)

# Phase steps
n_phase_steps = 9
phases = seq(0, 2 , length.out = n_phase_steps)

plots = list()


p = ggplot(signal_df, aes(x = time, y = signal)) +
  geom_hline(yintercept = 0, color = "grey50") +
  geom_vline(xintercept = 0, color = "grey50") +
  labs(x = "Time", y = "Signal") +
  ylim(-1.1, 1.1) +
  xlim(-t_max,t_max)+
  theme_minimal() +
  theme(
    axis.text = element_blank()
  ) 



for (idx in 1:n_phase_steps) {
  phase = phases[idx]
  
  # Label for annotation
  if (phase == 0) {
    label_text = paste("phi == scriptstyle(0)")
    
  }
  
  else if  (phase == 1)  {
    label_text <- "phi == pi"
    
  }
  else {
    label_text <- paste("phi == ", fraction_labels[idx], " * pi")
  }
  
  # Cosine wave with phase shift
  signal = cos(2 * pi * freq * time + pi*phase)
  
  signal_df = data.frame(time = time, signal = signal)
  
  # Vertical line to peak
  t_peak = -phase / (2 * freq_0)
  vertical_line = data.frame(x = t_peak,y =1)
  
  p_signal = p +
    geom_line(data = signal_df, color = "black") +
    geom_segment(data = vertical_line, aes(x = x, xend = x, y = 0, yend = 1), color = "blue") +
    annotate("text", x =- max(time)/4, y = 0.9, label = label_text, parse = TRUE, size = 6) 
    
    if (phase!= 0){
      p_signal = p_signal +
       annotate("segment",
          x = t_peak, xend = 0,
          y = 0.5, yend = 0.5,
          color = "green",
          arrow = arrow(ends = "last", length = unit(0.2, "cm")))
    }
    

  plots[[idx]] = p_signal
}

filename = "phase_encoding_1.gif"
path_out = file.path(path_figures, filename)
#create_gif_from_plots(plots, path_out, 800, 600, 200, fps = 1)

## Frequency phase relationship ##

# First, let's say we only have two pixels
n = 2

# And some base frequency
freq_0 = 4

# The gradient 
delta_f = 1.5
f_max = delta_f*(n-1)
grad_x = seq(-f_max, f_max, length.out = n)
freq_x = freq_0 + grad_x

# We turn off the gradient after some time (e.g. 1 period)
T_grad = (1/freq_0)


# Time interval
n_turns = 2
period = 1 / freq_0
n_samples_per_turn = 1000 #this needs to be big enough for integration
n_samples = n_samples_per_turn*n_turns
t_max = n_turns * period 
time = seq(0, t_max, length.out = n_samples)


# This is how the gradient varies in time
freq_xt = matrix(rep(freq_x, times = length(time)), nrow = length(freq_x))
freq_xt[, time > T_grad] = freq_0

# Plot the signals

# Colors and labels for the legend
colors = c("black", "red", "blue","pink")
labels = c("Signal without gradient", "Gradient turned off", "Low frequency signal", "High frequency signal")

# IDs, match IDs to color
label_ids = c(paste0("label", 1:(n + 2)))
values = setNames(colors, label_ids)

# A signal at the basic freq_0
base_signal =  cos(2* pi * freq_0 * time)
df_base = data.frame(time = time, signal = base_signal)

time_idx_peak_base = which.min(abs(time - 1.5*period))

p_signals = ggplot(df_base, aes(x = time, y = signal, color = "label1")) +
  geom_line(size = 1) +
  guides(color = guide_legend())+
  theme_minimal() +
  theme(
    legend.title = element_blank()
  ) +
  scale_color_manual(values = values,
                     breaks = label_ids,
                     labels = labels)+
  geom_hline(yintercept = 0, color = "black") +
  labs(x = "Time [s]", y = "Signal") +
  geom_vline(aes(xintercept = T_grad,color = "label2"),linetype = "dashed")+
  geom_vline(xintercept = time[time_idx_peak_base],color =colors[1],linetype = "dashed")


# Phase plot

dt = t_max/(n_samples-1)

freq_0t = rep(freq_0, times = length(time))
phase_base = 2*cumsum(freq_0t * dt)

df_phase_base = data.frame(time, phase)

p_phase = ggplot(df_phase_base, aes(x = time, y = phase_base)) +
  geom_line(size = 1, color = colors[1]) +
  theme_minimal() +
  geom_hline(yintercept = 0, color = "black") +
  labs(x = "Time [s]", y =expression(Phase~phi~"["*pi*"]")) +
  geom_vline(aes(xintercept = T_grad),color = colors[2],linetype = "dashed" )+
  geom_vline(xintercept = time[time_idx_peak_base],color =colors[1],linetype = "dashed")




#Frequency 
df_f_base = data.frame(time, freq = freq_0t)

p_freq = ggplot(df_f_base, aes(x = time, y = freq, color = "label1")) +
  geom_line(size = 1) +
  guides(color = guide_legend())+
  theme_minimal() +
  theme(
    legend.title = element_blank())+
  scale_color_manual(values = values,
                     breaks = label_ids,
                     labels = labels)+
  geom_hline(yintercept = 0, color = "black") +
  labs(x = "Time [s]", y = "Frequency [Hz]")+
  geom_vline(aes(xintercept = T_grad,color = "label2"),linetype = "dashed", )
 

                         
# Plotting the two signals
for (idx in 1:n) {
 
  # Integrate to get the instantaneous phase
  phase = 2*cumsum(freq_xt[idx, ] * dt)
  
  phase_diff = (phase-phase_base)[length(time)]
  time_diff = t_max/(n_turns*2)*phase_diff
  time_idx_peak = which.min(abs(time- (time[time_idx_peak_base]-time_diff)))
  
  signal = cos(pi*phase)
  
  df = data.frame(time,signal,label = label_ids[idx+2])

  # Signal plot
  p_signals = p_signals + 
    geom_line(data = df, aes(x = time, y = signal, color = label), 
                    size = 1,alpha = 0.5)   +    
      geom_vline(xintercept = time[time_idx_peak],color = colors[idx+2],linetype = "dashed")
  
    
  # Phase plot 
  
  df = data.frame(time,phase,label = label_ids[idx+2])
  
  p_phase = p_phase + geom_line(data = df, aes(x = time, y = phase), 
                                size = 1,alpha = 0.5, color = colors[idx+2]) +
    geom_vline(xintercept = time[time_idx_peak],color =colors[idx+2],linetype = "dashed")
  
  # Frequency plot
  
  df = data.frame(time,freq  = freq_xt[idx,],label = label_ids[idx+2])
  p_freq = p_freq + geom_line(data = df, aes(x = time, y = freq, color = label), 
                              size = 1,alpha = 0.5)
   
}

p_signals 
p_phase
p_freq

x_labels = seq(0, t_max*freq_0*2, 0.5)

# change axis
p_signals + 
  scale_x_continuous(
   breaks = seq(0, t_max, length.out = length(x_labels)),
  labels = x_labels)  + 
  labs(x = expression(Phase~phi~"(without gradient) ["*pi*"]"), y= 'Signal')
  
p_phase + 
  scale_x_continuous(
    breaks = seq(0, t_max, length.out = length(x_labels)),
    labels = x_labels)  + 
  scale_y_continuous(
    limits = c(min(x_labels),max(x_labels)),
    breaks = seq(0, max(x_labels), length.out = length(x_labels)),
    labels = x_labels)  + 
  labs(x = expression(Phase~phi~"(without gradient) ["*pi*"]"), y= expression(Phase~phi~"["*pi*"]"))








## plot with constant gradient ##

# Plot the signals

# Colors and labels for the legend
colors = c("black", "blue","pink")
labels = paste0("f = ", c(freq_0, freq_x), " Hz")


# IDs, match IDs to color
label_ids = c(paste0("label", 1:(n+1 )))
values = setNames(colors, label_ids)

# A signal at the basic freq_0
base_signal =  cos(2* pi * freq_0 * time)
df_base = data.frame(time = time, signal = base_signal)

time_idx_peak_base = which.min(abs(time - 1.5*period))

p_signals = ggplot(df_base, aes(x = time, y = signal, color = "label1")) +
  geom_line(size = 1) +
  guides(color = guide_legend())+
  theme_minimal() +
  theme(
    legend.title = element_blank()
  ) +
  scale_color_manual(values = values,
                     breaks = label_ids,
                     labels = labels)+
  geom_hline(yintercept = 0, color = "black") +
  labs(x = "Time [s]", y = "Signal") 

phase = 2*freq_0*time

df_phase_base = data.frame(time, phase)

p_phase = ggplot(df_phase_base, aes(x = time, y = phase)) +
  geom_line(size = 1, color = colors[1]) +
  theme_minimal() +
  geom_hline(yintercept = 0, color = "black") +
  labs(x = "Time [s]", y =expression(Phase~phi~"["*pi*"]")) 


for (idx in 1:n) {
  

  phase = 2*freq_x[idx]*time
  
  signal = cos(pi*phase)
  
  df = data.frame(time,signal,label = label_ids[idx+1])
  
  p_signals = p_signals + 
    geom_line(data = df, aes(x = time, y = signal, color = label), 
                                    size = 1,alpha = 0.5)       
  
  
  df = data.frame(time,phase,label = label_ids[idx+1])
 
  p_phase = p_phase +
    geom_line(data = df, aes(x = time, y = phase), 
                                size = 1,alpha = 0.5, color = colors[idx+1]) 
  
}

p_signals 
p_phase




## Now we simulate a gradient with changing gradient strength ##
freq_0 = 4

# time
n_turns = 2
period = 1 / freq_0
n_samples_per_turn = 50
n_samples = n_samples_per_turn*n_turns
t_max = n_turns * period 
time = seq(0, t_max, length.out = n_samples)


# gradient 
delta_f_min = -freq_0
delta_f_max = freq_0
delta_f = seq(delta_f_min,delta_f_max,length.out = n_samples)

freq_x = freq_0 + delta_f

# We turn off the gradient after some time (e.g. 1 period)
T_grad = (1/freq_0)

freq_xt = matrix(rep(freq_x, times = n_samples), nrow = length(freq_x))
freq_xt[, time > T_grad] = freq_0


dt = t_max/(n_samples-1)

# Third peak
time_peak_base = 2*period
#Second peak
time_peak_base_1 = period

signal_base = cos(2*pi*freq_0*time)
df = data.frame(x=time,y=signal_base)
p_signal = ggplot(df, aes(x, y)) +
  geom_line() +
  theme_minimal() +
  labs(x = "Time [s]", y = "Signal")+
  geom_vline(xintercept = T_grad,color = "red",linetype = "dashed" )+
  ylim(-1, 1) 

t_grad_ind = which.min(abs(time - T_grad))

phase_diff = numeric(0)
amp_freq = numeric(0)

plots = list()
for (f_idx in 1:length(delta_f)) {
  
  # Integrate to get the instantaneous phase
  phase = 2*cumsum(freq_xt[f_idx,] * dt)
  

  # The difference while gradient turned on
  phase_diff[f_idx] = 2*(freq_x[f_idx]-freq_0)*T_grad
  time_diff = t_max/(n_turns*2)*phase_diff[f_idx]

  
  if (delta_f[f_idx]<0) {
    time_peak_plot = time_peak_base_1
    time_peak = time_peak_base_1-time_diff
    arrow_end = "first"
  } else
  {
     time_peak = time_peak_base-time_diff
     time_peak_plot = time_peak_base
     arrow_end = "last"
  }
   
 
  # Signal
  signal = cos(pi*phase)
  
  #The first plot: frequency over time
  df = data.frame(x = time, y = freq_xt[f_idx,]-freq_0)
  p_freq = ggplot(df, aes(x, y)) +
    geom_line() +
    theme_minimal() +
    labs(x = "Time [s]", y = expression(Delta*"f [Hz]"))+
    geom_vline(xintercept = T_grad,color = "red",linetype = "dashed" )+
    ylim(-delta_f_max, delta_f_max)
    
  #The second plot: signal over time
  df = data.frame(x = time, y = signal)
  p_signal2 = p_signal + 
    geom_line(data = df, aes(x, y), color = "pink") +
    geom_vline(xintercept = time_peak,color = "pink",linetype = "dashed")+
    annotate(
      "segment",
      x = time_peak, xend = time_peak_plot,
      y = 0.5, yend = 0.5,
      color = "green",
      arrow = arrow(ends = "last", length = unit(0.2, "cm"))
    )
  
  #The third plot: delta phi over delta freq
  df = data.frame(x = delta_f[1:f_idx], y = phase_diff )
  p_phase = ggplot(df, aes(x, y)) +
    geom_line(color = "green") +
    theme_minimal() +
    labs(x = expression(Delta*"f [Hz]"), y = expression(Delta*phi~"["*pi*"]"))+
    xlim(delta_f[1],delta_f_max)+
    ylim(2*delta_f_min*T_grad, 2*delta_f_max*T_grad)
  
  #The forth plot: Amplitude over delta freq
  
  amp_freq[f_idx] = signal[t_grad_ind]
  df = data.frame(x = delta_f[1:f_idx], y = amp_freq )
  p_amp = ggplot(df, aes(x, y)) +
    geom_line() +
    theme_minimal() +
    labs(x = expression(Delta*"f [Hz]"), y = "Signal at time = T")+
    xlim(delta_f[1],delta_f_max)+
    ylim(-1, 1)
  
  grid_plot = wrap_plots(p_freq,p_phase,p_signal2,p_amp, ncol = 2, nrow = 2)  # auto-adjusts layout
  
  
  plots[[f_idx]] = grid_plot & theme(
    plot.background  = element_rect(fill = "transparent", colour = NA),
    panel.background = element_rect(fill = "transparent", colour = NA)
  )
  
  
}

plots_selected = plots[seq(1, length(plots), by = 5)]

filename = "phase_encoding_epic_demo_test.gif"
path_out = file.path(path_figures, filename)
create_gif_from_plots(plots_selected, path_out, 1200, 900, 150, fps = 1)



### Now create a gif where the gradient changes and we measure the signal ##

# Convert to data frame for ggplot
df = expand.grid(x = 1:ncol(mat_plot), y = 1:nrow(mat_plot))
df$val = as.vector(mat_plot)

# Plot in ggplot
p_object = ggplot(df, aes(x = x, y = y, fill = val)) +
  geom_raster(interpolate = FALSE) +
  scale_fill_gradient(low = "black", high = "white") +
  coord_fixed(expand = FALSE) +
  theme_void() +
  theme(legend.position = "none")


# Time interval
#Test with freq_0 = 1, instead of 2
delta_f_max = 1
period = 1/delta_f_max


n_turns = 2
n_samples_per_turn = 100
n_samples = n_samples_per_turn*n_turns

T_grad = 1/2*period*n_turns


delta_f = seq(-delta_f_max,delta_f_max,length.out = n_samples)

n_samples = length(delta_f)

delta_f_FOV = (n-1)*delta_f

# How frequency varies in space (1 to n px) depending on gradient strength 
freq_x = t(sapply(delta_f_FOV, function(f) seq(0, f, length.out = n)))


#amp_sum = sum(mat)
#amp_freq = numeric(0)
kspace = numeric(0)
plots = list()
for (f_idx in 1:n_samples){
 
  #Phase
  signals_px = numeric(n_object_px^2)
  for (px_idx in 1:n_object_px^2) {
    row_idx = object_idx[px_idx, 1]
    col_idx = object_idx[px_idx, 2]
    amp = mat[row_idx, col_idx]
    
    phase = 2* freq_x[f_idx,col_idx]*T_grad
    signals_px[px_idx] = amp*cos(pi*phase)
    
  }
  
  kspace[f_idx] = sum(signals_px)
 
  df = data.frame(x = delta_f[1:f_idx], y = kspace )
  p_amp = ggplot(df, aes(x, y)) +
    geom_line() +
    theme_minimal() +
    labs(x = expression(Delta*"f [Hz]"), y = "Value at time = T")+
    xlim(delta_f[1],delta_f_max)+
    coord_fixed(ratio = 1/12) +
    ylim(-amp_sum/2, amp_sum)

  
# Plot the gradient
df = data.frame(
  x = c(0, n, n),
  y = c(0, 0, delta_f_FOV[f_idx])
)

p_gradient = ggplot(df, aes(x, y)) +
  geom_polygon(fill = "blue") +
  coord_fixed(ratio = 1/2.5) +
  theme_void()+
  ylim(min(delta_f_FOV),max(delta_f_FOV))+
  ggtitle("Phase gradient")+
  theme(plot.title = element_text(size = 10))
  
plots[[f_idx]] = wrap_plots(p_object,p_amp,p_gradient,ncol = 2,nrow = 2)

}

 
plots_gif = plots[seq(1, length(plots), by = 10)]

filename = "phase_encoding_object_demo_test.gif"
path_out = file.path(path_figures, filename)
create_gif_from_plots(plots_gif, path_out, 800, 400, 100, fps = 1)


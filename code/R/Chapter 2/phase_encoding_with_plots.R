## Simulate a phase encoding gradient ##

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
T_grad = (1/freq_0)*2


# Time interval
n_turns = 2
period = 1 / freq_0
n_samples_per_turn = 1000 #this needs to be big enough to calculate phase
n_samples_total = n_samples_per_turn*n_turns
time_max = n_turns * period 
time = seq(0, time_max, length.out = n_samples_total)


# This is how the gradient varies in time
freq_xt = matrix(rep(freq_x, times = length(time)), nrow = length(freq_x))
freq_xt[, time > T_grad] = freq_0

plot(time,freq_xt[2,],type = "l")



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
         #(#position = "inside")
   # ) +
  theme_minimal() +
  theme(
    #axis.text.y = element_blank(),
    legend.title = element_blank()#,
   # legend.position.inside = c(1, 1),
    #legend.justification.inside = c("right", "top")
  ) +
  scale_color_manual(values = values,
                     breaks = label_ids,
                     labels = labels)+
  geom_hline(yintercept = 0, color = "black") +
  labs(x = "Time [s]", y = "Amplitude") +
  geom_vline(aes(xintercept = T_grad,color = "label2"),linetype = "dashed")+
  geom_vline(xintercept = time[time_idx_peak_base],color =colors[1],linetype = "dashed")


#+
  #scale_x_continuous(
   # breaks = seq(-t_max, t_max, length.out = length(x_labels)),
    #labels = x_labels)  + 
  #labs(x = expression(Phase~phi~"["*pi*"]"), y= 'Amplitude')


# Phase plot

dt = time_max/(n_samples_total-1)

freq_0t = rep(freq_0, times = length(time))
phase_base = 2*cumsum(freq_0t * dt)

df_phase_base = data.frame(time, phase)

p_phase = ggplot(df_phase_base, aes(x = time, y = phase_base)) +
  geom_line(size = 1, color = colors[1]) +
  #guides(color = guide_legend())+
  #(#position = "inside")
  # ) +
  theme_minimal() +
  theme(
    #axis.text.y = element_blank(),
    #legend.title = element_blank()#,
    # legend.position.inside = c(1, 1),
    #legend.justification.inside = c("right", "top")
  ) +
  #scale_color_manual(values = values,
   #                  breaks = label_ids,
    #                 labels = labels)+
  geom_hline(yintercept = 0, color = "black") +
  labs(x = "Time [s]", y =expression(Phase~phi~"["*pi*"]")) +
  geom_vline(aes(xintercept = T_grad),color = colors[2],linetype = "dashed" )+
  geom_vline(xintercept = time[time_idx_peak_base],color =colors[1],linetype = "dashed")




#Frequency 



df_f_base = data.frame(time, freq = freq_0t)

p_freq = ggplot(df_f_base, aes(x = time, y = freq, color = "label1")) +
  geom_line(size = 1) +
  guides(color = guide_legend())+
  #(#position = "inside")
  # ) +
  theme_minimal() +
  theme(
    #axis.text.y = element_blank(),
    legend.title = element_blank()#,
    # legend.position.inside = c(1, 1),
    #legend.justification.inside = c("right", "top")
  ) +
  scale_color_manual(values = values,
                     breaks = label_ids,
                     labels = labels)+
  geom_hline(yintercept = 0, color = "black") +
  labs(x = "Time [s]", y = "Frequency [Hz]")+
  geom_vline(aes(xintercept = T_grad,color = "label2"),linetype = "dashed", )
 



                         

for (idx in 1:n) {
 
   # time step 
  
  # Integrade to get the instantaneous phase
  phase = 2*cumsum(freq_xt[idx, ] * dt)
  

  phase_diff = (phase-phase_base)[length(time)]
  time_diff = t_max/(n_turns*2)*phase_diff
  time_idx_peak = which.min(abs(time- (time[time_idx_peak_base]-time_diff)))
  
   
  
  signal = cos(pi*phase)
  
  
  df = data.frame(time,signal,label = label_ids[idx+2])


    p_signals = p_signals + geom_line(data = df, aes(x = time, y = signal, color = label), 
                    size = 1,alpha = 0.5)   +    
      geom_vline(xintercept = time[time_idx_peak],color =colors[idx+2],linetype = "dashed")
  
    

  
  df = data.frame(time,phase,label = label_ids[idx+2])
  
  #p_phase = p_phase + geom_line(data = df, aes(x = time, y = phase, color = label), 
   #                             size = 1,alpha = 0.5)+
  #  geom_vline(xintercept = time[time_idx_peak],color =colors[idx+2],linetype = "dashed")
  
  p_phase = p_phase + geom_line(data = df, aes(x = time, y = phase), 
                                size = 1,alpha = 0.5, color = colors[idx+2]) +
    geom_vline(xintercept = time[time_idx_peak],color =colors[idx+2],linetype = "dashed")
  
  
  
  
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
  labs(x = expression(Phase~phi~"["*pi*"]"), y= 'Amplitude')
  
p_phase + 
  scale_x_continuous(
    breaks = seq(0, t_max, length.out = length(x_labels)),
    labels = x_labels)  + 
  scale_y_continuous(
    breaks = seq(0, max(x_labels), length.out = length(x_labels)),
    labels = x_labels)  + 
  labs(x = expression(Phase~phi~"["*pi*"]"), y= expression(Phase~phi~"["*pi*"]"))


## plot with constant gradient

# Plot the signals

# Colors and labels for the legend
colors = c("black", "red", "blue","pink")
labels = c("f = 4 Hz", "Gradient turned off", "f = 2.5 Hz", "f = 5.5 Hz")

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
  #(#position = "inside")
  # ) +
  theme_minimal() +
  theme(
    #axis.text.y = element_blank(),
    legend.title = element_blank()#,
    # legend.position.inside = c(1, 1),
    #legend.justification.inside = c("right", "top")
  ) +
  scale_color_manual(values = values,
                     breaks = label_ids,
                     labels = labels)+
  geom_hline(yintercept = 0, color = "black") +
  labs(x = "Time [s]", y = "Amplitude") 

#+
#scale_x_continuous(
# breaks = seq(-t_max, t_max, length.out = length(x_labels)),
#labels = x_labels)  + 
#labs(x = expression(Phase~phi~"["*pi*"]"), y= 'Amplitude')


# Phase plot

dt = time_max/(n_samples_total-1)

freq_0t = rep(freq_0, times = length(time))
phase_base = 2*cumsum(freq_0t * dt)

df_phase_base = data.frame(time, phase)

p_phase = ggplot(df_phase_base, aes(x = time, y = phase_base)) +
  geom_line(size = 1, color = colors[1]) +
  #guides(color = guide_legend())+
  #(#position = "inside")
  # ) +
  theme_minimal() +
  theme(
    #axis.text.y = element_blank(),
    #legend.title = element_blank()#,
    # legend.position.inside = c(1, 1),
    #legend.justification.inside = c("right", "top")
  ) +
  #scale_color_manual(values = values,
  #                  breaks = label_ids,
  #                 labels = labels)+
  geom_hline(yintercept = 0, color = "black") +
  labs(x = "Time [s]", y =expression(Phase~phi~"["*pi*"]")) 


#Frequency 



df_f_base = data.frame(time, freq = freq_0t)

p_freq = ggplot(df_f_base, aes(x = time, y = freq, color = "label1")) +
  geom_line(size = 1) +
  guides(color = guide_legend())+
  #(#position = "inside")
  # ) +
  theme_minimal() +
  theme(
    #axis.text.y = element_blank(),
    legend.title = element_blank()#,
    # legend.position.inside = c(1, 1),
    #legend.justification.inside = c("right", "top")
  ) +
  scale_color_manual(values = values,
                     breaks = label_ids,
                     labels = labels)+
  geom_hline(yintercept = 0, color = "black") +
  labs(x = "Time [s]", y = "Frequency [Hz]")+
  geom_vline(aes(xintercept = T_grad,color = "label2"),linetype = "dashed", )






for (idx in 1:n) {
  
  # time step 
  
  # Integrade to get the instantaneous phase
  phase = 2*cumsum(freq_xt[idx, ] * dt)
  
  phase_diff = (phase-phase_base)[length(time)]
  time_diff = t_max/(n_turns*2)*phase_diff
  time_idx_peak = which.min(abs(time- (time[time_idx_peak_base]-time_diff)))
  
  
  

  
  
  signal = cos(pi*phase)
  
  
  df = data.frame(time,signal,label = label_ids[idx+2])
  
  
  p_signals = p_signals + geom_line(data = df, aes(x = time, y = signal, color = label), 
                                    size = 1,alpha = 0.5)       
 
  
  
  
  df = data.frame(time,phase,label = label_ids[idx+2])
  
  #p_phase = p_phase + geom_line(data = df, aes(x = time, y = phase, color = label), 
  #                             size = 1,alpha = 0.5)+
  #  geom_vline(xintercept = time[time_idx_peak],color =colors[idx+2],linetype = "dashed")
  
  p_phase = p_phase + geom_line(data = df, aes(x = time, y = phase), 
                                size = 1,alpha = 0.5, color = colors[idx+2]) 

  
  
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
  labs(x = expression(Phase~phi~"["*pi*"]"), y= 'Amplitude')

p_phase + 
  scale_x_continuous(
    breaks = seq(0, t_max, length.out = length(x_labels)),
    labels = x_labels)  + 
  scale_y_continuous(
    breaks = seq(0, max(x_labels), length.out = length(x_labels)),
    labels = x_labels)  + 
  labs(x = expression("Phase (without gradient)"~"["*pi*"]"), y= expression(Phase~phi~"["*pi*"]"))











### Now we simulate a gradient with changing gradient strength ###
freq_0 = 4

# time
n_turns = 2
period = 1 / freq_0
n_samples_per_turn = 50
n_samples_total = n_samples_per_turn*n_turns
time_max = n_turns * period 
time = seq(0, time_max, length.out = n_samples_total)


# gradient 
delta_f_min = -freq_0
delta_f_max = freq_0
delta_f = seq(delta_f_min,delta_f_max,length.out = n_samples_total)

freq_x = freq_0 + delta_f

# We turn off the gradient after some time (e.g. 1 period)
T_grad = (1/freq_0)

freq_xt = matrix(rep(freq_x, times = n_samples_total), nrow = length(freq_x))
freq_xt[, time > T_grad] = freq_0


signals = array(0, dim = n_samples)
phase = array(0, dim =n_samples)

dt = time_max/(n_samples_total-1)
time_peak_base = 2*period
time_peak_base_1 = period

signal_base = cos(2*pi*freq_0*time)
df = data.frame(x=time,y=signal_base)
p_signal = ggplot(df, aes(x, y)) +
  geom_line() +
  theme_minimal() +
  labs(x = "Time [s]", y = "Amplitude")+
  geom_vline(xintercept = T_grad,color = "red",linetype = "dashed" )+
  ylim(-1, 1) 

t_grad_ind = which.min(abs(time - T_grad))
phase_diff = numeric(0)
amp_freq = numeric(0)
plots = list()
for (f_idx in 1:length(delta_f)) {
#  for (f_idx in 1:10) {
  
  # Integrate to get the instantaneous phase
  phase = 2*cumsum(freq_xt[f_idx,] * dt)
  
  

  # The difference while gradient turned on
  phase_diff[f_idx] = 2*(freq_x[f_idx]-freq_0)*T_grad
  time_diff = time_max/(n_turns*2)*phase_diff[f_idx]
 #time_diff = period/(2)*phase_diff[f_idx]
  
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
    #geom_vline(xintercept = time_peak_plot,color = "grey",linetype = "dashed" )+
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
    labs(x = expression(Delta*"f [Hz]"), y = "Amplitude at time = T")+
    xlim(delta_f[1],delta_f_max)+
    ylim(-1, 1)
  
  grid_plot = wrap_plots(p_freq,p_phase,p_signal2,p_amp, ncol = 2, nrow = 2)  # auto-adjusts layout
  
  
  plots[[f_idx]] = grid_plot
  
}

plots = plots[seq(1, length(plots), by = 4)]

filename = "phase_encoding_epic_demo_test.gif"
path_out = file.path(path_figures, filename)
create_gif_from_plots(plots, path_out, 800, 600, 100, fps = 1)



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


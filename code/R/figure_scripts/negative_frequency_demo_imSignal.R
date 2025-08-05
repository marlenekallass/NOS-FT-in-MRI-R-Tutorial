
source("functions/misc_utils.R")
source("functions/plotting_functions.R")


install_and_load(c("ggplot2", "magick", "pracma", "patchwork"))


path_figures = "../../figures"

#global variables
i = complex(real = 0, imaginary = 1)

#specific variables
#samp_f = seq(2.9,3.1, by = .1)
samp_f= 3
samp_f=c(3,-3)

#samp_f = 3
freq = 3 # == 3 Hz
time = seq(0,1,by=.0001)
time = seq(0,1,by=.01)
time = seq(0,1/freq+0.01*1/freq,by=.01*1/freq)
#initiate
g_t = cos(2*pi*freq*time) 
g_t = sin(2*pi*freq*time)*i 
g_t = cos(2*pi*freq*time+pi/4) 
g_t = -sin(2*pi*freq*time)*i 
gt_df <- data.frame(time = time, g_t = Im(g_t))
gt_df <- data.frame(time = time, g_t = g_t)
g_hat_mean = matrix(0, ncol = 2, nrow = length(samp_f))

# Mean of Re and Im for plot of the center of mass further below
for(index in 1:length(samp_f)){
  g_hat_mean[index,1] = Re(mean(g_t*exp(-2*pi*i*samp_f[index]*time)))
  g_hat_mean[index,2] = Im(mean(g_t*exp(-2*pi*i*samp_f[index]*time)))
} # End for index

# Turn into data.frame:
g_hat_mean_df = as.data.frame(g_hat_mean)
colnames(g_hat_mean_df) = c("Re","Im")

# Min max of each Re and Im for flexible plotting routines (limiting the y- and x-axis of a plot):
min_max = matrix(0, ncol = 4, nrow = length(samp_f))
# for(index in 1:length(samp_f)){
#   min_max[index,1] = min(Re(g_t*exp(-2*pi*i*samp_f[index]*time)))
#   min_max[index,2] = min(Im(g_t*exp(-2*pi*i*samp_f[index]*time)))
#   min_max[index,3] = max(Re(g_t*exp(-2*pi*i*samp_f[index]*time)))
#   min_max[index,4] = max(Im(g_t*exp(-2*pi*i*samp_f[index]*time)))
# } # End for index
for(index in 1:length(samp_f)){
  min_max[index,1] =-1
  min_max[index,2] = -1
  min_max[index,3] = 1
  min_max[index,4] = 1
} # End for index


index = 1

# Create data frame with coordinates of wound up function for respective sampop_f[index]:
full_Re = Re(g_t * exp(-2*pi*i*samp_f[index]*time))
full_Im = Im(g_t * exp(-2*pi*i*samp_f[index]*time))
# Extract coordinates for point of central mass:
point = g_hat_mean_df[index,]

index = 2

# Create data frame with coordinates of wound up function for respective sampop_f[index]:
full_Re_2 = Re(g_t * exp(-2*pi*i*samp_f[index]*time))
full_Im_2 = Im(g_t * exp(-2*pi*i*samp_f[index]*time))
# Extract coordinates for point of central mass:
point_2 = g_hat_mean_df[index,]



plots = list()
for(t in 1:(length(time)-1)){ 
  g_f = as.data.frame(cbind(full_Re[1:(t+1)], full_Im[1:(t+1)]))
  colnames(g_f) = c("Re", "Im")
  segment = data.frame(x = 0, y = 0,
                       xend = g_f[t+1,1], yend = g_f[t+1,2])
  
  # complex plane
  plot1 =  ggplot(data = g_f, aes(x = Re, y = Im)) +
    geom_path(color = "darkorchid4") +  # Color for line of the plot
    geom_point(data = point, aes(x = Re, y = Im), # add point of central mass
               color = "darkblue", size = 3) +  # color and size of the point
    geom_segment(data = segment, 
                 aes(x = x, y = y, xend = xend, yend = yend),
                 arrow = arrow(length = unit(0.2, "cm")),
                 color = "black") +
    labs(title = expression(omega == +nu),
         x = "Real Number", y = "Imaginary Number")+
    xlim(c(min(min_max[,1]),max(min_max[,3]))) +  # making sure that plot is not out of bounds
    ylim(c(min(min_max[,2]),max(min_max[,4]))) +  # and not just centralized
    theme_minimal() + # white instead of grey background
    theme(plot.title.position = "plot",  plot.title = element_text(hjust = 0.7),axis.text.x = element_text(angle = 45, hjust = 1)) +
    coord_fixed()
  
  # second frequency
  g_f = as.data.frame(cbind(full_Re_2[1:(t+1)], full_Im_2[1:(t+1)]))
  colnames(g_f) = c("Re", "Im")
  segment = data.frame(x = 0, y = 0,
                       xend = g_f[t+1,1], yend = g_f[t+1,2])
  
  # complex plane
  plot2 =  ggplot(data = g_f, aes(x = Re, y = Im)) +
    geom_path(color = "darkorchid4") +  # Color for line of the plot
    geom_point(data = point_2, aes(x = Re, y = Im), # add point of central mass
               color = "darkblue", size = 3) +  # color and size of the point
    geom_segment(data = segment, 
                 aes(x = x, y = y, xend = xend, yend = yend),
                 arrow = arrow(length = unit(0.2, "cm")),
                 color = "black") +
    labs(title = expression(omega == -nu),
         x = "Real Number", y = "Imaginary Number")+
    xlim(c(min(min_max[,1]),max(min_max[,3]))) +  # making sure that plot is not out of bounds
    ylim(c(min(min_max[,2]),max(min_max[,4]))) +  # and not just centralized
    theme_minimal() + # white instead of grey background
    theme(plot.title.position = "plot",plot.title = element_text(hjust = 0.7),axis.text.x = element_text(angle = 45, hjust = 1)) +
    coord_fixed()
  
  
  
  # time domain
  
  plot3 = ggplot(data = gt_df, aes(x = time, y = g_t)) +
    geom_line(color = "darkgreen") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_segment(
      x = time[t+1], xend = time[t+1],
      y = 0, yend = gt_df$g_t[t+1],
      arrow = arrow(length = unit(0.2, "cm")),
      color = "black"
    ) +
    #labs(title = "Imaginary signal",x = "Time", y = "Amplitude") +
    labs(title = "Real signal",x = "Time", y = "Amplitude") +
    scale_x_continuous(
      breaks = c(0, 1/(4*freq), 1/(2*freq), 3/(4*freq), 1/freq),
      labels = c(
        expression(0),
        expression(frac(1, 4*nu)),
        expression(frac(1, 2*nu)),
        expression(frac(3, 4*nu)),
        expression(frac(1, nu))))+
    theme_minimal() +
    theme(plot.title.position = "plot",plot.title = element_text(hjust = 0.6))
  
  plots[[t]] = (plot1 | plot2) / plot3
  
} # End for index


filename = "test.gif"
path_out = file.path(path_figures, filename)
create_gif_from_plots(plots, path_out)




source("functions/misc_utils.R")
source("functions/plotting_functions.R")
install_and_load(c("ggplot2", "magick", "pracma", "patchwork"))


path_figures = "../../figures/Chapter 3/gifs"

# Global variables
i = complex(real = 0, imaginary = 1)

# Parameters
freq_0 = 3
n_turns = 1
angle_steps = 1 / (16 * 4) * 1/freq_0
time = seq(0, n_turns * 1/freq_0, by =angle_steps)

# Sample frequencies
freqs = c(freq_0, -freq_0)

# Signal
g_t = cos(2 * pi * freq_0 * time)
gt_df = data.frame(time = time, g_t = g_t)

g_t = -sin(2 * pi * freq_0 * time)*i
gt_df <- data.frame(time = time, g_t = Im(g_t))

# Fourier means
g_hat_mean = matrix(0, ncol = 2, nrow = length(freqs))
for (index in 1:length(freqs)) {
  g_hat_mean[index, 1] = Re(mean(g_t * exp(2 * pi * i * freqs[index] * time)))
  g_hat_mean[index, 2] = Im(mean(g_t * exp(2 * pi * i * freqs[index] * time)))
}
g_hat_mean_df = as.data.frame(g_hat_mean)
colnames(g_hat_mean_df) = c("Re", "Im")

# Plot limits
min_max = matrix(c(-1, -1, 1, 1), ncol = 4, byrow = TRUE)

# Precompute complex plane trajectories
full_Re = list()
full_Im = list()
points = list()
for (index in 1:length(freqs)) {
  full_Re[[index]] = Re(g_t * exp(2 * pi * i * freqs[index] * time))
  full_Im[[index]] = Im(g_t * exp(2 * pi * i * freqs[index] * time))
  points[[index]] = g_hat_mean_df[index, ]
}

plots = list()
for (t in seq_along(time)) {
  
  # First frequency
  g_f = data.frame(Re = full_Re[[1]][1:t], Im = full_Im[[1]][1:t])
  segment = data.frame(x = 0, y = 0, xend = g_f[t, 1], yend = g_f[t, 2])
  plot1 = ggplot(g_f, aes(x = Re, y = Im)) +
    geom_path(color = "darkorchid4") +
    geom_point(data = points[[1]], aes(x = Re, y = Im), color = "darkblue", size = 3) +
    geom_segment(data = segment, aes(x = x, y = y, xend = xend, yend = yend),
                 arrow = arrow(length = unit(0.2, "cm")), color = "black") +
    labs(title = expression(omega == +nu),
         x = "Real Number", y = "Imaginary Number")+
    xlim(c(min(min_max[,1]),max(min_max[,3]))) +
    ylim(c(min(min_max[,2]),max(min_max[,4]))) +
    theme_minimal() +
    theme(plot.title.position = "plot",  plot.title = element_text(hjust = 0.6),
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    coord_fixed()
  
  # Second frequency
  g_f2 = data.frame(Re = full_Re[[2]][1:t], Im = full_Im[[2]][1:t])
  segment2 = data.frame(x = 0, y = 0, xend = g_f2[t, 1], yend = g_f2[t, 2])
  plot2 = ggplot(g_f2, aes(x = Re, y = Im)) +
    geom_path(color = "darkorchid4") +
    geom_point(data = points[[2]], aes(x = Re, y = Im), color = "darkblue", size = 3) +
    geom_segment(data = segment2, aes(x = x, y = y, xend = xend, yend = yend),
                 arrow = arrow(length = unit(0.2, "cm")), color = "black") +
    labs(title = expression(omega == -nu),
         x = "Real Number", y = "Imaginary Number")+
    xlim(c(min(min_max[,1]),max(min_max[,3]))) +
    ylim(c(min(min_max[,2]),max(min_max[,4]))) +
    theme_minimal() +
    theme(plot.title.position = "plot",  plot.title = element_text(hjust = 0.6),
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    coord_fixed()
  
  # Time domain
  plot3 = ggplot(gt_df, aes(x = time, y = g_t)) +
    geom_line(color = "darkgreen") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_segment(
      x = time[t], xend = time[t],
      y = 0, yend = gt_df$g_t[t],,
      arrow = arrow(length = unit(0.2, "cm")),
      color = "black"
    ) +
    #labs(x = "Time", y = "Real signal") +
    labs(x = "Time", y = "Imaginary signal") +
    theme_minimal() +
    theme(plot.title.position = "plot", plot.title = element_text(hjust = 0.55))
  
  plot3 <- plot3 +
    scale_x_continuous(
      breaks = c(0, 1/(4*freq_0), 1/(2*freq_0), 3/(4*freq_0), 1/freq_0),
      labels = c(
        expression(0),
        expression(frac(1, 4*nu)),
        expression(frac(1, 2*nu)),
        expression(frac(3, 4*nu)),
        expression(frac(1, nu))))
  
  plots[[t]] = (plot1 | plot2) / plot3 & theme(
    plot.background  = element_rect(fill = "transparent", colour = NA),
    panel.background = element_rect(fill = "transparent", colour = NA))
}

# Subset every skip_frames-th plot
skip_frames = 1
plots_subset = plots[seq(1, length(plots), by = skip_frames)]

#wrap_plots(plot1,plot2,plot3, ncol = 2, nrow = 2)  # auto-adjusts layout


filename = "test.gif"
path_out = file.path(path_figures, filename)
create_gif_from_plots(plots_subset, path_out, width = 900, height = 900,res = 150, fps = 2) 



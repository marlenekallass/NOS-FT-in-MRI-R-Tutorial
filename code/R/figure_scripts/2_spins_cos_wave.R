source("functions/misc_utils.R")
source("functions/plotting_functions.R")


install_and_load(c("ggplot2", "magick", "pracma", "patchwork"))


path_figures = "../../figures"

# Global variables
i = complex(real = 0, imaginary = 1)

# Parameters
skip_frames = 2
freq = 3
n_turns = 2
period = 1 / freq
angle_step = 1 / (16*2)
time = seq(0, n_turns * period, by = angle_step * period)

# Spin motions
spin_motion   = exp(-2 * pi * i * freq * time)
spin_motion_2 = exp(2 * pi * i * freq * time)  # define second spin here

# Signals
Mx = cos(2 * pi * freq * time)
Mx_df <- data.frame(time = time, g_t = Mx)

# Data frames
spin_df_full     <- data.frame(time = time, Re = Re(spin_motion),   Im = Im(spin_motion))
spin_df_full_2   <- data.frame(time = time, Re = Re(spin_motion_2), Im = Im(spin_motion_2))
time_first_turn  = time[time <= period]
spin_df_first_turn    <- subset(spin_df_full, time <= period)
spin_df_first_turn_2  <- subset(spin_df_full_2, time <= period)

# Axis limits
min_max = matrix(c(-1, -1, 1, 1), ncol = 4, byrow = TRUE)

plots = list()
idxs = seq(1, length(time), by = skip_frames)
for (k in seq_len(length(idxs) - 1)) {
  t = idxs[k]
  
  # Arrow segments
  arrow_segment = data.frame(
    x = 0, y = 0,
    xend = spin_df_full$Re[t],
    yend = spin_df_full$Im[t]
  )
  arrow_segment_2 = data.frame(
    x = 0, y = 0,
    xend = spin_df_full_2$Re[t],
    yend = spin_df_full_2$Im[t]
  )
  
  # Plot 1: spin_motion
  plot1 = ggplot(spin_df_first_turn, aes(x = Re, y = Im)) +
    geom_path(color = "grey70") +
    geom_segment(
      data = arrow_segment,
      aes(x = x, y = y, xend = xend, yend = yend),
      arrow = arrow(length = unit(0.2, "cm")),
      color = "black"
    ) +
    labs(x = "X", y = "Y") +
    xlim(c(min(min_max[,1]), max(min_max[,3]))) +
    ylim(c(min(min_max[,2]), max(min_max[,4]))) +
    coord_fixed() +
    theme_minimal() +
    theme(
      plot.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank()
    )
  
  # Plot 2: spin_motion_2
  plot2 = ggplot(spin_df_first_turn_2, aes(x = Re, y = Im)) +
    geom_path(color = "grey70") +
    geom_segment(
      data = arrow_segment_2,
      aes(x = x, y = y, xend = xend, yend = yend),
      arrow = arrow(length = unit(0.2, "cm")),
      color = "black"
    ) +
    labs(x = "X", y = "Y") +
    xlim(c(min(min_max[,1]), max(min_max[,3]))) +
    ylim(c(min(min_max[,2]), max(min_max[,4]))) +
    coord_fixed() +
    theme_minimal() +
    theme(
      plot.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank()
    )
  
  # Plot 3: EXACTLY your original Mx plot
  Mx_partial <- Mx_df[1:t, ]
  
  plot3 = ggplot(Mx_partial, aes(x = time, y = g_t)) +
    geom_line(color = "purple") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    labs(x = "time", y = expression(M[x])) +
    xlim(n_turns * period, 0) +     # <--- reversed x axis
    ylim(c(min(min_max[,1]), max(min_max[,3]))) +
    theme_minimal() +
    theme(
      plot.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank()
    )
  
  plot3 <- plot3 + coord_flip() + theme(aspect.ratio = 1)
  
  # Empty plot for lower right corner
  #empty_plot = ggplot() + theme_void()
  
  # Layout
  plots[[k]] = (plot1 | plot2) / (plot3 | plot3)
}


filename = "test.gif"
path_out = file.path(path_figures, filename)
create_gif_from_plots(plots, path_out, 800, 800, 300, fps = 2)

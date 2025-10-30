source("functions/misc_utils.R")
source("functions/plotting_functions.R")

install_and_load(c("ggplot2", "magick", "pracma", "patchwork"))

path_figures = "../../figures"
par(mar = c(0, 0, 0, 0))

# Global variables
i = complex(real = 0, imaginary = 1)

# Parameters
angle_step = 1 / (16*2)
time = seq(0, 2*pi, by = angle_step)



# Phase steps
n_phase_steps = (8*2)+1 
skip_frames = 1
phases = seq(0, 2 * pi, length.out = n_phase_steps)
phases = phases[seq(1, length(phases), by = skip_frames)]

plots = list()

signal1 = rep(0.7, length(time))
signal2 = rep(0.3, length(time))
signal3 = cos(time/2)^2
signal4 = sin(time/2)^2
signal_df = data.frame(time = time, signal1 = signal1, signal2 = signal2, signal3 = signal3,signal4 = signal4)


for (k in seq_along(phases)) {
  phase = phases[k]
  
  # Cosine wave with phase shift
  #signal = cos(2 * pi * freq * time - phase)
  signal_df_subset_long = tidyr::pivot_longer(
    signal_df[signal_df$time <= phase, ],  # filter by phase
    cols = c("signal1", "signal2", "signal3", "signal4"),
    names_to = "State", values_to = "Value"
  )
  
  plot1 = ggplot(signal_df_subset_long, aes(x = time, y = Value, color = State, linetype = State)) +
    geom_hline(yintercept = 0, color = "grey50") +
    geom_vline(xintercept = 0, color = "grey50") +
    geom_line() +
    scale_color_manual(
      values = c("signal1" = "red", "signal2" = "blue", "signal3" = "green", "signal4" = "black"),
      labels = c(
        expression(I[z] == frac(1,2)), 
        expression(I[z] == -frac(1,2)), 
        expression(I[x] == frac(1,2)), 
        expression(I[x] == -frac(1,2))  # new label
      )
    ) +
    scale_linetype_manual(
      values = c("signal1" = "solid", "signal2" = "solid", "signal3" = "dashed", "signal4" = "dashed"),
      labels = c(
        expression(I[z] == frac(1,2)), 
        expression(I[z] == -frac(1,2)), 
        expression(I[x] == frac(1,2)), 
        expression(I[x] == -frac(1,2))  # new label
      )
    ) +
    labs(x = "Time", y = "Probability") +
    ylim(0, 1) +
    xlim(0, max(time)) +
    theme_minimal() +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      #plot.title = element_text(size = 8, hjust = 0.5),
      axis.title = element_text(),
      legend.position = "right",
      plot.title = element_text(
        hjust = 0.5,
        vjust = 0.5)
      #legend.title = element_text(size = 8),  # match plot title size
     # legend.background = element_rect(fill = "transparent")
      
    ) +
    ggtitle(expression("Measurement result probability"))
  
  # Fourier Transform point and radial arrow
  ft_value = 0.5 * exp(i * phase)
  ft_df = data.frame(Re = Re(ft_value), Im = Im(ft_value))
  radial_arrow_df = data.frame(x = 0, y = 0, xend = Re(ft_value), yend = Im(ft_value))
  
  # Arc path from phi = 0 to current phi
  angle_length = 0.5
  arc_phis = seq(0, phase, length.out = 100)
  arc_points = data.frame(
    Re = angle_length * cos(arc_phis),
    Im = angle_length * sin(arc_phis)
  )
  arc_last = tail(arc_points, 2)  # last segment for arrowhead
  
  plot2 = ggplot(ft_df, aes(x = Re, y = Im)) +
    geom_point(aes(color = "adot"), size = 2) +
    geom_hline(yintercept = 0, color = "grey70") +
    geom_vline(xintercept = 0, color = "grey70") +
   geom_path(data = arc_points,
                           aes(x = Re, y = Im, color = "red_arrow"))+
    geom_segment(data = arc_last,
                              aes(x = Re[1], y = Im[1], xend = Re[2], yend = Im[2], color = "red_arrow"),
                              arrow = arrow(length = unit(0.2, "cm")),
                              inherit.aes = FALSE) +
    geom_segment(data = radial_arrow_df,
                 aes(x = x, y = y, xend = xend, yend = yend, color = "blue_line")) +
    scale_color_manual(
      values = c("adot" = "black", "red_arrow" = "red", "blue_line" = "blue"),
      labels = c("adot" = expression(c(t)==A*e^{i*phi(t)}), "red_arrow" = expression(phi(t)), "blue_line" = expression(A)),
      name = "Legend title"
    ) +
    #scale_linetype_manual(values = c("red_arrow" = "solid"), guide = "none") +
    coord_fixed() +
    xlim(-0.6, 0.6) + ylim(-0.6, 0.6) +
    labs(x = "Re", y = "Im") +
    theme_minimal() +
    theme(
      legend.position = "left",
      axis.text = element_blank(),
      axis.ticks = element_blank(),
     # plot.title = element_text(size = 8, hjust = 0.5),
    #  plot.title = element_text(),
     # axis.title = element_text(size = 8)
     axis.title = element_text(),
    legend.text = element_text(size = 12),
    legend.title = element_blank()
    )+
    ggtitle(expression("Complex amplitude of "~I[z]==-frac(1,2)~"state"))
  
  plots[[k]] = plot2 | plot1
}

skip_frames = 1
plots_subset = plots[seq(1, length(plots), by = skip_frames)]


filename = "test.gif"
path_out = file.path(path_figures, filename)
create_gif_from_plots(plots_subset, path_out, 900, 450, 100, fps = 1)

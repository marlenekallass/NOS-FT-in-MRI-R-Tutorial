source("fourier_trans.R")
source("install_and_load.R")

install_and_load(c("ggplot2", "magick", "pracma", "patchwork"))

# Global variables
i = complex(real = 0, imaginary = 1)

# Parameters
freq = 3
n_turns = 1
period = 1 / freq
angle_step = 1 / (16*2)
time = seq(0, n_turns * period, by = angle_step * period)


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
skip_frames = 1
phases = seq(0, 2 * pi, length.out = n_phase_steps)
phases = phases[seq(1, length(phases), by = skip_frames)]

plots = list()

for (k in seq_along(phases)) {
  phase = phases[k]
  
  # Label for annotation, special case for k=1 (phi=0)
  if (k == 1) {
    label_text <- "phi == scriptstyle(0)"
  } else if (k == 5)  {
    label_text <- "phi == pi"
    
  }
  else {
    label_text <- paste("phi == ", fraction_labels[k], " * pi")
  }
  
  # Cosine wave with phase shift
  #signal = cos(2 * pi * freq * time - phase)
  signal = cos(2 * pi * freq * time - phase)
  
  signal_df = data.frame(time = time, value = signal)
  
  # Vertical line to peak
  t_peak = phase / (2 * pi * freq)
  vertical_line = data.frame(x = t_peak)
  arrow_segment = data.frame(x = 0, xend = t_peak, y = 0.8)
  
  plot1 = ggplot(signal_df, aes(x = time, y = value)) +
    geom_hline(yintercept = 0, color = "grey50") +
    geom_line(color = "black") +
    geom_segment(data = vertical_line, aes(x = x, xend = x, y = 0, yend = 1), color = "blue") +
    # Only add red arrow if k != 1
    {if (k != 1) geom_segment(data = arrow_segment,
                              aes(x = x, xend = xend, y = 0.6, yend = 0.6),
                              arrow = arrow(ends = "both", length = unit(0.15, "cm")),
                              color = "red") else NULL} +
    annotate("text", x = max(time)/5, y = 0.9, label = label_text, parse = TRUE, size = 2.5) +
    labs(x = "time", y = "Amplitude") +
    ylim(-1.1, 1.1) +
    theme_minimal() +
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      plot.title = element_text(size = 8, hjust = 0.5),
      axis.title = element_text(size = 8)
    ) +
    ggtitle(expression(S(t) == cos(omega[0] * t - phi)))
  
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
    geom_hline(yintercept = 0, color = "grey70") +
    geom_vline(xintercept = 0, color = "grey70") +
    # Only add red arc and arrow if k != 1
    {if (k != 1) geom_path(data = arc_points, aes(x = Re, y = Im), color = "red") else NULL} +
    {if (k != 1) geom_segment(data = arc_last,
                              aes(x = Re[1], y = Im[1], xend = Re[2], yend = Im[2]),
                              arrow = arrow(length = unit(0.2, "cm")),
                              inherit.aes = FALSE, color = "red") else NULL} +
    geom_segment(data = radial_arrow_df,
                 aes(x = x, y = y, xend = xend, yend = yend),
                 color = "blue") +
    geom_point(size = 1, color = "black") +
    coord_fixed() +
    xlim(-0.6, 0.6) + ylim(-0.6, 0.6) +
    labs(x = "Re", y = "Im") +
    theme_minimal() +
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      plot.title = element_text(size = 8, hjust = 0.5),
      axis.title = element_text(size = 8)
    ) +
    ggtitle(expression(FT*"{"*S(t)*"}"~(omega == omega[0])))
  
  plots[[k]] = plot1 | plot2
}


source("create_gif_from_plots.R")
create_gif_from_plots(plots, "fourier_phase_rotation_arc.gif", 800, 400, 300, fps = 1)

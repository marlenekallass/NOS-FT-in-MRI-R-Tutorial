source("functions/plotting_functions.R")

p = last_plot()
save_gg(p,path_figures,'phase_encoding_2.png',180,60)

p = last_plot()
save_gg(p,path_figures,'phase_encoding_phase.png',120,120)

p = recordPlot()
save_figure(p, path_figures , 'k_line.png')

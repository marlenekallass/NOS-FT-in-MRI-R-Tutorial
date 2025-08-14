p = last_plot()
save_gg(p,path_figures,'simulated_kspace_phase.png',80,80)

p = last_plot()
save_gg(p,path_figures,'phase_encoding_phase.png',120,120)

p = recordPlot()
save_figure(p, path_figures , 'k_line.png')

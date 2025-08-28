source("functions/plotting_functions.R")

p = last_plot()
save_gg(p,path_figures,'real_kspace.png',70,70)

p = last_plot()
save_gg(p,path_figures,'colsum_image.png',60,10)

p = last_plot()
save_gg(p,path_figures,'phase_encoding_phase.png',120,120)

p = recordPlot()
save_figure(p, path_figures , 'img_obj.png')

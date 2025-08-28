library(ggplot2)
library(patchwork)

path_figures = "../../figures/Chapter 3/linear FT"

ft_plot = function(freqs,values){
  df = data.frame(x = freqs, y = values)
  ggplot(df, aes(x, y)) +
    geom_segment(aes(x = x, xend = x, y = 0, yend = y),linewidth = 1.5,color = "darkblue") +
    ylim(-1.5,2.5) +
    xlab(expression(nu)) + ylab("") +
    theme_void()+
    xlim(-1.5,1.5)+
    annotate("text",x = 1.5,y = -0.3,label = expression(omega),size = 5)+
    geom_segment(aes(x = 0, xend = 0, y = -0.04, yend = 0.04), color = "black")+
    annotate("text",x = 0,y = -0.3,label = 0,size = 5)+
    geom_segment(aes(x = -1.5, xend = 1.5, y = 0, yend = 0),
                 color = "black",
                 arrow = arrow(length = unit(0.15, "inches")))
}


time_plot = function(t,M,label){
  ggplot(data.frame(t,M), aes(t, M)) + 
    geom_line(linewidth = 1.5, color = "darkblue") + 
    theme_void()+
    annotate("text", x = -0.1, y = 1, label = label, size = 5)+
  annotate("text",x = max(t),y = -0.2,label = "Time",size = 5)+
    geom_segment(aes(x = 0, xend = max(t)+0.1, y = 0, yend = 0),
                 color = "black",
                 arrow = arrow(length = unit(0.15, "inches")))+
    geom_segment(aes(x = 0, xend = 0, y =-1.2, yend = 1.2),
                 color = "black",
                 arrow = arrow(length = unit(0.15, "inches")))
}

time_plot(t,My,expression(i*M[y]))

time_plot(t,Mx,expression(M[x]))

freqs = c(-1,1)
values = c(1,1)
ft_plot(freqs,values)

freqs = c(-1,1)
values = c(-1,1)
ft_plot(freqs,values)

freqs = 1
values = 2
ft_plot(freqs,values)



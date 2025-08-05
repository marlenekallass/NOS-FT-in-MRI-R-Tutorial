fftshift = function(kspace) {  #
  # For better readability - could also be done via ncol() and nrow()
  rows = nrow(kspace) # Evaluate n of rows
  cols = ncol(kspace) # ... n of cols
  reshape_row = c((rows/2+1):rows, 1:(rows/2))  # rows/2+1 so it starts at first position second half
  # not last position of first half!
  reshape_col = c((cols/2+1):cols, 1:(cols/2))  # same here...
  kspace[reshape_row,reshape_col]               # reshape k-space
} # End of function fftshift()

fourier_trans = function(g_t,samp_f,time){
  
  ### Plot all 101 plots via basic plot(). Use the arrows to mimic a gif of the transition, using different sampling 
  ### frequencies. Using for loop for the plotting:
  
  # Set complex number "i":
  i = complex(real = 0, imaginary = 1) # beware to NOT USE "i" as index variable in a for loop from now on!!
  
  # Min max of each Re and Im for flexible plotting routines (limiting the y- and x-axis of a plot):
  min_max = matrix(0, ncol = 4, nrow = length(samp_f))
  for(index in 1:length(samp_f)){
    min_max[index,1] = min(Re(g_t*exp(-2*pi*i*samp_f[index]*time)))
    min_max[index,2] = min(Im(g_t*exp(-2*pi*i*samp_f[index]*time)))
    min_max[index,3] = max(Re(g_t*exp(-2*pi*i*samp_f[index]*time)))
    min_max[index,4] = max(Im(g_t*exp(-2*pi*i*samp_f[index]*time)))
  } # End for index
  
  
  # Plotting: 
  for(index in 1:length(samp_f)){ # don't call it i when i was defined above as a constant variable!!!
    
    plot(g_t*exp(-2*pi*i*samp_f[index]*time), type = "l", col = "darkorchid4", 
         # Title entails information on winding/sample freq and original cosine freq
         main = paste("Winding Frequency of", samp_f[index],"Hz"), 
         xlab = "Real Numer",
         ylab = "Imaginary Number",
         # The python script doesn't do this, always centers the the plot
         xlim = c(min(min_max[,1]),max(min_max[,3])), # making sure that plot is not out of bounds
         ylim = c(min(min_max[,2]),max(min_max[,4]))) 
    
    # Add center mass point == integral of g_t*exp(-2*pi*i*samp_f[index]*time)
    points(x = Re(mean(g_t*exp(-2*pi*i*samp_f[index]*time))), Im(mean(g_t*exp(-2*pi*i*samp_f[index]*time))),
           col = "darkblue", pch = 19)
    
  } # End for index
  
  # Matrix for all the means of g_hat for Re and Im:
  g_hat_mean = matrix(0, ncol = 2, nrow = length(samp_f))
  
  # Mean of Re and Im for plot of the center of mass further below
  for(index in 1:length(samp_f)){
    g_hat_mean[index,1] = Re(mean(g_t*exp(-2*pi*i*samp_f[index]*time)))
    g_hat_mean[index,2] = Im(mean(g_t*exp(-2*pi*i*samp_f[index]*time)))
  } # End for index
  
  # Function of the center of mass of the wound up graph, peaking at the 
  # frequency or frequencies of the initial cosine wave!
  plot(x = samp_f, y = g_hat_mean[,1], type ="l",   # plot of mean(Re)
       xlab = "Winding Frequency",  # rename labels
       ylab = "Mean of g_hat(f)",
       main = "Function of the Center of Mass for each Winding Frequency",
       col = "deepskyblue3", ylim = c(min(min(g_hat_mean[,1]),min(g_hat_mean[,2])), 
                                      max(max(g_hat_mean[,1]),max(g_hat_mean[,2]))))
  lines(x = samp_f, y = g_hat_mean[,2], type = "l", col = "hotpink2") # Add mean(Im) to plot
  axis(1, at = seq(min(samp_f),max(samp_f), by = 1)) # Adjust x-axis tick mark (every integer step)
  legend("bottomright",legend = c("Re","Im"),               # add legend
         col = c("deepskyblue3","hotpink2"),  # legend line color
         lty = 1,   # line type of legend (corresponding to plot)
         cex = .75) # size of legend in ratio to standard size
  
  # Smoothed out version (similar to python version):
  g_hat_smooth = g_hat_mean
  for(index in 1:length(g_hat_smooth[,1])){
    if(g_hat_smooth[index,1]<=max(g_hat_smooth[,1])/2){ # Filter for values below Nyquist frequency
      g_hat_smooth[index,1] = 0
    } # End if 
  } # End for i
  
  # Change to data.frame for ggplot and rename columns
  g_hat_smooth = as.data.frame(g_hat_smooth)
  colnames(g_hat_smooth) = c("Re","Im")
  
  # Bar plot via ggplot
  bar = ggplot(g_hat_smooth)+ 
    geom_bar(aes(x=samp_f, y=Re), 
             stat="identity", # frequencies on y-axis
             fill="black",  # color bars
             alpha=0.7)+ # opacity
    theme_minimal()+ # white instead of grey background
    scale_x_continuous(breaks = seq(min(samp_f),max(samp_f),by=.5)) # adjust breaks x-axis tick marks
  # Print ggplot
  print(bar)
  
  # Plot original cosine wave:
  plot(x = time, y = g_t, type = "l", xlab = paste("Time in Seconds", "\n",
                                                   freq,"periods in 1 Second ==",freq,"Hz"), 
       ylab = "g_t = cos(2*pi*freq*time)", 
       col = "deepskyblue3") 
  
} # End of function fourier_trans

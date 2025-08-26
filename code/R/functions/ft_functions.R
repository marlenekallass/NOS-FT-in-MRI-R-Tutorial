fftshift = function(kspace) {  #
  if (is.null(dim(kspace))){
   
    n = length(kspace) # Evaluate n of rows
    reshape_row = c((n/2+1):n, 1:(n/2))  # rows/2+1 so it starts at first position second half
    kspace[reshape_row]  
    
  }
 else if (length(dim(kspace)) == 2){
  # For better readability - could also be done via ncol() and nrow()
  rows = nrow(kspace) # Evaluate n of rows
  cols = ncol(kspace) # ... n of cols
  reshape_row = c((rows/2+1):rows, 1:(rows/2))  # rows/2+1 so it starts at first position second half
  # not last position of first half!
  reshape_col = c((cols/2+1):cols, 1:(cols/2))  # same here...
  kspace[reshape_row,reshape_col]               # reshape k-space
 }
} # End of function fftshift()

fftshift1D = function(signal){
  x = c(1:length(signal))
  if((length(x) %% 2) == 0){
    xshift = x[c((length(x)/2+1):length(x),c(1:(length(x)/2)))]
  } # End if even
  else if((length(x) %% 2) != 0){
    xshift = x[c(ceiling(length(x)/2+1):length(x),c(1:ceiling((length(x)/2))))]
  } # End else if odd
  fftshift1D = signal[xshift] 
  return(fftshift1D)
} # End of fftshift1D

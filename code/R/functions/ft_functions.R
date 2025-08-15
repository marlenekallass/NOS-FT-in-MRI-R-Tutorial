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


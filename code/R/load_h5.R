#if (!requireNamespace("BiocManager", quietly = TRUE))
#  install.packages("BiocManager")
#BiocManager::install("rhdf5")

library(rhdf5)

k_filename = 'K_slice_test.h5'


# Datensatz lesen
k_real <- h5read(k_filename, "K_slice_real")
k_imag <- h5read(k_filename, "K_slice_imag")
k_slice <- k_real + 1i * k_imag



fftshift = function(kspace) {  #
  # For better readability - could also be done via ncol() and nrow()
  rows = nrow(kspace) # Evaluate n of rows
  cols = ncol(kspace) # ... n of cols
  reshape_row = c((rows/2+1):rows, 1:(rows/2))  # rows/2+1 so it starts at first position second half
  # not last position of first half!
  reshape_col = c((cols/2+1):cols, 1:(cols/2))  # same here...
  kspace[reshape_row,reshape_col]               # reshape k-space
} # End of function fftshift()




k_test <- k_slice

#unterschiede testen
#k_test <- abs(k_slice) #no reconstruction possible

#mirroring effect
#k_test <- k_real
#k_test <- k_imag

# see that k_space is originally in star form
asp_ratio <- dim(k_test)[1] / dim(k_test)[2] #sonst ist das bild komisch gestretched

image(abs(k_test),
      col = gray.colors(256, start = 0, end = 1), #dadurch ist die scale richtig
      axes = FALSE, asp = asp_ratio)


# Inverse FFT with shifting
im_test <- abs(fftshift(fft(fftshift(k_test), inverse = TRUE)))

#MATLAB imagesc(): rows → y-axis, columns → x-axis
#R image(): rows → x-axis, columns → y-axis
im_test <- t(im_test)

#MATLAB imagesc(): origin at top-left, y-axis down
#R image(): origin bottom-left, y-axis up
im_test <- im_test[,ncol(im_test):1]


asp_ratio <- dim(im_test)[1] / dim(im_test)[2]

image(abs(im_test),
      col = gray.colors(256, start = 0, end = 1), 
      axes = FALSE, asp = asp_ratio)

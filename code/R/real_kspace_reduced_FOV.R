#if (!requireNamespace("BiocManager", quietly = TRUE))
#  install.packages("BiocManager")
#BiocManager::install("rhdf5")

library(rhdf5)

k_filename = 'K_slice_test.h5'


# Datensatz lesen
k_real <- h5read(k_filename, "K_slice_real")
k_imag <- h5read(k_filename, "K_slice_imag")
k_slice <- k_real + 1i * k_imag
#k_slice <- k_imag



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
#k_test <- K
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

nr <- dim(im_test)[1]
nc <- dim(im_test)[2]

row_center <- nr / 2
col_center <- nc / 2

row_half <- round(nr / 4)
col_half <- round(nc / 4)

row_idx <- (row_center - row_half + 1):(row_center + row_half)
col_idx <- (col_center - col_half + 1):(col_center + col_half)

row_idx <- (row_center ):(row_center + row_half)
col_idx <- (col_center ):(col_center + col_half)


row_idx <- (row_center - row_half ):(row_center)
col_idx <- (col_center -col_half ):(col_center )


im_test_middle <- im_test[row_idx, col_idx]

image(abs(im_test_middle),
     col = gray.colors(256, start = 0, end = 1), 
    axes = FALSE, asp = asp_ratio)

max_k_x = dim(k_slice)[1]
max_k_y = dim(k_slice)[2]
f_nyquist_x = max_k_x/2
f_nyquist_y = max_k_y/2
pixel_ind_x = 1:max_k_x
pixel_ind_y = 1:max_k_y
modulation_f <- outer(
  pixel_ind_x, pixel_ind_y,
  function(x, y) exp(i * 2 * pi * (f_nyquist_x * x + f_nyquist_y * y))
)

modulated_k <- modulation_f * k_slice

k_test = Re(modulated_k)
#k_test = K

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



# Q12: top-right quadrant (ky from 1 to N/2, kx from N/2+1 to N)
Q12 <- Re(k_slice)
Ny <- nrow(Q12) * 2
Nx <- ncol(Q12) * 2
K <- matrix(0+0i, nrow = Ny, ncol = Nx)

# Q12 → top-right
K[1:(Ny/2), (Nx/2+1):Nx] <- Q12

# Q11 → top-left: horizontal mirror of Q12
K[1:(Ny/2), 1:(Nx/2)] <- Conj(Q12[, (Nx/2):1])

# Q22 → bottom-right: vertical mirror
K[(Ny/2+1):Ny, (Nx/2+1):Nx] <- Conj(Q12[(Ny/2):1, ])

# Q21 → bottom-left: diagonal mirror
K[(Ny/2+1):Ny, 1:(Nx/2)] <- Conj(Q12[(Ny/2):1, (Nx/2):1])

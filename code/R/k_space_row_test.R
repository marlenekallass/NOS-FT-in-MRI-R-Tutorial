


plot_kspace_line <- function(k_slice, axis = c("row", "col"), index = 1) {
  axis <- match.arg(axis)
  
  # Select the line (row or column)
  line_data <- if (axis == "row") {
    k_slice[index, ]
  } else {
    k_slice[, index]
  }
  
  plot(line_data, type = "l", lwd = 2,
       ylab = "Value", xlab = paste(axis, "index"), 
       main = paste("K-space", axis, "=", index))
}

plot_kspace_line(k_real,'row',10)



# Check if k_imag is a shifted version of k_real
k_imag= k_imag[,,1]
k_real = k_real[,,1]

# Function to check if matrix B is a shifted version of A by (shift_row, shift_col)
is_shifted <- function(A, B, shift_row = 0, shift_col = 0) {
  nr <- nrow(A)
  nc <- ncol(A)
  
  # Circular shift B by (-shift_row, -shift_col) to align with A
  B_shifted <- B[
    ((1:nr - shift_row - 1) %% nr) + 1,
    ((1:nc - shift_col - 1) %% nc) + 1
  ]
  
  identical(A, B_shifted)
}

# Try shifts over some range, e.g. -5 to 5
for (r in -600:600) {
  for (c in -1:1) {
    res <- is_shifted(k_real, k_imag, r, c)
    if (isTRUE(res)) {
      cat("k_imag is k_real shifted by (rows, cols):", r, c, "\n")
    }
  }
}

phase_image_from_ift <- function(k_complex) {
  im <- fft(k_complex, inverse = TRUE) / length(k_complex)
  phase_mat <- Arg(im)
  image(phase_mat, col = viridis::viridis(256), axes = FALSE, main = "Phase of IFT(k)")
  invisible(phase_mat)
}

# Usage example:

phase_image_from_ift(k_slice[,,1])

FOV_mm <- 256
delta_x_mm <- 1
N_enc <- 512

N_req <- FOV_mm / delta_x_mm
factor <- N_enc / N_req
skip_factor = 1
## undersampling
# Subsample k-space
k_sub <- k_slice[seq(1, nrow(k_slice[,,1]), by=skip_factor), seq(1, ncol(k_slice[,,1]), by=1),1]
k_sub = Re(k_sub)
# Assume k_test is your k-space slice (complex matrix)
k_test <- t(k_sub)

# Shift k-space
k_test_shifted <- fftshift(k_test)

# Correct orientation for plotting k-space
image(log(1 + abs(k_test_shifted)), col = gray.colors(256), axes = FALSE, main = "Shifted k-space (log mag)")


image(log(1 + abs(k_test)), col = gray.colors(256), axes = FALSE, main = "Shifted k-space (log mag)")

# Inverse FFT requires correct orientation:
im_test <- abs(fftshift(fft(k_test_shifted)))

# Transpose reconstructed image for display
image(im_test, col = gray.colors(256), axes = FALSE,asp = 1, main = "Reconstructed image via inverse FFT")



plot_cosine_sums <- function(f = 1, L = 1, resolution = 300) {
  x <- seq(-L, L, length.out = resolution)
  y <- seq(-L, L, length.out = resolution)
  grid <- expand.grid(x = x, y = y)
  
  z1 <- cos(2 * pi * f * grid$x) + cos(2 * pi * f * grid$y)
  z2 <- cos(2 * pi * f * (grid$x + grid$y) / sqrt(2))
  
  z1_mat <- matrix(z1, nrow = resolution, byrow = FALSE)
  z2_mat <- matrix(z2, nrow = resolution, byrow = FALSE)
  
  par(mfrow = c(1, 2))
  image(x, y, z1_mat, main = "cos(x) + cos(y)", col = terrain.colors(100),
        xlab = "x", ylab = "y", asp = 1, useRaster = TRUE)
  image(x, y, z2_mat, main = "cos((x+y)/√2)", col = terrain.colors(100),
        xlab = "x", ylab = "y", asp = 1, useRaster = TRUE)
}

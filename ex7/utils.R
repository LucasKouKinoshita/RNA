
# Função para criar a malha de pontos
criar_grid <- function(x, n = 200) {
  x1_range <- seq(min(x[,1]) - 0.5, max(x[,1]) + 0.5, length.out = n)
  x2_range <- seq(min(x[,2]) - 0.5, max(x[,2]) + 0.5, length.out = n)
  grid <- expand.grid(x1 = x1_range, x2 = x2_range)
  return(as.matrix(grid))
}

plot_classification <- function(xin, yin, modelo, titulo) {
  grid <- criar_grid(xin)
  pred_grid <- YRBF(grid, modelo)
  pred_grid <- apply(pred_grid, 1, which.max) # Pegando a classe com maior ativação
  
  plot(xin[,1], xin[,2], col = yin, pch = 19, main = titulo, xlab = "", ylab = "")
  points(grid[,1], grid[,2], col = pred_grid, pch = ".", cex = 0.5)
}

graphics.off()
rm(list = ls())
library("mlbench")
library("corrplot")
library(plot3D)
library('corpcor')

library(mlbench)
library(MASS) # para ginv (pseudoinversa)

plot_elm_decision_surface <- function(data, p = 60, s = 1, title = "ELM Decision Surface") {
  # Dados
  X <- data$x * s
  Y <- ifelse(data$classes == levels(data$classes)[1], -1, 1)
  Y <- matrix(Y, ncol = 1)
  
  # Parâmetros
  Z <- matrix(runif(3 * p, -0.5, 0.5), nrow = 3)
  Xaug <- cbind(1, X)
  H <- tanh(Xaug %*% Z)
  Haug <- cbind(1, H)
  w <- ginv(Haug) %*% Y
  
  # Previsão e erro
  Yhat <- sign(Haug %*% w)
  e_train <- sum((Y - Yhat)^2) / 4
  cat("Erro de treinamento:", e_train, "\n")
  
  # Plot
  plot(X, col = ifelse(Y == -1, "red", "blue"),
       xlab = "x1", ylab = "x2", main = title, xlim = c(min(X[,1])-1, max(X[,1])+1),
       ylim = c(min(X[,2])-1, max(X[,2])+1))
  
  # Malha para contorno
  seqx1 <- seq(min(X[,1])-1, max(X[,1])+1, length.out = 200)
  seqx2 <- seq(min(X[,2])-1, max(X[,2])+1, length.out = 200)
  grid <- expand.grid(x1 = seqx1, x2 = seqx2)
  grid_aug <- cbind(1, as.matrix(grid))
  hgrid <- cbind(1, tanh(grid_aug %*% Z))
  pred_grid <- matrix(sign(hgrid %*% w), nrow = 200)
  
  contour(seqx1, seqx2, pred_grid, levels = 0, add = TRUE, drawlabels = FALSE)
}


# mlbench.2dnormals
data1 <- mlbench.2dnormals(200)
plot_elm_decision_surface(data1, title = "2D Normals")

# mlbench.xor
data2 <- mlbench.xor(100)
plot_elm_decision_surface(data2, title = "XOR")

# mlbench.circle
data3 <- mlbench.circle(100)
plot_elm_decision_surface(data3, title = "Circle")

# mlbench.spirals
data4 <- mlbench.spirals(100, sd = 0.05)
plot_elm_decision_surface(data4, title = "Spirals")




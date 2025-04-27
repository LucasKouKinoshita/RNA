graphics.off()
rm(list = ls())
library("mlbench")
library("corrplot")
library(plot3D)
library('corpcor')

source("D:/github/RNA/aulas/RBF.R")
source("D:/github/RNA/ex7/utils.R")

p <- 12 # Número de neurônios (centros)  
r <- 1 # dispersão

# Função para plotar a classificação
plot_classification <- function(xin, yin, modelo, titulo) {
  grid <- criar_grid(xin)
  pred_grid <- YRBF(grid, modelo)
  pred_grid <- apply(pred_grid, 1, which.max) # Classe com maior ativação
  
  plot(xin[,1], xin[,2], col = yin, pch = 19, main = titulo, xlab = "", ylab = "")
  points(grid[,1], grid[,2], col = pred_grid, pch = ".", cex = 0.5)
}

# Normals
normals <- mlbench.2dnormals(200)
xin_normals <- as.matrix(normals$x)
yin_normals <- as.numeric(normals$classes)

modelo_normals <- RBF(xin_normals, diag(1,2)[yin_normals,], p, r)
plot_classification(xin_normals, yin_normals, modelo_normals, "2D Normals")

# XOR
xor <- mlbench.xor(100)
xin_xor <- as.matrix(xor$x)
yin_xor <- as.numeric(xor$classes)

modelo_xor <- RBF(xin_xor, diag(1,2)[yin_xor,], p, r)
plot_classification(xin_xor, yin_xor, modelo_xor, "XOR")

# Circle
circle <- mlbench.circle(100)
xin_circle <- as.matrix(circle$x)
yin_circle <- as.numeric(circle$classes)

modelo_circle <- RBF(xin_circle, diag(1,2)[yin_circle,], p, r)
plot_classification(xin_circle, yin_circle, modelo_circle, "Circle")

# Spirals
spiral <- mlbench.spirals(100, cycles=1, sd=0.05)
xin_spiral <- as.matrix(spiral$x)
yin_spiral <- as.numeric(spiral$classes)

modelo_spiral <- RBF(xin_spiral, diag(1,2)[yin_spiral,], p, r)
plot_classification(xin_spiral, yin_spiral, modelo_spiral, "Spirals")

# Sinc
x <- runif(100, -15, 15)
y <- -sin(x)/x + rnorm(100, 0 , 0.05)
xin <- as.matrix(x)
yin <- as.matrix(y)
p <- 16
r <- 1  
modeloRBF <- RBF(xin, yin, p, r)
yhat <- YRBF(xin, modeloRBF)
plot(x, y, col = 'blue', main = 'sinc(x)', xlab = 'x', ylab = 'y')
points(x, yhat, col = 'red')
legend("topright", legend = c("Original", "Aproximação"), 
       col = c("blue", "red"), pch = c(1,1))

xtest <- runif(50, -15, 15)
ytest <- -sin(xtest)/xtest + rnorm(50, 0, 0.05)
xtest <- as.matrix(xtest)
yhat <- YRBF(xtest, modeloRBF)

mse <- mean((ytest - yhat)^2)
print(mse)


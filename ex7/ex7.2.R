graphics.off()
rm(list = ls())
library("mlbench")
library("corrplot")
library(plot3D)
library('corpcor')

source("D:/github/RNA/aulas/RBF.R")
source("D:/github/RNA/ex7/utils.R")
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

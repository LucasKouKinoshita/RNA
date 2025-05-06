#graphics.off()
rm(list = ls())
library("mlbench")
library(RSNNS)
library(plot3D)
library('corpcor')
source("C:/personal/RNA/aulas/RBF.R") 
library("MASS")
library("matlib")

### Regularização

N <- 200
sd <- 0.45
xg1 <- matrix(rnorm(N*2), ncol =2, nrow = N) * sd + matrix(c(2,2), ncol = 2, nrow = N, byrow = 1)
xg2 <- matrix(rnorm(N*2), ncol =2, nrow = N) * sd + matrix(c(4,4), ncol = 2, nrow = N, byrow = 1)
xg3 <- matrix(rnorm(N*2), ncol =2, nrow = N) * sd + matrix(c(2,4), ncol = 2, nrow = N, byrow = 1)
xg4 <- matrix(rnorm(N*2), ncol =2, nrow = N) * sd + matrix(c(4,2), ncol = 2, nrow = N, byrow = 1)
xall <- rbind(xg1, xg2, xg3, xg4)
yall <- rbind(matrix(-1, ncol = 1, nrow = 2*N), matrix(1, ncol = 1, nrow = 2*N))
xc1 <- rbind(xg1, xg2)
xc2 <- rbind(xg3, xg4)
plot(xc1[,1], xc1[,2], col = 'red', xlim = c(0,6), ylim = c(0,6), xlab ='x1', ylab = 'x2')
par(new=TRUE)
plot(xc2[,1], xc2[,2], col = 'blue', xlim = c(0,6), ylim = c(0,6), xlab ='x1', ylab = 'x2')
X <- rbind(xc1, xc2)
Y <- rbind(matrix(-1, ncol = 1, nrow = 2*N), matrix(1, ncol = 1, nrow = 2*N))

p <- 100
Z <- matrix(runif(3*p, -0.5, 0.5), nrow = 3, ncol = p)

Xaug <- cbind(1,X)
H <- tanh(Xaug %*% Z)
Haug <- cbind(1, H)

seqL <- seq(0.05, 2, 0.05)
evec <- matrix(nrow = length(seqL), ncol = 1)
wvec <- matrix(nrow = length(seqL), ncol = 1)
cl <- 0
for(L in seqL){
  cl <- cl + 1
  #w <- pseudoinverse(Haug) %*% Y
  w <- pseudoinverse(t(Haug) %*% Haug + L * diag(p+1)) %*% t(Haug) %*% Y
  
  Yhat_train <- Haug %*% w
  e_train <- Yhat_train - Y
  
  evec[cl] <- e_train
  wvec[cl] <- t(w)%*%w # Norma quadatica
  
}

plot(evec, wvec, type = 'b')

plot(wvec)
plot(evec)


calcG1var <- function(x,m,r) (exp(0.5 * ((x-m)^2/(2*r^2))))
calcA <- function(H, L) (t(H) %*% H + L)
calcP <- function(H, A, N) (diag(N) - H %*% solve(A) %*% t(H))
calcL00 <- function(P,y,N) ((t(y) %*% P %*% (t(solve(diag(diag(P)))) %*% solve(diag(diag(P)))) %*% P %*% y)/ N)
calcGCV <- function(P,y,N) (N * (t(y) %*% (P %*% P) %*% y) / (sum(diag(P))) ^ 2)

w <- pseudoinverse(t(Haug) %*% Haug + L * diag(p+1)) %*% t(Haug) %*% Y

Yhat_train <- Haug %*% w
e_train <- Yhat_train - Y







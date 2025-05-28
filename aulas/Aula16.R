graphics.off()
rm(list = ls())

library(RSNNS)
library(plot3D)
library('corpcor')
library("matlib")

source("C:/personal/RNA/aulas/RBF.R") 

### Regularização LOO


calcG1var <- function(x,m,r) (exp(0.5 * ((x-m)^2/(2*r^2))))
calcA <- function(H, L) (t(H) %*% H + L)
calcP <- function(H, A, N) (diag(N) - H %*% solve(A) %*% t(H))
calcLOO <- function(P,y,N) ((t(y) %*% P %*% (t(solve(diag(diag(P)))) %*% solve(diag(diag(P)))) %*% P %*% y)/ N)
calcGCV <- function(P,y,N) (N * (t(y) %*% (P %*% P) %*% y) / (sum(diag(P))) ^ 2)


N <- 100
sd <- 0.4
xg1 <- matrix(rnorm(N*2), ncol =2, nrow = N) * sd + matrix(c(2,2), ncol = 2, nrow = N, byrow = 1)
xg2 <- matrix(rnorm(N*2), ncol =2, nrow = N) * sd + matrix(c(4,4), ncol = 2, nrow = N, byrow = 1)
xg3 <- matrix(rnorm(N*2), ncol =2, nrow = N) * sd + matrix(c(2,4), ncol = 2, nrow = N, byrow = 1)
xg4 <- matrix(rnorm(N*2), ncol =2, nrow = N) * sd + matrix(c(4,2), ncol = 2, nrow = N, byrow = 1)

xc1 <- rbind(xg1, xg2)
xc2 <- rbind(xg3, xg4)
plot(xc1[,1], xc1[,2], col = 'red', xlim = c(0,6), ylim = c(0,6), xlab ='x1', ylab = 'x2')
par(new=TRUE)
plot(xc2[,1], xc2[,2], col = 'blue', xlim = c(0,6), ylim = c(0,6), xlab ='x1', ylab = 'x2')
X <- rbind(xc1, xc2)
Y <- rbind(matrix(-1, ncol = 1, nrow = 2*N), matrix(1, ncol = 1, nrow = 2*N))

p <- 100
r <- 20

Z <- matrix(runif(3*p, -0.5, 0.5), nrow = 3, ncol = p)

Xaug <- cbind(1,X)
H <- tanh(Xaug %*% Z)
Haug <- cbind(1, H)
N <- 4*N # N total


seqL <- seq(0.05, 1, 0.05)
evec <- matrix(nrow = length(seqL), ncol = 1)
LOOvec <- matrix(nrow = length(seqL), ncol = 1)
wvec <- matrix(nrow = length(seqL), ncol = 1)
cl <- 0
for(L in seqL){
  cl <- cl + 1
  
  #ELM
  w <- pseudoinverse(t(Haug) %*% Haug + L * diag(p+1)) %*% t(Haug) %*% Y
  Yhat_train <- Haug %*% w
  print(w)
  #RBF
  #RBFmod <- RBF(X, Y, p, r)
  #Yhat_train <- YRBF(X, RBFmod)
  #w <- as.matrix(unlist(RBFmod[4]))
  
  e_train <- Yhat_train - Y
  
  A <- calcA(H, L*diag(p))
  P <- calcP(H, A, N)
  LOO <- calcLOO(P, Yhat_train, N)
  LOOvec[cl] <- LOO
  #wvec[cl] <- w
  #print(LOO)
  
  Je <- t(Y) %*% (P %*% P) %*% Y
  Jew <- t(Y - Yhat_train) %*% (Y - Yhat_train)
  
  Jw<- t(Y)%*%(P - P%*%P)%*%Y
  Jww <- t(w) %*% (L * diag(p+1)) %*% w
  
  J <- t(Y) %*% P %*% Y
  Jsum<-Jew+Jww
  #print(cbind(Je, Jew))
  #print(cbind(Jw, Jww))
  #print(cbind(J,Jsum))
}

plot(LOOvec)
bestLOOidx <- which.min(LOOvec) #melhor valor de LOO index
bestL <- seqL[bestLOOidx] # melhor lambda
bestw <- wvec[bestLOOidx] # melhor vetor de pesos
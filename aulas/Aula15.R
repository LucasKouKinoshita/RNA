graphics.off()
rm(list = ls())

library(RSNNS)
library(plot3D)
library('corpcor')
library("matlib")

### Regularização

N <- 200
sd <- 0.45
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

p <- 50
Z <- matrix(runif(3*p, -0.5, 0.5), nrow = 3, ncol = p)

Xaug <- cbind(1,X)
H <- tanh(Xaug %*% Z)
Haug <- cbind(1, H)

N <- 4*N
calcG1var <- function(x,m,r) (exp(0.5 * ((x-m)^2/(2*r^2))))
calcA <- function(H, L) (t(H) %*% H + L)
calcP <- function(H, A, N) (diag(N) - H %*% solve(A) %*% t(H))
calcL00 <- function(P,y,N) ((t(y) %*% P %*% (t(solve(diag(diag(P)))) %*% solve(diag(diag(P)))) %*% P %*% y)/ N)
calcGCV <- function(P,y,N) (N * (t(y) %*% (P %*% P) %*% y) / (sum(diag(P))) ^ 2)

seqL <- seq(0.05, 2, 0.05)
evec <- matrix(nrow = length(seqL), ncol = 1)
wvec <- matrix(nrow = length(seqL), ncol = 1)
Jevec <- matrix(nrow = length(seqL), ncol = 1)
Jwvec <- matrix(nrow = length(seqL), ncol = 1)

Jewvec <- matrix(nrow = length(seqL), ncol = 1)
Jwwvec <- matrix(nrow = length(seqL), ncol = 1)
cl <- 0
for(L in seqL){
  cl <- cl + 1
  w <- pseudoinverse(t(Haug) %*% Haug + L * diag(p+1)) %*% t(Haug) %*% Y
  
  Yhat_train <- Haug %*% w
  e_train <- Yhat_train - Y
  
  w <- pseudoinverse(t(Haug) %*% Haug + L * diag(p+1)) %*% t(Haug) %*% Y
  
  Yhat_train <- Haug %*% w
  e_train <- Yhat_train - Y
  
  A <- calcA(H, L*diag(p))
  P <- calcP(H, A, N)
  
  Je <- t(Y) %*% (P %*% P) %*% Y
  Jew <- t(Y - Yhat_train) %*% (Y - Yhat_train)
  
  Jw<- t(Y)%*%(P - P%*%P)%*%Y
  Jww <- t(w) %*% (L * diag(p+1)) %*% w
  
  J <- t(Y) %*% P %*% Y
  Jsum<-Jew+Jww
  #print(cbind(Je, Jew))
  #print(cbind(Jw, Jww))
  #print(cbind(J,Jsum))
  
  Jwwvec[cl]<- Jww
  Jewvec[cl]<- Jew
  
  Jwvec[cl] <- Jw
  Jevec[cl]<- Je
  
  evec[cl] <- mean(e_train^2)
  wvec[cl] <- t(w)%*%w # Norma quadatica
  
}

plot(evec, wvec, type = 'b')

plot(wvec)
plot(evec)

plot(Jevec)
plot(Jewvec)

plot(Jwvec)
plot(Jwwvec)


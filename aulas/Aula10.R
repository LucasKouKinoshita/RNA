graphics.off()
rm(list = ls())
library("mlbench")
library("corrplot")
library(plot3D)
library('corpcor')

s1 <- 0.6
s2 <- 0.6
nc <- 60
xc1_1 <- matrix(rnorm(nc*2), ncol = 2)*s1 + t(matrix((c(2,2)), ncol = nc, nrow = 2))
xc1_2 <- matrix(rnorm(nc*2), ncol = 2)*s1 + t(matrix((c(4,4)), ncol = nc, nrow = 2))
xc1 <- rbind(xc1_1, xc1_2)

xc2_1 <- matrix(rnorm(nc*2), ncol = 2)*s2 + t(matrix((c(2,4)), ncol = nc, nrow = 2))
xc2_2 <- matrix(rnorm(nc*2), ncol = 2)*s2 + t(matrix((c(4,2)), ncol = nc, nrow = 2))
xc2 <- rbind(xc2_1, xc2_2)


plot(xc1[,1], xc1[,2], xlim = c(0,6), ylim = c(0,6), xlab = 'x1', ylab = 'x2', col = 'red')
par(new=TRUE)
plot(xc2[,1], xc2[,2], xlim = c(0,6), ylim = c(0,6), xlab = 'x1', ylab = 'x2', col = 'blue')

X <- rbind(xc1, xc2)
Y <- rbind(matrix(-1, ncol = 1, nrow = 2*nc), matrix(1, ncol = 1, nrow = 2*nc))

p <- 60
Z <- matrix(runif(3*p, -0.5, 0.5), nrow = 3, ncol = p)
Xaug <- cbind(1,X)
H <- tanh(Xaug %*% Z)

L <- 0.0
Haug <- cbind(1, H)

w <- pseudoinverse(Haug) %*% Y

Yhat_train <- sign(Haug %*% w)
e_train <- sum((Y-Yhat_train)^2)/4
print(e_train)

seqx1x2 <- seq(0, 6, 0.1)
lseq <- length(seqx1x2)
MZ <- matrix(nrow = lseq, ncol = lseq)
cr <- 0
for (i in 1:lseq){
  for (j in 1:lseq) {
    cr <- cr + 1
    
    x1 <- seqx1x2[i]
    x2 <- seqx1x2[j]
    
    x1x2 <- as.matrix(cbind(1, x1,x2))
    h1<-cbind(1, tanh(x1x2 %*% Z))
    MZ[i,j] <- sign(h1 %*% w)
    #print(MZ[i,j])
  }
}

par(new=TRUE)
contour(seqx1x2, seqx1x2, MZ, nlevels = 0)



############################ dados reais #####################################
ELM <- function(xin, yin, p, par) {
  n <- dim(xin)[2]
  
  if(par == 1){
    xin <- cbind(1, xin)
    Z <- matrix(runif((n+1)*p, -0.5, 0.5), nrow = n, ncol = p)
  }
  
  Z <- matrix(runif(n*p, -0.5, 0.5), nrow = n, ncol = p)
  Xaug <- cbind(1,xin)
  H <- tanh(Xaug %*% Z)
  
  L <- 0.0
  Haug <- cbind(1, H)
  
  w <- pseudoinverse(Haug) %*% yd
  
  Yhat_train <- sign(Haug %*% w)
  e_train <- sum((yd-Yhat_train)^2)/4
  print(e_train)  
  
  
} 


# Load BreastCancer
data("BreastCancer")
df <- BreastCancer
df <- df[, -1]  # drop 'Id'
df <- na.omit(df)

# Convert factor columns to numeric (except 'Class')
df[, 1:9] <- lapply(df[, 1:9], function(x) as.numeric(as.character(x)))
df$Class <- ifelse(df$Class == "malignant", 1, 0)

# Matrix version
X <- as.matrix(df[, 1:9])
Y <- as.matrix(df$Class)
N <- nrow(X)

ntrain <- nrow(df)*0.7  # you can adjust this
reps <- 10
acc_treino <- numeric(reps)
acc_teste <- numeric(reps)




for (r in reps) {
  
  # Shuffle indices
  idx <- sample(N)
  
  # Train/Test split
  train_idx <- idx[1:ntrain]
  test_idx <- idx[(ntrain + 1):N]
  
  xin <- X[train_idx, ]
  yd <- Y[train_idx]
  xinteste <- X[test_idx, ]
  yteste <- Y[test_idx]
  

  
}






graphics.off()
rm(list = ls())
library("mlbench")
library(RSNNS)
library(plot3D)
library('corpcor')
source("C:/personal/RNA/aulas/RBF.R") 

data("airquality")
airq <- na.omit(airquality)
xyall <- data.matrix(airq)
xyall <- scale(xyall)

xysplit <- splitForTrainingAndTest(xyall[,(2:4)], xyall[,1], ratio = 0.3)
xtr <- data.matrix(xysplit$inputsTrain)
ytr <- data.matrix(xysplit$targetsTrain)
xtst <- data.matrix(xysplit$inputsTest)
ytst <- data.matrix(xysplit$targetsTest)

xytr <- data.frame(xtr)
xytr[,"Ozone"] <- ytr 
plot(data.frame(xytr))

modRBF <- RBF(xtr, ytr, 3, 5)
Yhat_tst <- YRBF(xtst, modRBF)
plot(ytst, type = 'b', col = 'red', xlim = c(0,40), ylim = c(min(ytst), max(ytst) + 1),
     xlab = "Amostra", ylab = "ytst, Yhat_tst")
par(new = TRUE)
plot(Yhat_tst, type = 'l', col = 'blue', 
     xlim = c(0,40), ylim = c(min(ytst), max(ytst) + 1),
     xlab = "Amostra", ylab = "ytst, Yhat_tst")



####################################
rm(list = ls())
minx <- -10
maxx <- 12
xrange <- seq(minx, maxx, 0.5)

fe<- (xrange)^2-3
fw<- sin(xrange)^30 + 20

ymaxfe <- max(fe)
ymaxfw <- max(fw)

ymax <- max(ymaxfe, ymaxfw)

plot(xrange, fe, xlim = c(minx, maxx), ylim = c(0, ymax), col = 'red')
par(new = TRUE)
plot(xrange, fw, xlim = c(minx, maxx), ylim = c(0, ymax), col = 'blue')

plot(fw,fe)

##################################
N <- 60
xg1 <- matrix(rnorm(N*2), ncol =2, nrow = N) * 0.5 + matrix(c(2,2), ncol = 2, nrow = N, byrow = 1)
xg2 <- matrix(rnorm(N*2), ncol =2, nrow = N) * 0.5 + matrix(c(4,4), ncol = 2, nrow = N, byrow = 1)
xg3 <- matrix(rnorm(N*2), ncol =2, nrow = N) * 0.5 + matrix(c(2,4), ncol = 2, nrow = N, byrow = 1)
xg4 <- matrix(rnorm(N*2), ncol =2, nrow = N) * 0.5 + matrix(c(4,2), ncol = 2, nrow = N, byrow = 1)
xall <- rbind(xg1, xg2, xg3, xg4)
yall <- rbind(matrix(-1, ncol = 1, nrow = 2*N), matrix(1, ncol = 1, nrow = 2*N))
xc1 <- rbind(xg1, xg2)
xc2 <- rbind(xg3, xg4)
plot(xc1[,1], xc1[,2], col = 'red', xlim = c(0,6), ylim = c(0,6))
par(new=TRUE)
plot(xc2[,1], xc2[,2], col = 'blue', xlim = c(0,6), ylim = c(0,6))
X <- rbind(xc1, xc2)
Y <- rbind(matrix(-1, ncol = 1, nrow = 2*N), matrix(1, ncol = 1, nrow = 2*N))

p <- 100
Z <- matrix(runif(3*p, -0.5, 0.5), nrow = 3, ncol = p)

Xaug <- cbind(1,X)
H <- tanh(Xaug %*% Z)

L <- 0.00001
Haug <- cbind(1, H)
w <- pseudoinverse(Haug) %*% Y
w <- solve(t(Haug) %*% Haug + L * diag(p+1)) %*% t(Haug) %*% Y

Yhat_train <- sign(Haug %*% w)
e_train <- sum((Y-Yhat_train)^2)/(4*120)
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

















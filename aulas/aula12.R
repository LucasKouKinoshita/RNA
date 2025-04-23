graphics.off()
rm(list = ls())
library("mlbench")
library("corrplot")
library(plot3D)
library('corpcor')

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

retlist1 <- kmeans(xc1, 2, 10)
retlist2 <- kmeans(xc2, 2, 10)

m1all <- retlist1[[1]]
m11 <- as.matrix(m1all[1,])
m12 <- as.matrix(m1all[2,])
S11 <- cov(xc1[which(retlist1[[2]] == 1), ])
S12 <- cov(xc1[which(retlist1[[2]] == 2), ])

m2all <- retlist2[[1]]
m21 <- as.matrix(m2all[1,])
m22 <- as.matrix(m2all[2,])
S21 <- cov(xc2[which(retlist2[[2]] == 1), ])
S22 <- cov(xc2[which(retlist2[[2]] == 2), ])

Hxt <- matrix(nrow = 4*N, ncol = 5)
for(i in 1:(4*N)){
  xtr <- matrix(xall[i,], ncol = 1, nrow = 2 )
  h1 <- pdfnvar(xtr, m11, S11, 2)
  h2 <- pdfnvar(xtr, m12, S12, 2)
  h3 <- pdfnvar(xtr, m21, S21, 2)
  h4 <- pdfnvar(xtr, m22, S22, 2)
  Hxt[i,] <- rbind(1, h1, h2, h3, h4)
}

w <- pseudoinverse(Hxt) %*% yall

seqx1x2 <- seq(0, 6, 0.2)
npgrid <- length(seqx1x2)
MZ <- matrix(nrow = npgrid, ncol = npgrid)
for(i in 1:lseq){
  for(j in 1:lseq){
    
    x1 <- seqx1x2[i]
    x2 <- seqx1x2[j]
    x1x2 <- as.matrix(cbind(x1,x2))
    x1x2 <- matrix(x1x2, nrow = 2, ncol = 1)
    h1 <- pdfnvar(x1x2, m11, S11, 2)
    h2 <- pdfnvar(x1x2, m12, S12, 2)
    h3 <- pdfnvar(x1x2, m21, S21, 2)
    h4 <- pdfnvar(x1x2, m22, S22, 2)
    H <- rbind(1, h1, h2, h3, h4)
    
    MZ[i,j] <- 
  }
}


















#plot(xall[,1], xall[,2], col = unlist(retlist[2]), xlim=c(0,6), ylim = c(0,6))



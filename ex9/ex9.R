graphics.off()
rm(list = ls())
library("mlbench")
library("corrplot")
library(plot3D)
library('corpcor')

### backpropagation

sech2<-function(u){ ## tangente hiperbolica ao quadrado
  return(((2/(exp(u)+exp(-u)))*(2/(exp(u)+exp(-u)))))
}
#xrange <- seq(-3, 3, 0.1)
#plot(xrange, (sech2(xrange)), type = 'l')

x_range = c(0, 2*pi, 0.01)
N <- 45
noise_sd <- 0.1
xtr <- runif(N, min = x_range[1], max = x_range[2])
ytr <- sin(xtr) + rnorm(N, 0, noise_sd)

xtst <- seq(0.0, 2 * pi, 0.01)
ytst <- sin(xtst)

plot(x = xtr, y = ytr, col = 'blue', xaxt = "n", xlab = "x", ylab = "y")
par(new = TRUE)
plot(x = xtst, y = ytst, type = 'line', col = 'red', xaxt = "n", yaxt = "n", xlab = "", ylab = "")

w14 <- runif(1)-0.5
w15 <- runif(1)-0.5

#w24 <- runif(1)-0.5
#w25 <- runif(1)-0.5

w34 <- runif(1)-0.5
w35 <- runif(1)-0.5


w47 <- runif(1)-0.5
#w48 <- runif(1)-0.5


w57 <- runif(1)-0.5
#w58 <- runif(1)-0.5


w67 <- runif(1)-0.5
#w68 <- runif(1)-0.5

maxepocas <- 1000
nepocas <- 0

tol <- 0.00001 # tolerancia
eepoca <-tol + 1 # erro epoca
evec <- matrix(maxepocas)
eta <- 0.01 # passo

i3 <- +1
i6 <- +1
#xproj <- matrix(0, nrow = 4, ncol = 2)
while((nepocas < maxepocas) && (eepoca > tol)){
  ei2 <-0 # erro quadrático acumulado por época
  
  xseq <- sample(N)
  for(i in 1:N){
    irand <- xseq[i]
    i1 <- xtr[irand]
    #i2 <- xtr[irand,2]
    
    u4 <- i1*w14 + i3*w34
    i4 <- tanh(u4)
    
    u5 <- i1*w15 + i3*w35
    i5 <- tanh(u5)
    
    u7 <- i5*w57 + i4*w47 + i6*w67
    i7 <- tanh(u7)
    
    #u8 <- i5*w58 + i4*w48 + i6*w68
    #i8 <- tanh(u8)
    
    y7 <- ytr[irand]
    #y8 <- ytr[irand, 2]
    
    e7 <- y7 - i7
    #e8 <- y8 - i8
    
    # Calculo da variação dos pesos da camada de saida
    d7 <- e7*sech2(u7)
    #d8 <- e8*sech2(u8)
    
    dw47 = eta*d7*i4
    #dw48 = eta*d8*i4
    
    dw57 = eta*d7*i5
    #dw58 = eta*d8*i5
    
    dw67 = eta*d7*i6
    #dw68 = eta*d8*i6
    
    # Calculo da variaçaõ dos pesos da camada intermediáia
    e4 <- (d7*w47)
    e5 <- (d7*w57)
    
    d4 <- e4 * sech2(u4)
    d5 <- e5 * sech2(u5)
    
    dw14 = eta*d4*i1
    dw15 = eta*d5*i1
    
    #dw24 = eta*d4*i2
    #dw25 = eta*d5*i2
    
    dw34 = eta*d4*i3
    dw35 = eta*d5*i3
    
    # Atualização de todos os pesos
    
    w14 <- w14 + dw14
    w15 <- w15 + dw15
    #w24 <- w24 + dw24
    #w25 <- w25 + dw25
    w34 <- w34 + dw34
    w35 <- w35 + dw35
    
    w47 <- w47 + dw47
    #w48 <- w48 + dw48
    w57 <- w57 + dw57
    #w58 <- w58 + dw58
    w67 <- w67 + dw67
    #w68 <- w68 + dw68
    
    ei2 <- ei2 + (e7^2)/4 # acrescenta erro normalizado
    #xproj[i,1] <- i4
    #xproj[i,2] <- i5
  }
  
  nepocas <- nepocas + 1
  evec[nepocas] <- ei2/N # erro po amostra
  
  eepoca <- evec[nepocas]
}

plot((1:nepocas),evec)

plot(1:nepocas, evec[1:nepocas], type = 'l', col = 'darkgreen', xlab = "Epoch", ylab = "Mean Squared Error (MSE)", main = "Training Error Convergence")
abline(h = tol, col = "red", lty = 2)
legend("topright", legend=c("MSE", "Tolerance"), col=c("darkgreen", "red"), lty=c(1,2))

yhat_tst <- numeric(length(xtst))
for (k in 1:length(xtst)) {
  i1 <- xtst[k]
  
  u4 <- i1 * w14 + i3 * w34
  i4 <- tanh(u4)
  
  u5 <- i1 * w15 + i3 * w35
  i5 <- tanh(u5)
  
  u7 <- i4 * w47 + i5 * w57 + i6 * w67
  yhat_tst[k] <- tanh(u7)
}

plot(x = xtr, y = ytr, col = 'blue', pch = 1, xlab = "x", ylab = "y", 
     main = "MLP Sine Approximation (After Training)", 
     xlim=c(x_range[1], x_range[2]), ylim=c(-1.5, 1.5))

lines(x = xtst, y = ytst, col = 'red') 

lines(x = xtst, y = yhat_tst, col = 'black', lty = 2) 
legend("topright", legend=c("xtr", "y", "y predito"), 
       col=c("blue", "red", "black"), pch=c(1,NA,NA), 
       lty=c(NA,1,2), lwd=c(NA,2,2))

cat("\nFinal Weights:\n")
cat("w14:", w14, " w15:", w15, "\n")
cat("w34:", w34, " w35:", w35, "\n")
cat("w47:", w47, " w57:", w57, "\n")
cat("w67:", w67, "\n")



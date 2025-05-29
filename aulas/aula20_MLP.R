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

xtr <- matrix(c(0,0,0,1,1,0,1,1), ncol = 2, nrow = 4, byrow = TRUE)
N <- 4
ytr <- matrix(c(-1,+1,+1,-1,+1,-1,-1,+1), ncol = 2, nrow = 4, byrow = TRUE)

w14 <- runif(1)-0.5
w15 <- runif(1)-0.5

w24 <- runif(1)-0.5
w25 <- runif(1)-0.5

w34 <- runif(1)-0.5
w35 <- runif(1)-0.5


w47 <- runif(1)-0.5
w48 <- runif(1)-0.5


w57 <- runif(1)-0.5
w58 <- runif(1)-0.5


w67 <- runif(1)-0.5
w68 <- runif(1)-0.5

maxepocas <- 100000
nepocas <- 1

tol <- 0.001 # tolerancia
eepoca <-tol + 1 # erro epoca
evec <- matrix()
eta <- 0.01 # passo

i3 <- +1
i6 <- +1
xproj <- matrix(0, nrow = 4, ncol = 2)
while((nepocas < maxepocas) && (eepoca > tol)){
  ei2 <-0 # erro quadrático acumulado por época
  
  xseq <- sample(N)
  for(i in 1:N){
    irand <- xseq[i]
    i1 <- xtr[irand,1]
    i2 <- xtr[irand,2]
    
    u4 <- i1*w14 + i2*w24 + i3*w34
    i4 <- tanh(u4)
    
    u5 <- i1*w15 + i2*w25 + i3*w35
    i5 <- tanh(u5)
    
    u7 <- i5*w57 + i4*w47 + i6*w67
    i7 <- tanh(u7)
    
    u8 <- i5*w58 + i4*w48 + i6*w68
    i8 <- tanh(u8)
    
    y7 <- ytr[irand, 1]
    y8 <- ytr[irand, 2]
    
    e7 <- y7 - i7
    e8 <- y8 - i8
    
    # Calculo da variação dos pesos da camada de saida
    d7 <- e7*sech2(u7)
    d8 <- e8*sech2(u8)
    
    dw47 = eta*d7*i4
    dw48 = eta*d8*i4
    
    dw57 = eta*d7*i5
    dw58 = eta*d8*i5
    
    dw67 = eta*d7*i6
    dw68 = eta*d8*i6
    
    # Calculo da variaçaõ dos pesos da camada intermediáia
    e4 <- (d7*w47 + d8*w48)
    e5 <- (d7*w57 + d8*w58)
    
    d4 <- e4 * sech2(u4)
    d5 <- e5 * sech2(u5)
    
    dw14 = eta*d4*i1
    dw15 = eta*d5*i1
    
    dw24 = eta*d4*i2
    dw25 = eta*d5*i2
    
    dw34 = eta*d4*i3
    dw35 = eta*d5*i3
    
    # Atualização de todos os pesos
    
    w14 <- w14 + dw14
    w15 <- w15 + dw15
    w24 <- w24 + dw24
    w25 <- w25 + dw25
    w34 <- w34 + dw34
    w35 <- w35 + dw35
    
    w47 <- w47 + dw47
    w48 <- w48 + dw48
    w57 <- w57 + dw57
    w58 <- w58 + dw58
    w67 <- w67 + dw67
    w68 <- w68 + dw68
    
    ei2 <- ei2 + (e7^2 + e8^2)/4 # acrescenta erro normalizado
    xproj[i,1] <- i4
    xproj[i,2] <- i5
  }
  
  nepocas <- nepocas + 1
  evec[nepocas] <- ei2/N # erro po amostra
  
  eepoca <- evec[nepocas]
}

plot((1:nepocas),evec)




yhattr <- matrix(ncol = 2, nrow = 4)
for(i in 1:N){
  i1 <- xtr[i,1]
  i2 <- xtr[i,2]
  
  u4 <- i1*w14 + i2*w24 + i3*w34
  i4 <- tanh(u4)
  
  u5 <- i1*w15 + i2*w25 + i3*w35
  i5 <- tanh(u5)
  
  u7 <- i5*w57 + i4*w47 + i6*w67
  i7 <- tanh(u7)
  
  u8 <- i5*w58 + i4*w48 + i6*w68
  i8 <- tanh(u8)
  
  yhattr[i,1] <- i4
  yhattr[i,2] <- i5
  
}

print(yhattr)






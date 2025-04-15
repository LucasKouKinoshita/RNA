rm(list = ls())
library("mlbench")
library("corrplot")


perceptron <- function(X, y, eta, tol, maxepocas, par) {
  N <- nrow(X) # Número de linhas de X
  n <- ncol(X)
  error_curve <- numeric(maxepocas) # Vetor para armazenar o erro por época
  
  # inicializando pesos
  if(par == 1){ 
    w <- as.matrix(runif(n+1) - 0.5)
    X <- cbind(1,X)
  } else {
    w <- as.matrix(runif(n) - 0.5)
  }
  
  nepocas <- 0
  erroepoca <- tol + 1 # Inicializa erro acima do limite para entrar no loop
  
  while (erroepoca > tol && nepocas < maxepocas) {
    xseq <- sample(N) # Embaralha os índices
    ei2 <- 0 # Inicializa erro da época
    
    for (i in xseq) {
      #print(w)
      #yhat <- 1*(t(w) %*%  X[i, ]) 
      yhat <- 1.0*((t(w) %*% X[i, ]) >= 0)
      print(yhat)
      erro <- y[i] - yhat
      dw <- eta * erro * X[i, ] # Atualização dos pesos
      w <- w + dw
      ei2 <- ei2 + erro^2 # Acumula erro quadrático
    }
    
    error_curve[nepocas] <- ei2 / N # Erro médio quadrático por época
    nepocas <- nepocas + 1
  }
  
  list(weights = w, error = error_curve[1:(nepocas - 1)])
}

xc1 <- matrix(0.3*rnorm(60) + 2, ncol = 2)
xc2 <- matrix(0.3*rnorm(60) + 4, ncol = 2)

plot(xc1[,1], xc1[,2], xlim = c(0,6), ylim = c(0,6), xlab = 'x1', ylab = 'x2', col = 'blue')
par(new=TRUE)
plot(xc2[,1], xc2[,2], xlim = c(0,6), ylim = c(0,6), xlab = 'x1', ylab = 'x2', col = 'red')

seqx1x2 <- seq(0,6, 0.2)
npgrid <- length(seqx1x2)
M <- matrix(nrow = npgrid, ncol = npgrid)
ci <- 0
w <- as.matrix(c(6,1,1))
for(x1 in seqx1x2){
  ci <- ci + 1
  cj <- 0
  for(x2 in seqx1x2){
    cj<- cj + 1
    xin <- as.matrix(cbind(-1, x1,x2))
    M[ci,cj] <- 1*((xin %*% w) >= 0)
  }
}

ribbon3D(seqx1x2, seqx1x2,
         xlab = 'x1', ylab = 'x2',
         xlim=c(0,6), ylim = c(0,6), M, colkey = F)

scatter3D(xc1[,1], xc1[,2],
          xlab = 'x1', ylab = 'x2',
          matrix(0, nrow = dim(xc1)[1]),
          add = T, col = 'blue',
          colkey = F)
scatter3D(xc2[,1], xc2[,2],
          xlab = 'x1', ylab = 'x2',
          matrix(0, nrow = dim(xc1)[1]),
          add = T, col = 'red',
          colkey = F)



xall <- rbind(xc1, xc2)
yall <- rbind(matrix(0, ncol = 1, nrow = nrow(xc1)),
              matrix(1, ncol = 1, nrow = nrow(xc2)))

retlist<- perceptron(xall, yall, 0.1, 0.1, 10000, 1)


final_weights <- retlist$weights
error_curve <- retlist$error

xallplus <- cbind(-1, xall)
yhat <- 1*((xallplus %*% final_weights) > 0)
et <- sum(t(yall - yhat) %*% (yall - yhat))


######################### Dadods reais - iris ##########
data(iris)

xc1 <- as.matrix(iris[1:50, 1:4])
xc2 <- as.matrix(iris[51:500, 1:4])

ntrain <- 30
seqc1 <- sample(50)

plot(iris, col = c("red", "green3", "blue")[unclass(iris$Species)])

xc1treina <- xc1[seqc1[1:ntrain],]
yc1treina <- matrix(0, nrow=ntrain)
seqc2<-sample(50)
xc2treina <- xc2[seqc2[1:ntrain],]
yc2treina <- matrix(1, nrow = ntrain)

xc1teste <- xc1[seqc1[(ntrain + 1): 50],]
yc1teste <- matrix(0, nrow=(50-ntrain))
xc2teste <- xc2[seqc2[(ntrain + 1): 50],]
yc2teste <- matrix(1, nrow = (50-ntrain))


xin <- as.matrix(rbind(xc1treina, xc2treina))
yd <- rbind(yc1treina, yc2treina)
xinteste <- as.matrix(rbind(xc1teste, xc2teste))
yteste <- rbind(yc1teste, yc2teste)


retlist <- perceptron(xin, yd,0.1, 0.01, 1000, 1)
wt <- retlist$weights
error_curve <- retlist$error

# Aproximação
xinteste <- cbind(xinteste, 1)
yt <- xinteste %*% wt

acuracia <- 1 - (t(yteste - yt) %*% (yteste - yt))/20
print(acuracia)



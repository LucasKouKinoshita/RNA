graphics.off()
rm(list = ls())
library("mlbench")
library("corrplot")
library(plot3D)
library(rmarkdown)

perceptron <- function(X, y, eta, tol, maxepocas, par) {
  N <- nrow(X) # Número de linhas de X
  n <- ncol(X)
  error_curve <- numeric(maxepocas) # Vetor para armazenar o erro por época
  
  # inicializando pesos
  if(par == 1){ 
    w <- as.matrix(runif(n+1) - 0.5)
    X <- cbind(X, 1)
  } else {
    w <- as.matrix(runif(n) - 0.5)
  }
  
  nepocas <- 0
  erroepoca <- tol + 1 # Inicializa erro acima do limite para entrar no loop
  
  while (erroepoca > tol && nepocas < maxepocas) {
    xseq <- sample(N) # Embaralha os índices
    ei2 <- 0 # Inicializa erro da época
    
    for (i in xseq) {
      yhat <- 1.0*(X[i, ] %*% w) >= 0
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

s1 <- 0.4
s2 <- 0.4
nc <- 200
xc1 <- matrix(rnorm(nc*2), ncol = 2)*s1 + t(matrix((c(2,2)), ncol = nc, nrow = 2))
xc2 <- matrix(rnorm(nc*2), ncol = 2)*s2 + t(matrix((c(4,4)), ncol = nc, nrow = 2))

plot(xc1[,1], xc1[,2], xlim = c(0,6), ylim = c(0,6), xlab = 'x1', ylab = 'x2', col = 'red')
par(new=TRUE)
plot(xc2[,1], xc2[,2], xlim = c(0,6), ylim = c(0,6), xlab = 'x1', ylab = 'x2', col = 'blue')

ntrain <- nc*0.7

seqc1 <- sample(nc)
xc1treina <- xc1[seqc1[1:ntrain],]
yc1treina <- matrix(0, nrow=ntrain)

seqc2<-sample(nc)
xc2treina <- xc2[seqc2[1:ntrain],]
yc2treina <- matrix(1, nrow = ntrain)

xc1teste <- xc1[seqc1[(ntrain + 1): nc],]
yc1teste <- matrix(0, nrow=(nc-ntrain))
xc2teste <- xc2[seqc2[(ntrain + 1): nc],]
yc2teste <- matrix(1, nrow = (nc-ntrain))

xall <- rbind(xc1treina, xc2treina)
yall <- rbind(matrix(0, ncol = 1, nrow = nrow(xc1treina)),
              matrix(1, ncol = 1, nrow = nrow(xc2treina)))

retlist<- perceptron(xall, yall, 0.1, 0.1, 100, 1)
final_weights <- retlist$weights
error_curve <- retlist$error

xinteste <- rbind(xc1teste, xc2teste)
yteste <- rbind(yc1teste, yc2teste)

xinteste <- cbind(xinteste, 1)
yt <- 1*((xinteste %*% final_weights) >= 0)

acuracia <- 1 - (t(yteste - yt) %*% (yteste - yt))/nrow(yteste)
cat("Acurácia (Teste):", round(acuracia, 4), "\n")

cat("Matriz de Confusão (Teste):\n")
print(table(Predito = yt, Verdadeiro = yteste))





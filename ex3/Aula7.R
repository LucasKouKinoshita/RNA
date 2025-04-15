rm(list = ls())
library("mlbench")
library("corrplot")


adaline <- function(X, y, eta, tol, maxepocas, par) {
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
      yhat <- 1*(t(w) %*%  X[i, ]) 
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


data("Ozone")
Ozone <- data.matrix(na.omit(Ozone))

xall <- scale(data.matrix(Ozone[, -4])) # entradas
yall <- scale(data.matrix(Ozone[,4])) # 4º coluna é a saída

cormat <- cor(Ozone)
corrplot(cormat, method="number", type="upper")

xseq <- sample(203)

xtrain <- as.matrix(xall[xseq[1:150]])
ytrain <- as.matrix(yall[xseq[1:150]])

xtest <- as.matrix(xall[xseq[151:203]])
ytest <- as.matrix(yall[xseq[151:203]])

retlist<-adaline(xtrain, ytrain, 0.001, 0.1, 200, 1)


# Extrair resultados
final_weights <- retlist$weights
error_curve <- retlist$error

# Plotar curva de aprendizado
plot(error_curve, type = "l", col = "blue", lwd = 2, 
     xlab = "Épocas", ylab = "Erro Médio Quadrático", 
     main = "Curva de Aprendizado do Adaline")

# Aproximação
y_pred <- Ht %*% final_weights

# Plotar resultados
plot(X, Y, col = "red", pch = 1, xlab = "X", 
     ylab = "Y", main = "Regressão com Adaline")
lines(xt, yg, col = 'red')
lines(xt, y_pred, col = "blue", lwd = 1)







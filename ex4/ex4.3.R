rm(list = ls())
graphics.off()
library("mlbench")

perceptron <- function(X, y, eta, tol, maxepocas, par) {
  N <- nrow(X)
  n <- ncol(X)
  error_curve <- numeric(maxepocas)
  
  if(par == 1){ 
    w <- as.matrix(runif(n+1) - 0.5)
    X <- cbind(1,X)
  } else {
    w <- as.matrix(runif(n) - 0.5)
  }
  
  nepocas <- 0
  erroepoca <- tol + 1
  
  while (erroepoca > tol && nepocas < maxepocas) {
    xseq <- sample(N)
    ei2 <- 0
    
    for (i in xseq) {
      yhat <- 1.0*((t(w) %*% X[i, ]) >= 0)
      erro <- y[i] - yhat
      dw <- eta * erro * X[i, ]
      w <- w + dw
      ei2 <- ei2 + erro^2
    }
    
    error_curve[nepocas] <- ei2 / N
    nepocas <- nepocas + 1
  }
  
  list(weights = w, error = error_curve[1:(nepocas - 1)])
}

####### Loop 100x #########

data(iris)

ntrain <- 30
reps <- 100
acc_treino <- numeric(reps)
acc_teste <- numeric(reps)

for (r in 1:reps) {
  # Define dados
  xc1 <- as.matrix(iris[1:50, 1:4])        # Setosa
  xc2 <- as.matrix(iris[51:150, 1:4])      # Versicolor + Virginica
  
  seqc1 <- sample(50)
  seqc2 <- sample(100)
  
  # Treino
  xc1treina <- xc1[seqc1[1:ntrain],]
  yc1treina <- matrix(0, nrow=ntrain)
  xc2treina <- xc2[seqc2[1:ntrain],]
  yc2treina <- matrix(1, nrow=ntrain)
  # Teste
  xc1teste <- xc1[seqc1[(ntrain + 1): 50],]
  yc1teste <- matrix(0, nrow=(50 - ntrain))
  xc2teste <- xc2[seqc2[(ntrain + 1): 100],]
  yc2teste <- matrix(1, nrow=(100 - ntrain))
  
  # Conjuntos
  xin <- rbind(xc1treina, xc2treina)
  yd <- rbind(yc1treina, yc2treina)
  
  xinteste <- rbind(xc1teste, xc2teste)
  yteste <- rbind(yc1teste, yc2teste)
  
  # Treinamento
  retlist <- perceptron(xin, yd, eta = 0.1, tol = 0.01, maxepocas = 1000, par = 1)
  wt <- retlist$weights
  
  # Acurácia treino
  xitreino <- cbind(1, xin)
  ytreino_pred <- 1*((xitreino %*% wt) >= 0)
  acc_treino[r] <- 1 - (t(yd - ytreino_pred) %*% (yd - ytreino_pred)) / nrow(yd)
  
  # Acurácia teste
  xiteste <- cbind(1, xinteste)
  yteste_pred <- 1*((xiteste %*% wt) >= 0)
  acc_teste[r] <- 1 - (t(yteste - yteste_pred) %*% (yteste - yteste_pred)) / nrow(yteste)
}

# Resultados
cat("Treinamento:\n")
cat("Média da acurácia:", round(mean(acc_treino), 4), "\n")
cat("Variância da acurácia:", round(var(acc_treino), 6), "\n\n")

cat("Teste:\n")
cat("Média da acurácia:", round(mean(acc_teste), 4), "\n")
cat("Variância da acurácia:", round(var(acc_teste), 6), "\n")

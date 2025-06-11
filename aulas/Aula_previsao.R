rm(list=ls(all=TRUE))
graphics.off()

library('corpcor')
library('mlbench')
data("AirPassengers")
plot(AirPassengers)

diffairpassangers <- diff(AirPassengers)

plot(diffairpassangers)
xcor <- acf(diffairpassangers)

adaline <- function(X, y, eta, tol, maxepocas, par) {
  N <- nrow(X) # Número de linhas de X
  n <- ncol(X)
  error_curve <- numeric(maxepocas) # Vetor para armazenar o erro por época
  
  # Inicializando pesos
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

delay <- as.matrix(c(0,0,0,0,0,0))
delay_12 <- as.matrix(c(0,0,0,0,0,0,0,0,0,0,0,0))
series <- as.matrix(AirPassengers) 
delayed_series <- rbind(delay, series) 
delayed_series_12 <- rbind(delay_12, series) 

size <- 100
total_size <- 144
xs_0_6 <- rbind(delayed_series[1:total_size], series[1:total_size])
xin1 <- xs_0_6[1,(7:size)]

xs_0_12 <- rbind(delayed_series_12[1:total_size], series[1:total_size])
xin2 <- xs_0_12[1,(13:size)]
yin <- xs_0_12[2,(13:size)]

xt <- xs_0_12[1,((size+1):total_size)]
yt <- xs_0_12[2,((size+1):total_size)]
# Preparando as matrizes de entrada
H <- cbind(xin2, 1)
Ht <- cbind(xt, 1)

# Parâmetros do Adaline
eta <- 1e-6
tol <- 1e-2
maxepocas <- 50
adaline_model <- adaline(H, yin, eta, tol, maxepocas, par = 0)

# Extrair resultados
final_weights <- adaline_model$weights
error_curve <- adaline_model$error

y_pred <- Ht %*% final_weights

plot(y_pred, type = 'l', col ='red', ylim = c(100, 650), ylab = '')
par(new=TRUE)
plot(yt, type = 'l', col = 'blue',  ylim = c(100, 650), , ylab = 'predito vs real')


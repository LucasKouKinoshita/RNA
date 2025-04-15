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
      #print(w)
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

# Definição da função alvogx <- function(xin) 0.5 * xin^2 + 3 * xin + 10 

# Gerar dataset
X <- runif(n = 100, min = -18, max = 15)
xt <- seq(-18, 15, 0.1)
Y <- fgx(X) + 10 * rnorm(length(X)) # Adiciona ruído

yg<- fgx(xt) # saída real

# Preparar matriz de entrada
H <- cbind(X^2, X, 1)
Ht <- cbind(xt^2, xt, 1)
# Treinamento do Adaline
eta <- 1e-5
tol <- 1e-2
maxepocas <- 10000
adaline_model <- adaline(H, Y, eta, tol, maxepocas, par = 0)

# Extrair resultados
final_weights <- adaline_model$weights
error_curve <- adaline_model$error

# Plotar curva de aprendizado
plot(error_curve, type = "l", col = "blue", lwd = 2, 
     xlab = "Épocas", ylab = "Erro Médio Quadrático", 
     main = "Curva de Aprendizado do Adaline")

# Fazer previsões
y_pred <- Ht %*% final_weights

# Plotar resultados
plot(X, Y, col = "red", pch = 1, xlab = "X", 
     ylab = "Y", main = "Regressão com Adaline")
lines(xg, yg, col = 'red')
lines(xt, y_pred, col = "blue", lwd = 1)
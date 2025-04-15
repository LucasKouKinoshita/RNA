graphics.off()
rm(list = ls())
library('corpcor')

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

# Leitura dos dados
t <- read.table('dados/Ex1_t', header = FALSE, sep = " ", fill = TRUE)
t <- na.omit(t)
t <- as.matrix(t[,-1])

x <- read.table('dados/Ex1_x', header = FALSE, sep = " ", fill = TRUE)
x <- na.omit(x)
X <- as.matrix(x[,-1])

# Gerando o sinal Xt com amostragem mais densa
t_seq <- seq(min(t), max(t), by = 0.2) # Amostragem mais fina
Xt <- sin((pi/3) * t_seq)  # Sinal seno
Xt <- as.matrix(Xt)  

y <- read.table('dados/Ex1_y', header = FALSE, sep = " ", fill = TRUE)
y <- na.omit(y)
Y <- as.matrix(y[,-1])

# Plotando o dataset original
plot(t, X, col = "red", pch = 1, xlab = "t", 
     ylab = "Y", main = "Dataset")
lines(t, X, col = 'red')
points(t, Y, col = "blue", pch = 16)
lines(t, Y, col = "blue", lwd = 2)

# Preparando as matrizes de entrada
H <- cbind(X, 1)
Ht <- cbind(Xt, 1)

# Parâmetros do Adaline
eta <- 1e-3
tol <- 1e-2
maxepocas <- 10000
adaline_model <- adaline(H, Y, eta, tol, maxepocas, par = 0)

# Extrair resultados
final_weights <- adaline_model$weights
error_curve <- adaline_model$error

# Plotando a curva de aprendizado
plot(error_curve, type = "l", col = "blue", lwd = 2, 
     xlab = "Épocas", ylab = "Erro Médio Quadrático", 
     main = "Curva de Aprendizado do Adaline")

# Fazendo previsões com os dados Xt
y_pred <- Ht %*% final_weights

# Plotando os resultados da previsão no gráfico original
plot(t, X, col = "red", pch = 1, xlab = "t", 
     ylab = "Y", main = "Regressão com Adaline")
lines(t, X, col = 'red')
points(t, Y, col = "blue", pch = 16)
lines(t, Y, col = "blue", lwd = 1)

# Adicionando as previsões
points(t_seq, y_pred, col = "green", pch = 16)
lines(t_seq, y_pred, col = "green", lwd = 2)

# Adicionando legendas
legend("bottomright", 
       legend = c("X (Entrada)", "Y (Saída)", "Previsão Adaline"), 
       col = c("red", "blue", "green"), 
       pch = c(1, 16, 16), lty = c(1, 1, 1), lwd = c(NA, 2, 2))

# Plotando a avaliação com dados de teste (Xt)
plot(t_seq, Xt, col = "purple", pch = 16, xlab = "t", ylab = "Y", main = "Resposta a Novo Sinal")
lines(t_seq, Xt, col = "purple", lwd = 2)
points(t_seq, y_pred, col = "green", pch = 16)
lines(t_seq, y_pred, col = "green", lwd = 2)

legend("bottomright", 
       legend = c("Xt (Entrada)", "Y (Saída)"), 
       col = c("purple", "green"), 
       pch = c(1, 16, 16), lty = c(1, 1, 1), lwd = c(NA, 2, 2))


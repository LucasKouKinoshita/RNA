graphics.off()
rm(list = ls())
library('corpcor')

################ não-linearmente separável
# Gerando os dados
gx <- seq(-1, 1, by = 0.1)
gy <- seq(-1, 1, by = 0.1)
grid <- expand.grid(x = gx, y = gy)

# Função para calcular a distância ao centro (raio)
circle <- function(x, y) {
  return(sqrt(x^2 + y^2))
}

raio <- 0.6

# Definição das classes
classe_labels <- 1 * (circle(grid$x, grid$y) > raio)

grid$classe <- as.factor(classe_labels)  # Evita o erro

# Projeção não linear (transformação quadrática)
grid$phi <- grid$x^2 + grid$y^2

# Visualização
library(ggplot2)
ggplot(grid, aes(x = phi, y = y, color = classe)) +
  geom_point() +
  labs(title = "Projeção não linear para separação linear", 
       x = expression(phi~"= x² + y²"), 
       y = "y")


#############
graphics.off()
rm(list = ls())

set.seed(42) # Para reprodutibilidade

fgx <- function(xin) 0.5*xin^2 + 3*xin + 10 # Função geradora fg(x)

N_train <- 100
X <- runif(n = N_train, min = -15, max = 10) 
#X <- (X/max(X)) - 3 # normalização
Y <- fgx(X) + 4*rnorm(length(X)) 

# Criando matriz de características para ajuste polinomial
p <- 5
H <- cbind(X^p, X, 1)
w <- pseudoinverse(H) %*% Y  # Obtendo os coeficientes

#Criar 20 pontos para avaliar a aproximação
N_eval <- 20
xt <- seq(-15, 10, length.out = N_eval) 
Ht <- cbind(xt^p, xt, 1)
Yhattst <- Ht %*% w  # Valores previstos pelo modelo ajustado

# Função original sem ruído para referência
yg <- fgx(xt)

# Plotagem
plot(X, Y, col = 'red', pch = 16, xlab = "X", ylab = "Y", 
     main = "Ajuste Polinomial", xlim = c(-15, 15), ylim = c(0, 50))

lines(xt, yg, col = 'red', lwd = 2, lty = 2) # Função original
lines(xt, Yhattst, col = 'black', lwd = 2) # Aproximação polinomial
points(xt, Yhattst, col = "black", pch = 16) # Adiciona os 20 pontos de teste

legend("bottomright", 
       legend = c("Amostras com ruído", "Função original", 
                  "Polinômio aproximado", "Amostras de teste"),
       col = c("red", "red", "black", "black"), 
       pch = c(16, NA, NA, 16), lty = c(NA, 2, 1, NA), lwd = c(NA, 2, 2, NA))

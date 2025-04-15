graphics.off()
rm(list = ls())
library('plot3D')


# Definição das variáveis
X <- matrix(c(0.1, 2, -1, 0.2), ncol = 1, nrow = 4)
Y <- matrix(c(2.05, 3, 1.5, 2.1), ncol = 1, nrow = 4)

plot(X, Y, type = 'b')

seqx1x2 <- seq(-6, 8, 0.2)

npgrid <- length(seqx1x2)
M <- matrix(nrow = npgrid, ncol = npgrid)

# Criando superificie de erro
for (i in 1:npgrid) {
  for (j in 1:npgrid) {
    w1 <- seqx1x2[i]
    w0 <- seqx1x2[j]
    M[i, j] <- (0.25) * sum((Y - (w1 * X + w0))^2)
  }
}

# Parâmetros iniciais
w1t <- 6
w0t <- -6
Jt <- (0.25) * sum((Y - (w1t * X + w0t))^2)

# Gerar 10 incrementos aleatórios para w1 e w0
delta_w1 <- runif(10, -6, 6)
delta_w0 <- runif(10, -6, 6)

# Testar cada novo par (w1, w0)
J_new <- numeric(10)
J_new <- numeric(10)
for (i in 1:10) {
  w1_new <- w1t + delta_w1[i]
  w0_new <- w0t + delta_w0[i]
  
  J_new[i] <- (0.25)*((Y[1] - (w1_new*X[1] + w0_new))^2 +
                        (Y[2] - (w1_new*X[2] + w0_new))^2 +
                        (Y[3] - (w1_new*X[3] + w0_new))^2 +
                        (Y[4] - (w1_new*X[4] + w0_new))^2)
}

# Escolher o melhor w1 e w0 que minimizam J
idx_best <- which.min(J_new)
w1_best <- w1t + delta_w1[idx_best]
w0_best <- w0t + delta_w0[idx_best]
J_best <- J_new[idx_best]


contour(seqx1x2, seqx1x2, M, nlevels = 100, xlab = "w1", ylab = "w0", main = "Evolução de J(w1, w0)")

# Ponto inicial
points(w1t, w0t, col = "blue", pch = 19, cex = 1.5)

# Melhor ponto
points(w1_best, w0_best, col = "green", pch = 19, cex = 1.5)

# Linha conectando os pontos
lines(c(w1t, w1_best), c(w0t, w0_best), col = "black", lwd = 2, lty = 2)

persp3D(seqx1x2, seqx1x2, M, xlim=c(-6,8), ylim=c(-6,8), 
        xlab='w1', ylab='w0', colkey = F)

print(J_best)
print(Jt)

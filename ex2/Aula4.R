rm(list = ls())
# Parametros
N<-10 # Numero de amostras
n<-5 # Numero de caracteristicas
m<-5 # Numero de saidas
p<-12 # Numero de neuronios

# phi(X_Nxn * Z_nxp) = psi(H_Nxp * W_pxm) = Yhat_Nxm
X<-matrix(runif(N*n), ncol = n, nrow = N)
Z<-matrix(runif(n*p), ncol = p, nrow = n)
U<-X %*% Z

# Aplicando não linearidade -> em H e O onde O = (H_Nxp * W_pxm)
H<-tanh(U)
W<-matrix(runif(p*m), ncol = m, nrow = p)
O<-H%*%W

yhat<-tanh(O)
Y<-matrix(runif(N*m), ncol=m, nrow = N)

# Erro
E<- Y - yhat
squared_E<- E*E

## Adicionando termo de polarização, da eq da reta onde y = ax + b (b sendo o termo de polarização)
Xaug<-cbind(X, 1)
Zaug<-matrix(runif((n+1)*p), ncol = p, nrow = n+1)
Uaug<-Xaug %*% Zaug

Haug<- tanh(Uaug)
Haugaug <- cbind(Haug, 1)
Waug<- matrix(runif((p+1)*(m)), ncol = m, nrow = p+1)
Oaug<- Haugaug %*% Waug

#yhataug<-tanh(Oaug)
#yhataugaug <- cbind(yhataug, 1)
#Yaug<-matrix(runif((N+1)*m), ncol = m, nrow = N + 1)
#Eaug <- Yaug - yhataug


################################################################# Aproximaçao de polinimomios
rm(list = ls())
library('corpcor')

fgx <- function(xin) 0.5*xin^2+3*xin+10 # função geradora fg(x)

# Gerando gaussiana em Y em torno da funçao fg
X<-runif(n = 100, min = -18, max=15) # Amostra X
#X <- (X/max(X)) - 3 # normalização
xt <- seq(-18,15,0.1)
Y<- fgx(X) + 10*rnorm(length(X)) # ruido

H <- cbind(X^2, X, 1)
Ht <- cbind(xt^2, xt, 1)
w<- pseudoinverse(H) %*% Y

Yhat <- H %*% w
Yhattst <- Ht %*% w
ymax <- max(Yhat)
yg<- fgx(xt)

plot(X, Y, col = 'blue', xlim = c(-18,15), ylim = c(0, ymax))
par(new = TRUE)
plot(X, Yhat, col='red', xlim = c(-18,15), ylim = c(0,ymax))
par(new=TRUE)
plot(xt, Yhattst, col='black', type='l', xlim = c(-18,15), ylim = c(0,ymax))
par(new=TRUE)
plot(xt, yg, col='red', type='l', xlim=c(-18,15), ylim = c(0, ymax))


###############################  Função Superdimensionada

rm(list = ls())

fgx <- function(xin) 0.5*xin^2+3*xin+10 # função geradora fg(x)
# Gerando gaussiana em Y em torno da funçao fg
X<-runif(n = 100, min = -18, max=15) # Amostra X
#X <- (X/max(X)) - 3 # normalização
xt <- seq(-18,15,0.1)
Y<- fgx(X) + 0.2*rnorm(length(X)) # ruido

H <- cbind(X^8, X, 1)
Ht <- cbind(xt^8, xt, 1)
w<- pseudoinverse(H) %*% Y

Yhat <- H %*% w
Yhattst <- Ht %*% w
ymax <- max(Yhat)
yg<- fgx(xt)

plot(X, Y, col = 'blue', xlim = c(-18,15), ylim = c(-10, ymax))
par(new = TRUE)
plot(X, Yhat, col='red', xlim = c(-18,15), ylim = c(0,ymax))
par(new=TRUE)
plot(xt, Yhattst, col='black', type='l', xlim = c(-18,15), ylim = c(0,ymax))
par(new=TRUE)
plot(xt, yg, col='red', type='l', xlim=c(-18,15), ylim = c(0, ymax))




















graphics.off()
rm(list = ls())
library("mlbench")
library("corrplot")
library(plot3D)
library('corpcor')

library(mlbench)

############################ dados reais #####################################
## treina ELM
ELM <- function(xin, yin, p, par) {
  n <- dim(xin)[2]
  
  if(par == 1){
    xin <- cbind(1, xin)
    Z <- matrix(runif((n+1)*p, -0.5, 0.5), nrow = (n+1), ncol = p)
  } else {
    Z <- matrix(runif(n*p, -0.5, 0.5), nrow = n, ncol = p)
  }

  H <- tanh(xin %*% Z)
  Haug <- cbind(1, H)
  
  w <- pseudoinverse(Haug) %*% yin
  
  return( list(w, H, Z))
} 

## saída ELM para valores -1 e +1
YELM <- function(xin, Z, W, par){
  n <- dim(xin)[2]
  
  if (par == 1){
    xin <- cbind(1, xin)
  }
  
  H <- tanh(xin %*% Z)
  Haug <- cbind(1,H)
  Yhat <- sign(Haug %*% W)
  
  return(Yhat)
}

# Load BreastCancer
# tratamento de dados
data("BreastCancer")
df <- BreastCancer
df <- df[, -1]  # drop 'Id'
df <- na.omit(df)
df$Class <- ifelse(df$Class == "benign", 1, -1)

df[, 1:9] <- lapply(df[, 1:9], function(x) as.numeric(as.character(x)))
#df$Class <- ifelse(df$Class == "malignant", 1, -1)

X <- as.matrix(df[, 1:9])
Y <- as.matrix(df$Class)
N <- nrow(X)

ntrain <- nrow(df)*0.7  
reps <- 10
p <- 300
acc_treino <- numeric(reps)
acc_teste <- numeric(reps)

for (r in 1:reps) {
  # Shuffle indices
  idx <- sample(N)
  
  # Train/Test split
  train_idx <- idx[1:ntrain]
  test_idx <- idx[(ntrain + 1):N]
  
  xin <- X[train_idx, ]
  yin <- Y[train_idx]
  xinteste <- X[test_idx, ]
  yteste <- Y[test_idx]
  
  # Train ELM
  retlist <- ELM(xin, yin, p, par = 1)
  w <- retlist[[1]]
  Z <- retlist[[3]]
  
  yhat_train <- YELM(xin, Z, w, 1)
  yhat_test <- YELM(xinteste, Z, w, 1)
  
  # Cálculo da acurácia
  acc_treino[r] <- mean(yhat_train == yin)
  acc_teste[r] <- mean(yhat_test == yteste)
  
}

# Resultados finais
cat("\nAcurácia média (treino):", round(mean(acc_treino), 4), 
    "±", round(sd(acc_treino), 4), "\n")
cat("Acurácia média (teste):", round(mean(acc_teste), 4), 
    "±", round(sd(acc_teste), 4), "\n")


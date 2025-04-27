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
# Load statlog (Heart) 
# tratamento
df <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/statlog/heart/heart.dat")
colnames(df) <- c(
  "age", "sex", "chest_pain", "rest_bp", "chol", "fbs", "rest_ecg",
  "max_hr", "ex_angina", "oldpeak", "slope", "ca", "thal", "target"
)
df <- na.omit(df)
df$target <- ifelse(df$target == 1, 1, 0)

df[, 1:14] <- lapply(df[, 1:14], function(x) as.numeric(as.character(x)))

X <- as.matrix(df[, 1:14])
y <- as.matrix(df$target)
N <- nrow(X)

ntrain <- nrow(df)*0.7  # you can adjust this
reps <- 10
acc_treino <- numeric(reps)
acc_teste <- numeric(reps)

for (r in 1:reps) {
  # Shuffle indices
  idx <- sample(N)
  
  # Train/Test split
  train_idx <- idx[1:ntrain]
  test_idx <- idx[(ntrain + 1):N]
  
  xin <- X[train_idx, ]
  yd <- y[train_idx]
  xinteste <- X[test_idx, ]
  yteste <- y[test_idx]
  
  # Train perceptron
  retlist <- perceptron(xin, yd, eta = 0.001, tol = 0.01, maxepocas = 1000, par = 1)
  wt <- retlist$weights
  
  # Train accuracy
  xitreino <- cbind(1, xin)
  ytreino_pred <- 1 * ((xitreino %*% wt) >= 0)
  acc_treino[r] <- 1 - mean((yd - ytreino_pred)^2)
  
  # Test accuracy
  xiteste <- cbind(1, xinteste)
  yteste_pred <- 1 * ((xiteste %*% wt) >= 0)
  acc_teste[r] <- 1 - mean((yteste - yteste_pred)^2)
  print(r)
}

# Resultados
cat("\nAcurácia média (treino):", round(mean(acc_treino), 4), 
    "±", round(sd(acc_treino), 4), "\n")
cat("Acurácia média (teste):", round(mean(acc_teste), 4), 
    "±", round(sd(acc_teste), 4), "\n")

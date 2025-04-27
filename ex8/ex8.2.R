graphics.off()
rm(list = ls())
library("mlbench")
library(plot3D)
library('corpcor')
source("D:/github/RNA/models/RBF_aleatorio.R")
source("D:/github/RNA/models/RBF_PSO.R")
source("D:/github/RNA/aulas/RBF.R") 

# Statlog (Heart)
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

# Number of centers
p <- 60 # Fewer centers for faster PSO execution
ntrain <- floor(nrow(df) * 0.7)
reps <- 5 # Fewer reps since PSO is computationally intensive

# Store results for all methods
acc_treino_kmeans <- numeric(reps)
acc_teste_kmeans <- numeric(reps)
acc_treino_random <- numeric(reps)
acc_teste_random <- numeric(reps)
acc_treino_pso <- numeric(reps)
acc_teste_pso <- numeric(reps)

# PSO parameters
swarm_size <- 10 # Small swarm for demonstration
max_iter <- 20   # Limited iterations for demonstration

for (r in 1:reps) {
  #cat("\n--- Repetition", r, "of", reps, "---\n")
  
  # Shuffle indices
  idx <- sample(N)
  
  # Train/Test split
  train_idx <- idx[1:ntrain]
  test_idx <- idx[(ntrain + 1):N]
  
  xin <- X[train_idx, ]
  yd <- y[train_idx]
  xinteste <- X[test_idx, ]
  yteste <- y[test_idx]
  
  # Train all three models
  #cat("Training k-means RBF...\n")
  retlist_kmeans <- RBF(xin, yd, p = p, r = 1.0)
  
  #cat("Training random centers RBF...\n")
  retlist_random <- RBF_aleatorio(xin, yd, p = p)
  
  #cat("Training PSO-optimized RBF...\n")
  retlist_pso <- RBF_PSO(xin, yd, p = p, swarm_size = swarm_size, max_iter = max_iter)
  
  # Predictions for all models
  # K-means RBF
  yhat_train_kmeans <- YRBF(xin, retlist_kmeans)
  yhat_test_kmeans <- YRBF(xinteste, retlist_kmeans)
  
  # Random RBF
  yhat_train_random <- YRBF_aleatorio(xin, retlist_random)
  yhat_test_random <- YRBF_aleatorio(xinteste, retlist_random)
  
  # PSO RBF
  yhat_train_pso <- YRBF_PSO(xin, retlist_pso)
  yhat_test_pso <- YRBF_PSO(xinteste, retlist_pso)
  
  # Binarize predictions for classification
  yhat_train_class_kmeans <- ifelse(yhat_train_kmeans >= 0.5, 1, 0)
  yhat_test_class_kmeans <- ifelse(yhat_test_kmeans >= 0.5, 1, 0)
  yhat_train_class_random <- ifelse(yhat_train_random >= 0.5, 1, 0)
  yhat_test_class_random <- ifelse(yhat_test_random >= 0.5, 1, 0)
  yhat_train_class_pso <- ifelse(yhat_train_pso >= 0.5, 1, 0)
  yhat_test_class_pso <- ifelse(yhat_test_pso >= 0.5, 1, 0)
  
  # Calculate accuracies
  acc_treino_kmeans[r] <- mean(yhat_train_class_kmeans == yd)
  acc_teste_kmeans[r] <- mean(yhat_test_class_kmeans == yteste)
  acc_treino_random[r] <- mean(yhat_train_class_random == yd)
  acc_teste_random[r] <- mean(yhat_test_class_random == yteste)
  acc_treino_pso[r] <- mean(yhat_train_class_pso == yd)
  acc_teste_pso[r] <- mean(yhat_test_class_pso == yteste)
  
  # Print current iteration results
  #cat("K-means Test Accuracy:", round(acc_teste_kmeans[r], 4), "\n")
  #cat("Random Test Accuracy:", round(acc_teste_random[r], 4), "\n")
  #cat("PSO Test Accuracy:", round(acc_teste_pso[r], 4), "\n")
}

# Final results
cat("\n=========== FINAL RESULTS ===========\n")
cat("\n====== K-means RBF ======\n")
cat("Acurácia média (treino):", round(mean(acc_treino_kmeans), 4), 
    "±", round(sd(acc_treino_kmeans), 4), "\n")
cat("Acurácia média (teste):", round(mean(acc_teste_kmeans), 4), 
    "±", round(sd(acc_teste_kmeans), 4), "\n")

cat("\n====== RBF aleatorizado ======\n")
cat("Acurácia média (treino):", round(mean(acc_treino_random), 4), 
    "±", round(sd(acc_treino_random), 4), "\n")
cat("Acurácia média (teste):", round(mean(acc_teste_random), 4), 
    "±", round(sd(acc_teste_random), 4), "\n")

cat("\n====== PSO RBF ======\n")
cat("Acurácia média (treino):", round(mean(acc_treino_pso), 4), 
    "±", round(sd(acc_treino_pso), 4), "\n")
cat("Acurácia média (teste):", round(mean(acc_teste_pso), 4), 
    "±", round(sd(acc_teste_pso), 4), "\n")

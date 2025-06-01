graphics.off() 
rm(list = ls()) 

library(RSNNS)
library(dplyr) 
library(Metrics) 

N_EXECUTIONS <- 5      
TRAIN_RATIO <- 0.7  
HIDDEN_NEURONS <- 10
MAX_ITERATIONS <- 1000 

cat("Iniciando Análise: Classificação com Statlog (Heart) Dataset\n")
cat("--------------------------------------------------------------------\n")

cat("Carregando e pré-processando os dados...\n")
df <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/statlog/heart/heart.dat", sep = " ")
colnames(df) <- c("age", "sex", "chest_pain", "rest_bp", "chol", "fbs", "rest_ecg",
                  "max_hr", "ex_angina", "oldpeak", "slope", "ca", "thal", "target")

df[df == '?'] <- NA
df <- na.omit(df)

feature_cols <- setdiff(colnames(df), "target")
df[, feature_cols] <- lapply(df[, feature_cols], function(x) as.numeric(as.character(x)))

df$target <- ifelse(df$target == 1, -1, 1) 
df <- na.omit(df)

X <- as.matrix(df[, feature_cols])
Y <- as.matrix(df$target) # Y é uma matriz de uma coluna com -1 ou 1

# --- Inicialização de Vetores para Armazenar Resultados de Acurácia ---
all_accuracy_test <- numeric(N_EXECUTIONS)

cat("Iniciando Loop Experimental para Classificação...\n")
cat("----------------------------------------\n")

# --- Loop de Execuções ---
for (exec_num in 1:N_EXECUTIONS) {
  cat(paste("\n--- Execução:", exec_num, "de", N_EXECUTIONS, "---\n"))
  
  cat("Dividindo dados em conjuntos de treinamento e teste...\n")
  n_obs <- nrow(X)
  train_indices <- sample(1:n_obs, size = floor(TRAIN_RATIO * n_obs))
  
  x_train <- X[train_indices, ]
  y_train <- Y[train_indices, , drop = FALSE] # Mantém como matriz
  
  x_test <- X[-train_indices, ]
  y_test <- Y[-train_indices, , drop = FALSE] # Mantém como matriz
  
  cat("Treinando a rede MLP para classificação...\n")
  rede <- mlp(x_train, y_train, 
              size = HIDDEN_NEURONS,
              maxit = MAX_ITERATIONS,
              initFunc = "Randomize_Weights",
              initFuncParams = c(-0.3, 0.3), 
              learnFunc = "Rprop",
              learnFuncParams = c(0.1, 0.1),
              updateFunc = "Topological_Order",
              updateFuncParams = c(0), 
              hiddenActFunc = "Act_Bipas", 
              outputActFunc = "Act_TanH",     # Função de ativação da camada de SAÍDA
              shufflePatterns = TRUE, 
              inputsTest = x_test, 
              targetsTest = y_test) 
  
  cat("Rede treinada. Realizando previsões no conjunto de teste...\n")
  
  predicted_outputs_test <- predict(rede, x_test) 
  predicted_classes_test <- ifelse(predicted_outputs_test > 0, 1, -1)
  
  correct_predictions <- sum(predicted_classes_test == y_test)
  current_accuracy <- correct_predictions / length(y_test)
  
  all_accuracy_test[exec_num] <- current_accuracy
  
  cat(paste("Execução", exec_num, "- Acurácia (teste):", round(current_accuracy, 4), "\n"))
  
  if (exec_num == N_EXECUTIONS) {
    plotIterativeError(rede, main = paste("Erro Iterativo da MLP (Última Execução -", MAX_ITERATIONS, "iterações)"))
  }
}

cat("----------------------------------------\n")
cat("Loop Experimental Finalizado\n\n")

mean_accuracy_test <- mean(all_accuracy_test)
sd_accuracy_test <- sd(all_accuracy_test)

cat("Resultados Finais sobre", N_EXECUTIONS, "execuções (média ± desvio padrão):\n")
cat(paste("Acurácia (teste): ", round(mean_accuracy_test, 4), " ± ", round(sd_accuracy_test, 4), "\n\n"))

cat("----------------------------------------\n")
cat("Script de Classificação Finalizado\n")
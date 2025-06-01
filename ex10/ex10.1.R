graphics.off() 
rm(list = ls()) 

library(RSNNS)
library(dplyr)
library(MASS)      
library(Metrics)

# Função de normalização
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# Função de desnormalização
unnormalize <- function(x_scaled, original_min, original_max) {
  return (x_scaled * (original_max - original_min) + original_min)
}

# --- Configurações Globais do Experimento ---
N_EXECUTIONS <- 5
TRAIN_RATIO <- 0.7  
HIDDEN_NEURONS <- 5
MAX_ITERATIONS <- 2000 

cat("Iniciando a Parte 1: Dataset Boston Housing com Múltiplas Execuções\n")
cat("--------------------------------------------------------------------\n")

data(Boston) # Carrega do pacote MASS
boston_df <- Boston

# Normaliza o dataframe inteiro
boston_scaled_df <- as.data.frame(lapply(boston_df, normalize))

# Salva os valores mínimo e máximo da variável 'medv' original para desnormalização posterior
min_medv_original_global <- min(boston_df$medv)
max_medv_original_global <- max(boston_df$medv)

# --- Inicialização de Vetores para Armazenar Resultados de RMSE ---
all_rmse_scaled_test <- numeric(N_EXECUTIONS)
all_rmse_original_test <- numeric(N_EXECUTIONS)

cat("Iniciando Loop Experimental...\n")
cat("----------------------------------------\n")

# --- Loop de Execuções ---
for (exec_num in 1:N_EXECUTIONS) {
  cat(paste("\n--- Execução:", exec_num, "de", N_EXECUTIONS, "---\n"))
  
  cat("Dividindo dados em conjuntos de treinamento e teste...\n")
  n_obs <- nrow(boston_scaled_df)
  train_indices <- sample(1:n_obs, size = floor(TRAIN_RATIO * n_obs))
  
  x_train <- boston_scaled_df[train_indices, 1:13] 
  y_train_scaled <- boston_scaled_df$medv[train_indices]   
  
  x_test <- boston_scaled_df[-train_indices, 1:13]
  y_test_scaled <- boston_scaled_df$medv[-train_indices]  
  
  # Valores originais (não escalonados) do alvo para o conjunto de teste (para avaliação final)
  y_test_original <- boston_df$medv[-train_indices]
  
  cat("Treinando a rede MLP...\n")
  rede <- mlp(x_train, y_train_scaled, 
              size = HIDDEN_NEURONS,
              maxit = MAX_ITERATIONS,
              initFunc = "Randomize_Weights",
              initFuncParams = c(-0.3, 0.3), 
              learnFunc = "Rprop",
              learnFuncParams = c(0.1, 0.1), # Parâmetros como no script original
              updateFunc = "Topological_Order",
              updateFuncParams = c(0), 
              hiddenActFunc = "Act_Bipas",
              shufflePatterns = TRUE, 
              linOut = TRUE, # Saída linear, apropriado para regressão
              inputsTest = x_test, # Fornece dados de teste para monitorar erro durante o treino
              targetsTest = y_test_scaled) 
  
  cat("Rede treinada. Realizando previsões no conjunto de teste...\n")
  
  predicted_values_test_scaled <- predict(rede, x_test)
  predicted_values_test_original <- unnormalize(predicted_values_test_scaled, 
                                                min_medv_original_global, 
                                                max_medv_original_global)
  
  current_rmse_scaled <- rmse(y_test_scaled, predicted_values_test_scaled)
  current_rmse_original <- rmse(y_test_original, predicted_values_test_original)
  
  all_rmse_scaled_test[exec_num] <- current_rmse_scaled
  all_rmse_original_test[exec_num] <- current_rmse_original
  
  cat(paste("Execução", exec_num, "- RMSE (teste, escalado):", round(current_rmse_scaled, 4), "\n"))
  cat(paste("Execução", exec_num, "- RMSE (teste, original):", round(current_rmse_original, 4), "\n"))
  
  if (exec_num == N_EXECUTIONS) {
    plotIterativeError(rede, main = paste("Erro Iterativo da MLP (Última Execução -", MAX_ITERATIONS, "iterações)"))
  }
}

cat("----------------------------------------\n")
cat("Loop Experimental Finalizado\n\n")

# --- Resultados ---
mean_rmse_scaled_test <- mean(all_rmse_scaled_test)
sd_rmse_scaled_test <- sd(all_rmse_scaled_test)
mean_rmse_original_test <- mean(all_rmse_original_test)
sd_rmse_original_test <- sd(all_rmse_original_test)

cat("Resultados Finais sobre", N_EXECUTIONS, "execuções (média ± desvio padrão):\n")
cat(paste("RMSE (teste, escalado): ", round(mean_rmse_scaled_test, 4), " ± ", round(sd_rmse_scaled_test, 4), "\n"))
cat(paste("RMSE (teste, original): ", round(mean_rmse_original_test, 4), " ± ", round(sd_rmse_original_test, 4), "\n\n"))

# --- Plots de diagnóstico da última execução ---
# Plot dos valores reais vs. previstos (escalados) para a última execução
plot(y_test_scaled, type = 'l', col = 'blue', lwd = 2, # 'l' para linha, lwd para espessura
     ylim = range(c(y_test_scaled, predicted_values_test_scaled)), 
     main = "Última Execução: 'medv' Escalado (Teste) - Real vs. Previsto (Linhas)", 
     ylab = "medv (escalado)", xlab = "Índice da Observação no Conjunto de Teste")
lines(predicted_values_test_scaled, type = 'l', col = 'red', lwd = 2)
legend("topright", legend = c("Real", "Previsto"), col = c("blue", "red"), lty = 1, lwd = 2) # lty=1 para linha sólida


# Plot dos valores reais vs. previstos (na escala original) para a última execução
plot(y_test_original, predicted_values_test_original, 
     main = "Última Execução: 'medv' Original (Teste) - Real vs. Previsto",
     xlab = "Valor Real de medv", ylab = "Valor Previsto de medv",
     pch = 16, col = rgb(0,0,1,0.5)) # Pontos azuis semi-transparentes
abline(a = 0, b = 1, col = "red", lwd = 2) # Linha de referência y=x (ideal)
legend("bottomright", legend = c("Previsões", "Linha Ideal (Real=Previsto)"), 
       pch=c(16,NA), lty=c(NA,1), col = c(rgb(0,0,1,0.5), "red"))


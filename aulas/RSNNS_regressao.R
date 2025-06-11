graphics.off() 
rm(list = ls()) 

library(RSNNS)
library(dplyr)
library(MASS)      
library(Metrics)

x_range <- c(0, 2 * pi)
N_train <- 45
noise_range <- 0.4 # Noise uniformly sampled in [-0.1, 0.1]
xtr <- runif(N_train, min = x_range[1], max = x_range[2])
ytr <- sin(xtr) + runif(N_train, min = -noise_range, max = noise_range)

xtst <- seq(0, 2 * pi, 0.01)
ytst <- sin(xtst)

plot(x = xtr, y = ytr, col = 'blue', pch = 1, xlab = "x", ylab = "y",
     main = "Training and Test Data", xaxt = "n",
     xlim = x_range, ylim = c(-1.5, 1.5))
axis(side = 1, at = seq(0, 2*pi, pi/2),
     labels = c("0", "pi/2", "pi", "3pi/2", "2pi"))
lines(x = xtst, y = ytst, type = 'line', col = 'red')


N_EXECUTIONS <- 50
HIDDEN_NEURONS <- 100 # overfitted 
MAX_ITERATIONS <- 2000 

rede <- mlp(xtr, (ytr+1)/2, 
            size = HIDDEN_NEURONS,
            maxit = MAX_ITERATIONS,
            initFunc = "Randomize_Weights",
            initFuncParams = c(-0.3, 0.3), 
            learnFunc = "Rprop",
            learnFuncParams = c(0.1, 0.1),
            updateFunc = "Topological_Order",
            updateFuncParams = c(0), 
            hiddenActFunc = "Act_Bipas",
            shufflePatterns = TRUE, 
            linOut = FALSE) 

yhat <- (2*predict(rede, as.matrix(xtst))-1)

plot(xtst, yhat, type = 'l', col = 'red', xlab = "x", ylab = "y", 
     xlim = x_range, ylim = c(-1.5, 1.5))
par(new = TRUE)
plot(x = xtr, y = ytr, col = 'blue', pch = 1, xlab = "x", ylab = "y",
                   main = "Training and Test Data", xaxt = "n",
                   xlim = x_range, ylim = c(-1.5, 1.5))
    axis(side = 1, at = seq(0, 2*pi, pi/2),
         labels = c("0", "pi/2", "pi", "3pi/2", "2pi"))
    lines(x = xtst, y = ytst, type = 'line', col = 'blue')
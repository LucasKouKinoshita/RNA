graphics.off() 
rm(list = ls()) 

library(RSNNS)
library(dplyr)
library(MASS)      
library(Metrics)
library(mlbench)

data("LetterRecognition")
xy <- data.matrix(LetterRecognition)
xall <- xy[,(2:17)]
yall <- decodeClassLabels(xy[,1])
xtr <- xall[(1:1600),]
ytr <- yall[(1:1600),]
xtst <- xall[(1601: 20000),]
ytst <- yall[(1601: 20000),]

HIDDEN_NEURONS <- 200
MAX_ITERATIONS <- 500

rede <- mlp(xtr, (ytr+1)/2, 
            size = HIDDEN_NEURONS,
            maxit = MAX_ITERATIONS,
            initFunc = "Randomize_Weights",
            initFuncParams = c(-0.3, 0.3), 
            learnFunc = "Rprop",
            learnFuncParams = c(0.1, 0.1),
            updateFunc = "Topological_Order",
            updateFuncParams = c(0), 
            hiddenActFunc = "Act_logistic",
            shufflePatterns = TRUE, 
            linOut = FALSE) 

yhat_tr <- predict(rede, as.matrix(xtr))
y_tr_dec <- apply(X = ytr, FUN = which.max, 1)
yhat_tr_dec <- apply(X = yhat_tr, FUN = which.max, 1)

yhat_tst <- predict(rede, as.matrix(xtst))
yhat_tst_dec <- apply(X = yhat_tst, FUN = which.max, 1)
y_tst_dec <- apply(X = ytst, FUN = which.max, 1)


acc_tr <- sum(1*(y_tr_dec == yhat_tr_dec))/length(y_tr_dec)
cat(paste("Acurácia de treinamento: ", acc_tr))

acc_tst <- sum(1*(y_tst_dec == yhat_tst_dec))/length(y_tst_dec)
cat(paste("Acurácia de teste: ", acc_tst))








                                      
                   
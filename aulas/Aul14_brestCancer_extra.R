########################### Breast Cancer
graphics.off()
rm(list = ls())
library("mlbench")
library(RSNNS)
library(plot3D)
library('corpcor')

# Geneate H from RBF() 
# Reg H with lambda
# Compare train and test results

# Load BreastCancer
data("BreastCancer")
df <- BreastCancer
df <- df[, -1]  # drop 'Id'
df <- na.omit(df)

# Convert factor columns to numeric (except 'Class')
df[, 1:9] <- lapply(df[, 1:9], function(x) as.numeric(as.character(x)))
df$Class <- ifelse(df$Class == "malignant", 1, -1)

# Matrix version
X <- as.matrix(df[, 1:9])
Y <- as.matrix(df$Class)
N <- nrow(X)

# Number of centers
p <- 60 # Fewer centers for faster PSO execution

Z <- matrix(runif((ncol(X)+1)*p, -0.5, 0.5), nrow = (ncol(X)+1), ncol = p)

Xaug <- cbind(1,X)
H <- tanh(Xaug %*% Z)

L <- 0.1 # lambda -> coenficiente de regularização
Haug <- cbind(1, H)
w <- pseudoinverse(Haug) %*% Y
w <- solve(t(Haug) %*% Haug + L * diag(p+1)) %*% t(Haug) %*% Y

Yhat_train <- sign(Haug %*% w)
e_train <- sum((Y-Yhat_train)^2)/(4*120)
print(e_train)




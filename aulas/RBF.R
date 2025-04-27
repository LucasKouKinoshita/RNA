
kmeans <- function(xin, k, maxit){
  N <- dim(xin)[1]
  n <- dim(xin)[2]
  iseq <- sample(N)
  ci <- as.matrix(xin[iseq[1:k], ])
  c_mais_prox <- matrix(nrow = 1, ncol = N)
  
  for(j in 1:maxit) {
    for(i in 1:N){
      xaug_mat <- matrix(xin[i,], nrow = k, ncol = n, byrow = TRUE)
      dxaug_ci <- (ci - xaug_mat)^2
      di_vec <- rowSums(dxaug_ci)
      c_mais_prox[i] <- which.min(di_vec)
    }
    
    for(l in 1:k){
      ickvet <- which(c_mais_prox == l)
      if (length(ickvet) > 0) {
        ci[l,] <- colMeans(matrix(xin[ickvet, ], ncol = n))
      }
    }
  }
  
  return(list(center = ci, cluster = c_mais_prox))
}

pdfnvar <- function(x, m, K) {
  n <- length(x)
  coef <- 1 / sqrt((2*pi)^n * det(K))
  expoente <- exp(-0.5 * t(x-m) %*% solve(K) %*% (x-m))
  return(coef * expoente)
}

RBF <- function(xin, yin, p, r){
  radialnvar <- function(x, m, invK) exp(-0.5 * (t(x-m) %*% invK %*% (x-m)))
  
  N <- dim(xin)[1]
  n <- dim(xin)[2]
  
  xin <- as.matrix(xin)
  yin <- as.matrix(yin)
  
  xclust <- kmeans(xin, p, 10)
  
  m <- xclust$center
  covi <- r * diag(n)
  inv_covi <- (1/r) * diag(n)
  
  H <- matrix(nrow = N, ncol = p)
  for(j in 1:N){
    for(i in 1:p){
      mi <- m[i,]
      H[j,i] <- radialnvar(xin[j,], mi, inv_covi)
    }
  }
  
  Haug <- cbind(1, H)
  W <- pseudoinverse(Haug) %*% yin
  
  return(list(m, covi, r, W))
}

YRBF <- function(xin, modRBF){
  radialnvar <- function(x, m, invK) exp(-0.5 * (t(x-m) %*% invK %*% (x-m)))
  
  N <- dim(xin)[1]
  n <- dim(xin)[2]
  
  m <- modRBF[[1]]
  covi <- modRBF[[2]]
  inv_covi <- (1/modRBF[[3]]) * diag(n)
  W <- modRBF[[4]]
  p <- dim(m)[1]
  
  xin <- as.matrix(xin)
  H <- matrix(nrow = N, ncol = p)
  
  for(j in 1:N){
    for(i in 1:p){
      mi <- m[i,]
      H[j,i] <- radialnvar(xin[j,], mi, inv_covi)
    }
  }
  
  Haug <- cbind(1, H)
  Yhat <- Haug %*% W
  
  return(Yhat)
}


kmeans <- function(xin, k, maxit){
  
  N <- dim(xin) [1]
  n <- dim(xin) [2]
  
  iseq <- sample(N)
  ci <- as.matrix(xin[(iseq[1:k]),])
  
  c_mais_prox <- matrix(nrow = 1, ncol = N)
  for(j in 1:maxit) {
    for(i in 1:N){
      xaug_mat <- matrix(xin[i,], nrow = k, ncol = n, byrow = 1) # calculo das distancias
      
      dxaug_ci <- (ci-xaug_mat)*(ci-xaug_mat)
      di_vec<-rowSums(dxaug_ci) # vetor de distancias 
      c_mais_prox[i] <- which.min(di_vec) # menor distancia
      
    }
    for(l in 1:k){
      ickvet <- which(c_mais_prox == l) # vetor de indices para cada centro - > pega os indices das amostras do grupo "l"
      ci[l,] <- colMeans(xin[ickvet,])  # media de distancias ao centro l
    } 
  }
  
  
  return(list(ci, c_mais_prox))
}

pdfnvar <- function(x, m, K, n) ((1/sqrt*((2*pi)^n*(det(K))))) * 
  exp*(-0.5*(t(x-m) %*% (solve(K) %*% (x-m))))

RBF <- function(xin, yin, p, r){
  radialnvar <- function(x, m, invK) (ep(-0,5*(t(x-m)%*%(invK)%*%(x-m))))
  
  N <- dim(xin) [1]
  n <- dim(xin) [2]
  
  xin <- as.matrix(xin)
  yin <- as.matrix(yin)
  
  xclust <- kmeans(xin, p)
  
  m <- as.matrix(xclust$center)
  covi <- r*diag(n)
  inv_covi <- (1/r) * diag(n)
  
  H <- matrix(nrow = N, ncol= p)
  for(i in 1:N){
    for(i in 1:p) {
      mi <- m[i,]
      H[j,i] <- pdfnvar(xin[j,], mi, inv_covi )
    }
  }
  
  Haug <- cbind(1,H)
  W <- pseudoinverse(Haug) %*% yin
  return(list(m, covi, r, W, H))
}

YRBF <- function(xin, modRBF) {
  N <- dim(xin) [1]
  n <- dim(xin) [2]
}

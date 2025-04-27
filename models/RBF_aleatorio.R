# RBF implementation with centers randomly set between pairs of training points
# and radii equal to the distance between those points

RBF_aleatorio <- function(xin, yin, p) {
  radialnvar <- function(x, m, sigma) exp(-sum((x-m)^2) / (2*sigma^2))
  
  N <- dim(xin)[1]
  n <- dim(xin)[2]
  
  xin <- as.matrix(xin)
  yin <- as.matrix(yin)
  
  centers <- matrix(0, nrow = p, ncol = n)
  radii <- numeric(p)
  
  for(i in 1:p) {
    idx_pair <- sample(N, 2)
    point1 <- xin[idx_pair[1], ]
    point2 <- xin[idx_pair[2], ]
    
    centers[i, ] <- (point1 + point2) / 2
    
    radii[i] <- sqrt(sum((point1 - point2)^2))
    
    if(radii[i] < 1e-5) {
      radii[i] <- mean(radii[max(1, i-1)])  
      if(is.na(radii[i])) radii[i] <- 1.0  
    }
  }
  
  H <- matrix(nrow = N, ncol = p)
  for(j in 1:N) {
    for(i in 1:p) {
      H[j, i] <- radialnvar(xin[j, ], centers[i, ], radii[i])
    }
  }
  
  Haug <- cbind(1, H)
  W <- pseudoinverse(Haug) %*% yin
  
  return(list(centers = centers, radii = radii, W = W))
}

# Function to make predictions with the random RBF model
YRBF_aleatorio <- function(xin, modRBF) {
  radialnvar <- function(x, m, sigma) exp(-sum((x-m)^2) / (2*sigma^2))
  
  N <- dim(xin)[1]
  
  centers <- modRBF$centers
  radii <- modRBF$radii
  W <- modRBF$W
  p <- nrow(centers)
  
  xin <- as.matrix(xin)
  H <- matrix(nrow = N, ncol = p)
  
  for(j in 1:N) {
    for(i in 1:p) {
      H[j, i] <- radialnvar(xin[j, ], centers[i, ], radii[i])
    }
  }
  
  # Add bias term
  Haug <- cbind(1, H)
  
  # Calculate predictions
  Yhat <- Haug %*% W
  
  return(Yhat)
}
# RBF implementation with centers optimized using Particle Swarm Optimization (PSO)

# PSO function to optimize RBF centers
optimize_centers_PSO <- function(xin, yin, p, swarm_size = 20, max_iter = 50, c1 = 1.5, c2 = 1.5, w = 0.7) {
  N <- dim(xin)[1]
  n <- dim(xin)[2]
  
  # Initialize particles (each particle represents a full set of centers)
  # Each particle has p centers, each center has n dimensions
  particles <- array(0, dim = c(swarm_size, p, n))
  velocities <- array(0, dim = c(swarm_size, p, n))
  
  # Initialize particles with random positions within the bounds of the data
  min_vals <- apply(xin, 2, min)
  max_vals <- apply(xin, 2, max)
  
  for (i in 1:swarm_size) {
    for (j in 1:p) {
      for (k in 1:n) {
        particles[i, j, k] <- runif(1, min_vals[k], max_vals[k])
        velocities[i, j, k] <- runif(1, -abs(max_vals[k] - min_vals[k])/10, 
                                     abs(max_vals[k] - min_vals[k])/10)
      }
    }
  }
  
  particle_best_pos <- particles
  particle_best_fitness <- rep(Inf, swarm_size)
  global_best_pos <- particles[1, , ]
  global_best_fitness <- Inf
  # Define fitness function (average distance of samples to nearest center)
  calc_fitness <- function(centers) {
    distances <- matrix(0, nrow = N, ncol = p)
    
    for (i in 1:N) {
      for (j in 1:p) {
        distances[i, j] <- sqrt(sum((xin[i, ] - centers[j, ])^2))
      }
    }
    # For each sample, find distance to nearest center
    min_distances <- apply(distances, 1, min)
    return(mean(min_distances))
  }
  
  # Main PSO loop
  for (iter in 1:max_iter) {
    # Evaluate fitness for each particle
    for (i in 1:swarm_size) {
      current_fitness <- calc_fitness(particles[i, , ])
      
      # Update particle's best position if needed
      if (current_fitness < particle_best_fitness[i]) {
        particle_best_pos[i, , ] <- particles[i, , ]
        particle_best_fitness[i] <- current_fitness
        
        # Update global best if needed
        if (current_fitness < global_best_fitness) {
          global_best_pos <- particles[i, , ]
          global_best_fitness <- current_fitness
        }
      }
    }
    
    # Update velocities and positions
    for (i in 1:swarm_size) {
      for (j in 1:p) {
        for (k in 1:n) {
          # Cognitive component
          r1 <- runif(1)
          cognitive <- c1 * r1 * (particle_best_pos[i, j, k] - particles[i, j, k])
          
          # Social component
          r2 <- runif(1)
          social <- c2 * r2 * (global_best_pos[j, k] - particles[i, j, k])
          
          # Update velocity with inertia
          velocities[i, j, k] <- w * velocities[i, j, k] + cognitive + social
          
          # Update position
          particles[i, j, k] <- particles[i, j, k] + velocities[i, j, k]
          
          # Keep within bounds
          particles[i, j, k] <- max(min_vals[k], min(particles[i, j, k], max_vals[k]))
        }
      }
    }
    
    # Optional: print progress
    if (iter %% 10 == 0) {
      #cat("Iteration", iter, "- Best fitness:", global_best_fitness, "\n")
    }
  }
  
  # Return optimized centers
  return(global_best_pos)
}

# Calculate an appropriate radius for each center
calc_optimal_radii <- function(centers, xin) {
  p <- nrow(centers)
  n <- ncol(centers)
  N <- nrow(xin)
  
  # Calculate average distance to nearest neighbor center for each center
  radii <- numeric(p)
  
  for (i in 1:p) {
    dist_to_other_centers <- numeric(p-1)
    idx <- 1
    for (j in 1:p) {
      if (i != j) {
        dist_to_other_centers[idx] <- sqrt(sum((centers[i, ] - centers[j, ])^2))
        idx <- idx + 1
      }
    }
    # Set radius as a fraction of average distance to other centers
    radii[i] <- mean(dist_to_other_centers) / 2
  }
  
  return(radii)
}

# Main RBF function with PSO optimization
RBF_PSO <- function(xin, yin, p, swarm_size = 20, max_iter = 50) {
  radialnvar <- function(x, m, sigma) exp(-sum((x-m)^2) / (2*sigma^2))
  
  N <- dim(xin)[1]
  n <- dim(xin)[2]
  
  xin <- as.matrix(xin)
  yin <- as.matrix(yin)
  
  # Optimize centers using PSO
  #cat("Optimizing RBF centers with PSO...\n")
  centers <- optimize_centers_PSO(xin, yin, p, swarm_size, max_iter)
  
  # Calculate appropriate radii for the centers
  radii <- calc_optimal_radii(centers, xin)
  
  # Create activation matrix H
  H <- matrix(nrow = N, ncol = p)
  for(j in 1:N) {
    for(i in 1:p) {
      H[j, i] <- radialnvar(xin[j, ], centers[i, ], radii[i])
    }
  }
  
  # Add bias term
  Haug <- cbind(1, H)
  
  # Calculate weights using pseudoinverse
  W <- pseudoinverse(Haug) %*% yin
  
  # Return model parameters
  return(list(centers = centers, radii = radii, W = W))
}

# Function to make predictions with the PSO-optimized RBF model
YRBF_PSO <- function(xin, modRBF) {
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
#Define a function to calculate RMSE
RMSE <- function(rating, est_rating){
  sqr_err <- function(obs){
    sqr_error <- (obs[3] - est_rating[as.character(obs[2]), as.character(obs[1])])^2
    return(sqr_error)
  }
  return(sqrt(mean(apply(rating, 1, sqr_err))))  
}

#Stochastic Gradient Descent
# a function returns a list containing factorized matrices p and q, training and testing RMSEs.
gradesc <- function(f = 10, 
                    lambda = 0.3,lrate = 0.01, max.iter, stopping.deriv = 0.01,
                    data, train, test){
  set.seed(0)
  #random assign value to matrix p and q
  num_bins <- max(data$bin_label)
  p <- matrix(runif(f*U, -1, 1), ncol = U) 
  colnames(p) <- as.character(1:U)
  
  q <- matrix(runif(f*I, -1, 1), ncol = I)
  colnames(q) <- levels(as.factor(data$movieId))
  
  mu <- runif(1,-1,1)
  
  b_user <- matrix(runif(U,-1,1),ncol=1)
  rownames(b_user) <- levels(as.factor(data$userId))
  
  b_movie <- matrix(runif(I,-1,1),ncol=1)
  rownames(b_movie) <- levels(as.factor(data$movieId))
  
  b_bin <- matrix(runif(I*num_bins,-1,1),ncol=I) 
  colnames(b_bin) <- levels(as.factor(data$movieId))
  
  train_RMSE <- c()
  test_RMSE <- c()
  
  for(l in 1:max.iter){
    sample_idx <- sample(1:nrow(train), nrow(train))
    #loop through each training case and perform update
    for (s in sample_idx){
      
      u <- as.character(train[s,1])
      
      i <- as.character(train[s,2])
      
      t <- train[s,5]
      
      r_ui <- train[s,3]
      r_uit_hat <- mu + b_user[u,1] + b_movie[i,1] + b_bin[t,i] + t(q[,i]) %*% p[,u]
      
      e_ui <- r_ui - r_uit_hat
      
      grad_q <- e_ui %*% p[,u] - lambda * q[,i]
      
      if (all(abs(grad_q) > stopping.deriv, na.rm = T)){
        q[,i] <- q[,i] + lrate * grad_q
      }
      
      grad_p <- e_ui %*% q[,i] - lambda * p[,u]
      
      if (all(abs(grad_p) > stopping.deriv, na.rm = T)){
        p[,u] <- p[,u] + lrate * grad_p
      }
      
      grad_mu <- e_ui - lambda * mu
      
      if (all(abs(grad_mu) > stopping.deriv, na.rm = T)){
        mu <- mu + lrate * grad_mu
      }
      
      grad_b_user <- e_ui - lambda * b_user[u,1]
      
      if (all(abs(grad_b_user) > stopping.deriv, na.rm = T)){
        b_user[u,1] <- b_user[u,1] + lrate * grad_b_user
      }
      
      grad_b_movie <- e_ui - lambda * b_movie[i,1]
      
      if (all(abs(grad_b_movie) > stopping.deriv, na.rm = T)){
        b_movie[i,1] <- b_movie[i,1] + lrate * grad_b_movie
      }
      
      
      grad_b_bin <- e_ui - lambda * b_bin[t,i]
      
      if (all(abs(grad_b_bin) > stopping.deriv, na.rm = T)){
        b_bin[t,i] <- b_bin[t,i] + lrate * grad_b_bin
      } 
      
    }
    #print the values of training and testing RMSE
    if (l %% 10 == 0){
      cat("epoch:", l, "\t")
      est_rating <- array(NA,c(U,I,num_bins),
                          dimnames = list("User"=levels(as.factor(data$userId)),
                                          "Movie"=levels(as.factor(data$movieId)),
                                          "Bin"=levels(as.factor(1:30)))
      )
      for (bin in 1:num_bins){
        est_rating[,,bin] <- matrix(mu,nrow=U,ncol=I) + matrix(rep(b_user,I),ncol=I) +
          matrix(rep(b_movie,U),nrow=U,byrow=T) + matrix(rep(b_bin[bin,],U),nrow=U,byrow=T) + t(p) %*% q
      }
      
      train_RMSE_cur <- RMSE(train, est_rating)
      cat("training RMSE:", train_RMSE_cur, "\t")
      train_RMSE <- c(train_RMSE, train_RMSE_cur)
      
      test_RMSE_cur <- RMSE(test, est_rating)
      cat("test RMSE:",test_RMSE_cur, "\n")
      test_RMSE <- c(test_RMSE, test_RMSE_cur)
    } 
  }
  
  return(list(p = p, q = q, mu = mu, b_user=b_user, b_movie=b_movie, b_bin=b_bin, train_RMSE = train_RMSE, test_RMSE = test_RMSE))
}

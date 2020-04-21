#Define a function to calculate RMSE
RMSE <- function(dataset, mu, b_movie, b_bin, b_user, alpha_u, dev_u, p, q){ 
  data_est_rating <- apply(dataset, 1, estimate_rating, 
                           mu, b_movie, b_bin,
                           b_user, alpha_u, dev_u, 
                           p, q) %>% tibble::enframe() %>%  rename(obs_no=name, estimate_rating=value)
  as.numeric(sqrt(colMeans((dataset["rating"]-data_est_rating["estimate_rating"])^2)))
}

#Define a function to calculate estimated rating based on the parameter results from SGD
estimate_rating <- function(obs, 
                            mu, b_movie, b_bin,
                            b_user, alpha_u, dev_u, 
                            p, q){
  train_movie <-  as.character(as.numeric(obs["movieId"]))
  train_user <- as.character(as.numeric(obs["userId"]))
  train_day <- as.character(as.numeric(obs["days_diff"]))
  
  train_r <- as.numeric(obs["rating"])
  train_bin <- as.character(as.numeric(obs["bin_label"]))
  
  est_pq <- t(p) %*% q #??column label wrong? change to qp?
  
  estimated_r <- mu+b_movie[train_movie,1]+b_bin[train_bin,train_movie]+
    b_user[train_user,1]+alpha_u[train_user,1]*dev_u[train_day,train_user]+
    est_pq[train_user,train_movie]
  
  return(estimated_r)
}

#Stochastic Gradient Descent
# a function returns a list containing factorized matrices p and q, training and testing RMSEs.

# f = 10
# lambda = 0.3
# lrate = 0.01
# max.iter=100
# stopping.deriv = 0.01
# data = data_bins
# train = data_train
# test = data_test
# beta=0.4

gradesc <- function(f = 10, 
                    lambda = 0.3,lrate = 0.01, max.iter, stopping.deriv = 0.01, beta=0.4,
                    data, train, test){
  set.seed(0)
  num_bins <- length(unique(data$bin_label))
  p <- matrix(runif(f*U, -1, 1), ncol = U)  #10x610;  U=610 # of users; f=10 factor dim
  colnames(p) <- levels(as.factor(data$userId)) 
  
  q <- matrix(runif(f*I, -1, 1), ncol = I) #10x9724; I=9724 # of movies;
  colnames(q) <- levels(as.factor(data$movieId))
  
  mu <- runif(1,-1,1) 
  
  b_user <- matrix(runif(U,-1,1),ncol=1) # 610x1: user bias 
  rownames(b_user) <- colnames(p) # userId
  
  b_movie <- matrix(runif(I,-1,1),ncol=1) # 9724x1: movie bias 
  rownames(b_movie) <- colnames(q) # movieId
  
  b_bin <- matrix(runif(I*num_bins,-1,1),ncol=I) # 30x9724: time bin bias
  colnames(b_bin) <- colnames(q)# movieId
  rownames(b_bin) <- levels(as.factor(data$bin_label))
  
  num_days <- length(levels(as.factor(data$days_diff))) # 4105 days
  alpha_u <- matrix(runif(U,-1,1),ncol=1)
  rownames(alpha_u) <- colnames(p) # userId
  
  all_days_diff <- as.numeric(levels(as.factor(data$days_diff)))
  dev_u <- matrix(rep(all_days_diff,U), ncol=U) # 4105x610: days x users
  rownames(dev_u) <- levels(as.factor(data$days_diff))
  colnames(dev_u) <- colnames(p) # userId
  
  for(u in colnames(dev_u)){ #userId
    t_u <- (data %>% filter(userId==u) %>% ungroup() %>% select(user_mean_date))[1,] %>% as.numeric()
    dev_u[,u] <- sign(dev_u[,u]-t_u)*abs(dev_u[,u]-t_u)^beta
  }
  
  train_RMSE <- c()
  test_RMSE <- c()
  for(l in 1:max.iter){
    sample_idx <- sample(1:nrow(train), nrow(train))
    
    #loop through each training case and perform updates: p, q, mu, b_user, b_movie, b_bin, alpha_u
    for (s in sample_idx){
      u <- as.character(train[s,"userId"])
      
      i <- as.character(train[s,"movieId"])
      
      bin <- as.character(train[s,"bin_label"])
      
      d <- as.character(train[s,"days_diff"])
      
      r_ui <- as.numeric(train[s,"rating"])
      
      r_uit_hat <- mu + b_user[u,1] + alpha_u[u,1]*dev_u[d,u] + 
        b_movie[i,1] + b_bin[bin,i] + 
        t(q[,i]) %*% p[,u]

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
      
      grad_alpha_u <- e_ui*dev_u[d,u] - lambda * alpha_u[u,1]
      if (all(abs(grad_alpha_u) > stopping.deriv, na.rm = T)){
        alpha_u[u,1] <- alpha_u[u,1] + lrate * grad_alpha_u
      }
      
      grad_b_movie <- e_ui - lambda * b_movie[i,1]
      if (all(abs(grad_b_movie) > stopping.deriv, na.rm = T)){
        b_movie[i,1] <- b_movie[i,1] + lrate * grad_b_movie
      }

      grad_b_bin <- e_ui - lambda * b_bin[bin,i]
      if (all(abs(grad_b_bin) > stopping.deriv, na.rm = T)){
        b_bin[bin,i] <- b_bin[bin,i] + lrate * grad_b_bin
      }
      
    }
  
    # print the values of training and testing RMSE every 10 iterations
    if (l %% 10 == 0){
      cat("epoch:", l, "\t")
      
      train_RMSE_cur <- RMSE(train, mu, b_movie, b_bin, b_user, alpha_u, dev_u, p, q)
      cat("training RMSE:", train_RMSE_cur, "\t")
      train_RMSE <- c(train_RMSE, train_RMSE_cur)
      
      test_RMSE_cur <- RMSE(test, mu, b_movie, b_bin, b_user, alpha_u, dev_u, p, q)
      cat("test RMSE:",test_RMSE_cur, "\n")
      test_RMSE <- c(test_RMSE, test_RMSE_cur)
    }
  }
  
  return(list(p = p, q = q, mu=mu, b_user=b_user, b_movie=b_movie, b_bin=b_bin,
              train_RMSE = train_RMSE, test_RMSE = test_RMSE))
}

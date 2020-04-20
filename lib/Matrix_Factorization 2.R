#Define a function to calculate RMSE
RMSE <- function(rating, est_rating_pq, bin, date){
  sqr_err <- function(obs){
    i <- as.character(obs[1]) # 1: userId
    j <- as.character(obs[2]) # 2: movieId
    #k <- as.character(obs[5]) # 5: bin_label
    
    #r_uit_hat <- mu + b_user[u,1] + alpha_u[u,1]*dev_u[d,u] + b_movie[i,1]+ b_bin[t,i] + t(q[,i]) %*% p[,u]
    sqr_error <- (obs[3] - est_rating_pq[i,j]-)^2  #3: obs[3] - rating
    return(sqr_error)
  }
  return(sqrt(mean(apply(rating, 1, sqr_err))))
}

#Stochastic Gradient Descent
# a function returns a list containing factorized matrices p and q, training and testing RMSEs.

# f = 10
# lambda = 0.3
# lrate = 0.01
# max.iter=20
# stopping.deriv = 0.01
# data = data_m
# train = data_train
# test = data_test

gradesc <- function(f = 10, 
                    lambda = 0.3,lrate = 0.01, max.iter, stopping.deriv = 0.01,
                    data, train, test){
  set.seed(0)
  #random assign value to matrix p and q
  #?? change to t(q) %*% p
  num_bins <- max(data$bin_label)
  p <- matrix(runif(f*U, -1, 1), ncol = U)  #10x610;  U=610 # of users; f=10 factor dim
  colnames(p) <- as.character(1:U)
  
  q <- matrix(runif(f*I, -1, 1), ncol = I) #10x9724; I=9724 # of movies;
  colnames(q) <- levels(as.factor(data$movieId))
  
  # t(p) %*% q: 610x9724 users x movies
  
  mu <- runif(1,-1,1) #stochastics means random
  
  b_user <- matrix(runif(U,-1,1),ncol=1) # 610x1: user bias random
  rownames(b_user) <- levels(as.factor(data$userId))
  
  b_movie <- matrix(runif(I,-1,1),ncol=1) #9724x1: movie bias random
  rownames(b_movie) <- levels(as.factor(data$movieId))
  
  b_bin <- matrix(runif(I*num_bins,-1,1),ncol=I) #30x9724: bin random; num_bins=30, bin
  colnames(b_bin) <- levels(as.factor(data$movieId))
  
  num_days <- length(levels(as.factor(data$days_diff))) #4105
  alpha_u <- matrix(runif(U,-1,1),ncol=1)
  #b_u_daily <- alpha_u %*% dev_u
  rownames(alpha_u) <- levels(as.factor(data$userId))
  
  all_days_diff <- as.numeric(levels(as.factor(data$days_diff)))
  dev_u <- matrix(rep(all_days_diff,U), ncol=U) #4105x610: days x users
  #dev_u <- matrix(runif(U*num_days,-1,1),ncol=U) 
  rownames(dev_u) <- levels(as.factor(data$days_diff))
  colnames(dev_u) <- levels(as.factor(data$userId))
  for(u in colnames(dev_u)){ #userId
    t_u <- data %>% filter(userId==u) %>% summarise(mean(days_diff))
    t_u <- as.numeric(t_u)
    dev_u[,u] <- sign(dev_u[,u]-t_u)*abs(dev_u[,u]-t_u)^0.4
    }
  
  train_RMSE <- c()
  test_RMSE <- c()
  for(l in 1:max.iter){
    
    #l=1
    sample_idx <- sample(1:nrow(train), nrow(train))
    
    #loop through each training case and perform update
    for (s in sample_idx){
      #s=6924
      
      #cat("mu:", mu, "\n")
      u <- as.character(train[s,"userId"])
      
      i <- as.character(train[s,"movieId"])
      
      t <- train[s,"bin_label"]
      
      d <- as.character(train[s,"days_diff"])
      
      r_ui <- train[s,"rating"] #rating column
      r_uit_hat <- mu + b_user[u,1] + alpha_u[u,1]*dev_u[d,u] + b_movie[i,1]+ b_bin[t,i] + t(q[,i]) %*% p[,u]

      e_ui <- r_ui - r_uit_hat

      grad_q <- e_ui %*% p[,u] - lambda * q[,i]
      
      if (all(abs(grad_q) > stopping.deriv, na.rm = T)){
        q[,i] <- q[,i] + lrate * grad_q
      }
      grad_p <- e_ui %*% q[,i] - lambda * p[,u]
      
      if (all(abs(grad_p) > stopping.deriv, na.rm = T)){
        p[,u] <- p[,u] + lrate * grad_p
      }
      
      #??
      # grad_mu <- e_ui - lambda * mu
      # 
      # if (all(abs(grad_mu) > stopping.deriv, na.rm = T)){
      #   mu <- mu + lrate * grad_mu
      # }
      
      grad_b_user <- e_ui - lambda * b_user[u,1]
      
      if (all(abs(grad_b_user) > stopping.deriv, na.rm = T)){
        b_user[u,1] <- b_user[u,1] + lrate * grad_b_user
      }
      
      grad_alpha_u <- e_ui*dev_u[d,u] - lambda * alpha_u[u,1]
      print(sprintf("d is %s \n", d))
      print(sprintf("u is %s \n", u))
      
      if (all(abs(grad_alpha_u) > stopping.deriv, na.rm = T)){
        alpha_u[u,1] <- alpha_u[u,1] + lrate * grad_alpha_u
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
      est_pq <- t(p) %*% q #??column label wrong? change to qp?
      rownames(est_pq) <- levels(as.factor(data$userId))
      colnames(est_pq) <- levels(as.factor(data$movieId))
      
      rownames(b_movie) <- levels(as.factor(data$movieId))
      rownames(b_user) <- levels(as.factor(data$userId))
      
      
      estimate_rating <- function(obs){#obs=train[i,]
        train_movie <-  as.character(obs["movieId"])
        train_user <- as.character(obs["userId"])
        train_day <- as.character(obs["days_diff"])
        
        train_r <- obs["rating"]
        train_bin <- as.character(obs["bin_label"])
        train_tu <- obs["user_mean_date"]
        
        estimated_r <- mu+b_user[as.numeric(train_movie),1]+b_movie[train_user,1]+b_bin[as.numeric(train_bin),as.numeric(train_movie)]+
          alpha_u[train_user,1]*dev_u[train_day,train_user]+est_pq[as.numeric(train_user),as.numeric(train_movie)]
        
        estimated_r
      }
      
      
      
      
      
      # estimate_rating <- function(obs){#obs=train[i,]
      #   train_movie <-  as.character(obs["movieId"])
      #   # train_user <- as.character(obs["userId"])
      #   # train_day <- as.character(obs["days_diff"])
      #   # 
      #   # train_r <- obs["rating"]
      #   # train_bin <- as.numeric(obs["bin_label"])
      #   # train_tu <- obs["user_mean_date"]
      #   # 
      #   # 
      #   # 
      #   # print(sprintf("mu is %f \n",mu))
      #   print(sprintf("train_user is %s \n",train_user))
      #   print(b_user[as.numeric(train_movie),1])
      #   # print(sprintf("train_bin is %s \n",train_bin))
      #   # print(sprintf("train_movie is %s \n",train_movie))
      #   # print(sprintf("train_day is %s \n",train_day))
      #   # 
      #   # print(sprintf("obs[\"movieId\"] is %s \n", obs["movieId"]))
      #   #print(sprintf("buser obs[\"movieId\"] is %s \n", b_user[obs["movieId"],1]))
      #   
      #   
      #   
      #   
      #   estimated_r <- mu+b_user[train_movie,1]+b_movie[train_user,1]+b_bin[train_bin,train_movie]+
      #   alpha_u[train_user,1]*dev_u[train_day,train_user]+est_pq[train_user,train_movie]
      #   
      #   # print("down")
      #   # print(train_movie)
      #   # print(as.numeric(b_user[train_movie,1]))
      #   
      #   #result <- b_user[train_movie,1]
      #   #print(result)
      #   estimated_r
      # }
      # 
      # apply(train[1,], 1, FUN=estimate_rating)
      # map
      # 
      # # est_rating_pq <- t(p) %*% q
      # # rownames(est_rating) <- levels(as.factor(data$movieId))
      # # est_rating <- array(NA,c(U,I,num_bins, num_days),
      # #                     dimnames = list("User"=levels(as.factor(data$userId)),
      # #                                     "Movie"=levels(as.factor(data$movieId)),
      # #                                     "Bin"=levels(as.factor(1:30)),
      # #                                     "Day"=levels(as.factor(data$days_diff)))
      # #               )
      # # for (bin in 1:num_bins){
      # #   est_rating[,,bin] <- matrix(mu,nrow=U,ncol=I) + 
      # #     matrix(rep(b_user,I),ncol=I) +
      # #     matrix(rep(b_movie,U),nrow=U,byrow=T) + 
      # #     matrix(rep(b_bin[bin,],U),nrow=U,byrow=T) + 
      # #     t(p) %*% q
      # # }
      # # 
      # #r_uit_hat <- mu + b_user[u,1] + alpha_u[u,1]*dev_u[d,u] + b_movie[i,1]+ b_bin[t,i] + t(q[,i]) %*% p[,u]
      # #est_rating <- mu + t(q) %*% p +
      # #rownames(est_rating) <- levels(as.factor(data$movieId))
      # 
      # train_RMSE_cur <- RMSE(train, mu, est_pq, bin, date){ #only a number
      #   r <- train["rating"]
      #   apply()
      #   i <- 1
      #   
      #   
      #   
      #   
      #   
      # }
      cat("training RMSE:", train_RMSE_cur, "\t")
      train_RMSE <- c(train_RMSE, train_RMSE_cur)
      
      test_RMSE_cur <- RMSE(test, est_rating)
      cat("test RMSE:",test_RMSE_cur, "\n")
      test_RMSE <- c(test_RMSE, test_RMSE_cur)
    } 
  }
  
  return(list(p = p, q = q, mu=mu, b_user=b_user, b_movie=b_movie, b_bin=b_bin,
              train_RMSE = train_RMSE, test_RMSE = test_RMSE))
}

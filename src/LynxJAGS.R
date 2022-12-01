
  ## JAGS Model
model{
  ###########################
  # priors
  ###########################
    phi ~ dbeta(y.a, y.b)
    lambda ~ dunif(0, 10)
    sigma.p ~ dunif(0, 100)
    tau <- 1/sigma.p^2
    # initial conditions informed priors
    N[1] ~ dlnorm(log(y[1]/phi) , tau)
    # ????
    mu[1] <- N[1] * phi
    
  ###########################
  # likelihood
  ###########################
    # Process model:
    for(t in 2:(y.endyr+1)){
  	  alpha[t] <- lambda * (N[t-1] - y.H[t-1])
  	  N[t] ~ dlnorm(log(max(alpha[t], 0.000001)), tau) # can't take the log of alpha <= 0
  	  # ????
  	}
    # Data model:
    for(t in 2:y.endyr){
      mu[t] <- N[t] * phi
      # returns density (for continuous) because l.h.s. is data (deterministic b/c defined in data)
  	    y[t] ~ dpois(mu[t]) 
    }
  
  ###########################
  # Derived quantities
  ###########################
    # sum of squares calculation
      # have to put this in own loop because y[1] defined separately 
      for(t in 1:y.endyr){
        # returns random number generator because l.h.s. is not data (i.e. it is unknown: stochastic node)
          y_sim[t] ~ dpois(mu[t]) 
        # sum of squares 
          sq[t] <- (y[t]-mu[t])^2
          sq_sim[t] <- (y_sim[t]-mu[t])^2
      }
    #posterior predictive checks
      # test statistics y
      mean_y <- mean(y)
      sd_y <- sd(y)
      fit_y <- sum(sq)
      # test statistics y_sim
      mean_y_sim <- mean(y_sim)
      sd_y_sim <- sd(y_sim)
      fit_y_sim <- sum(sq_sim)
      # p-values
      p_val_mean <- step(mean_y_sim - mean_y)
      p_val_sd <- step(sd_y_sim - sd_y)
      p_val_fit <- step(fit_y_sim - fit_y)
}
  


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
      fam_grps[1] <- N[1] * phi # mean family groups (mu)
      
    ###########################
    # likelihood
    ###########################
      # Process model:
      for(t in 2:(y.endyr+1)){
    	  alpha[t] <- lambda * (N[t-1] - y.H[t-1])
    	  N[t] ~ dlnorm(log(max(alpha[t], 0.000001)), tau) # can't take the log of alpha <= 0
    	  # calculate the mean for use in the data model
    	    # include in this loop to get the forecast value for #6
    	  fam_grps[t] <- N[t] * phi # mean family groups (mu)
    	}
      # Data model:
      for(t in 2:y.endyr){
        # returns density (for continuous) because l.h.s. is data (deterministic b/c defined in data)
    	    y[t] ~ dpois(fam_grps[t]) 
      }
    
    ###########################
    # Derived quantities
    ###########################
      # sum of squares calculation
        # have to put this in own loop because y[1] defined separately 
        for(t in 1:y.endyr){
          # returns random number generator because l.h.s. is not data (i.e. it is unknown: stochastic node)
            y_sim[t] ~ dpois(fam_grps[t]) 
          # sum of squares 
            sq[t] <- (y[t]-fam_grps[t])^2
            sq_sim[t] <- (y_sim[t]-fam_grps[t])^2
          # autocorrelation
            # Assure yourself that the process model adequately accounts for...
              # ...temporal autocorrelation in the residuals — allowing the assumption... 
              # ...that they are independent and identically distributed. 
              # To do this, include a derived quantity:
            e[t] <- (y[t]-fam_grps[t])
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
  

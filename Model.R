## Model Code

# SAM model for replicating Ogle et al, 2015 


model{
  
  
  for(i in 1:N){ # for each year
    
    NPP[i] ~ dnorm(mu[i], tau) # Likelihood for observed NPP
    
    NPP.rep[i] ~ dnorm(mu[i],tau) # This is replicated to assess the model fit
    
    # Define model for latent (mean) NPP; Event[,k] represents the amount 
    # of precipitation received in different size classes, where k indexes 
    # the even size class (k=1 for < 5 mm; k=2 for 5-15 mm; k=3 for 15-30 mm; 
    # k=4 for >30 mm); convert antecedent precipitation (antX) from inches to mm.
    # 
    mu[i] <- a[1] + a[2]*antX[YearID[i]]*25.4 + a[3]*Event[i,1] + a[4]*Event[i,2] + a[5]*Event[i,3] + a[6]*Event[i,4]
    # Some of the precipitation event data are missing, so specify a simple
    # data model for the Event data for the purpose of estimating the
    # missing data:
    for(k in 1:4){
      Event[i,k] ~ dnorm(mu.ev[k], tau.ev[k]) 
    }
  }
  
  # When using Bayesian statistics, first define a prior probability distribution for the unknown
  # variables that are being investigated. The prior can be informed from previous data, experiments, etc. or it 
  # can be uninformed (e.g. a uniform distribution so that every possible value for the unknown variable(s) 
  # is considered to be equally likely before considering the data that has been observed)
  
  for (j in 1:Nblocks) {    # Assign a prior for the weight for each time block that precipitation data exists for
    deltaX[j] ~ dgamma(1,1) # Ogle uses a Dirichlet prior for the weights of timeframe precipitation. This is coded as
                            # as a gamma distribution due to some reason relating to R
  }


  
  
  
  
  for(t in 1:Nlag){ # For each year in the lag, compute the yearly weights

    for(m in 1:12){ # For each month 
      # Redefine the unnormalized monthly weights to account for post-ANPP harvest period; 
      # i.e., 2nd part involving equals and step functions 
      # # sets weight = 0 if in year 1 and in Oct, Nov, or Dec (i.e., post-ANPP harvest).
      delta[m,t] <- (deltaX[block[t,m]])*(1-equals(t,1)*step(m-9.5))
      
      # Compute normalized monthly weights, which will be between 0 and 1, and will sum to one.
      weight[m,t] <- delta[m,t]/sumD # sumD defined below
      
      # Reorder the weights to go from most recent month (Dec of current year) to “oldest” month 
      # (Jan at past year = Nlag). 
      weightOrdered[(t-1)*12 + (12-m+1)] <- weight[m,t]
      
      # For each time into the past, compute the weighted precipitation variable.
      for(i in Nlag:Nyrs){
        antX1[i,m,t] <- weight[m,t]*ppt[i-t+1,m]
        }
      } 
    }

  
# Calculate the sum of all delta weights over the entire lag period so that we can normalise the weights
  for(t in 1:Nlag){ # For each year considered
    sumD1[t] <- sum(delta[,t]) # Sum the monthly weights
  }
  sumD <- sum(sumD1[]) # Sum the yearly weights to get the total of all weights from over the lag period considered
  
  # Compute the cumulative monthly weights:
  for(t in 1:(12*Nlag)){
    cum.weight[t] <- sum(weightOrdered[1:t]) 
  }   
  
  
  
  # Compute the month within year weights (alpha’s = wP,m in Box 1 in main # text); that is, these weights sum to 1 within each past year
  for(m in 1:12){
    for(t in 1:Nlag){
      alpha[m,t] <- delta[m,t]/sum(delta[,t]) 
    }
  }
  
  
  # Compute antecedent precipitation by summing the weighted precipitation
  # variable over months and past years:
  for(i in Nlag:Nyrs){ # for each year 
    for(t in 1:Nlag){
      ant.sum1[i,t] <- sum(antX1[i,,t]) 
      }
    antX[i] <- sum(ant.sum1[i,]) 
    }
  
  # Assign priors to the ANPP regression parameters (covariate effects):
  for(k in 1:6){
    a[k] ~ dnorm(0,0.0000001) 
    }
  

  
  sigma ~ dunif(0,100) # Uniform distribution for the prior of the standard deviation of the observations
  tau <- 1/(sigma^2) # Turn standard deviation into a precision estimate
  
  # Priors for parameters in the Event missing data model:
  for(k in 1:4){
    mu.ev[k] ~ dunif(0,500) 
    sigma.ev[k] ~ dunif(0,500) 
    tau.ev[k] <- pow(sigma.ev[k],-2)
  }

}
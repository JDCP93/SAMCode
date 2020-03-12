## Model Code

# SAM model for replicating Ogle et al, 2015 

# We are looking into the effects of antecedent conditions on NPP, namely precipitation received over the past five years and then the amount of precipitation received in the growing year within 4 classifications of rainfall events based on intensity

model{
      # Some of the precipitation event data are missing, so specify a simple data model for the Event data for the purpose of estimating the missing data:
      # Very uninformed priors for parameters in the Event missing data model
      for(k in 1:4){
            mu.ev[k] ~ dunif(0,500) 
            sigma.ev[k] ~ dunif(0,500) 
            tau.ev[k] <- pow(sigma.ev[k],-2)
      }
      #The missing Events are normally distributed by the above prior 
      for (i in 1:N){
            for(k in 1:4){
            Event[i,k] ~ dnorm(mu.ev[k], tau.ev[k]) 
            }
      }
  
      # Assign priors to the ANPP regression parameters - these are very precisely zero
      for(k in 1:6){
            a[k] ~ dnorm(0,0.0000001) 
      }
      
      # Assign a prior for the weight for each time block of prior precipitation
      for (j in 1:Nblocks) {    
            deltaX[j] ~ dgamma(1,1) # Ogle uses a Dirichlet prior for the weights of time block precipitation. This is coded as a gamma distribution due to some reason relating to R
      }  
      
      # Compute the monthly weights for the prior precipitation
      for(t in 1:Nlag){
            for(m in 1:12){
                  delta[m,t] <- (deltaX[block[t,m]])*(1-equals(t,1)*step(m-9.5)) # weight for precipitation received after the NPP harvest for the current year is 0 (Oct,Nov,Dec)
                  weight[m,t] <- delta[m,t]/sumD # normalise the monthly weights (sumD is defined below)
                  weightOrdered[(t-1)*12 + (12-m+1)] <- weight[m,t] # Reorder the weights in order of "recentness" so that they run from Dec of current year, Nov,... through to Feb, Jan of oldest year
                  # For each year of observations which has 5 previous years of observations, compute the weighted precipitation variable.
                  for(i in Nlag:Nyrs){
                        antX1[i,m,t] <- weight[m,t]*ppt[i-t+1,m] # Antecedent rainfall is the monthly rainfall multiplied by the normalised monthly weight 
                  }
            } 
      }
  
      # Calculate the sum of all delta weights over the entire lag period so that we can normalise the weights
      for(t in 1:Nlag){
            sumD1[t] <- sum(delta[,t]) # Sum the monthly weights for each lag year 
      }
      sumD <- sum(sumD1[]) # Sum the yearly weights to get the total of all weights from over the lag period considered
  
      # Compute the cumulative monthly weights:
      for(t in 1:(12*Nlag)){
            cum.weight[t] <- sum(weightOrdered[1:t]) 
      }   
  
  
      # Compute antecedent precipitation by summing the weighted precipitation variable over months and past years:
      for(i in Nlag:Nyrs){
            for(t in 1:Nlag){
                  ant.sum1[i,t] <- sum(antX1[i,,t]) # antecedent rainfall in each lagged year t is the sum of the monthly rainfall
            }
            antX[i] <- sum(ant.sum1[i,]) # antecedent rainfall in year i is the sum of the rainfall over the previous 5 years
      }
  
  
  
      # Specify the sd of the NPP likelihood
      sigma ~ dunif(0,100) # Uniform distribution for the prior of the standard deviation of the observations
      tau <- 1/(sigma^2) # Turn standard deviation into a precision estimate, which is the sd of the modelled NPP

      # Define model for latent (mean) NPP; 
      # Event[,k] represents the amount of precipitation in the current growing year received in different size classes, where k indexes the event size class (k=1 for < 5 mm; k=2 for 5-15 mm; k=3 for 15-30 mm; k=4 for >30 mm); 
      for(i in 1:N){
            # Calculate mu, the mean of the distribution of NPP
            mu[i] <- a[1] + a[2]*antX[YearID[i]]*25.4 + a[3]*Event[i,1] + a[4]*Event[i,2] + a[5]*Event[i,3] + a[6]*Event[i,4] # convert antecedent precipitation (antX) from inches to mm.
    
            NPP[i] ~ dnorm(mu[i], tau) # Likelihood for observed NPP - it is a normal distribution with mean mu and sd tau
    
            NPP.rep[i] ~ dnorm(mu[i],tau) # This is replicated to assess the model fit - I don't get this - it's run through twice to see whether the fit changes?
      }
  

      # Compute the month within year weights (alpha’s = wP,m in Box 1 in main text); that is, these weights sum to 1 within each past year
      # I.E. alpha[m,t] gives the importance of month m's contribution to the contribution of year t, ignoring the weight of t's contribution
      for(m in 1:12){
            for(t in 1:Nlag){
                  alpha[m,t] <- delta[m,t]/sum(delta[,t]) 
            }
      }
}
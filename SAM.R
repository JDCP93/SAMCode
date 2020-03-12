# This script collects and formats the data required to replicate Ogle et al's methodology from their 2015 paper

# Ogle, K., Barber, J.J., Barron-Gafford, G.A., Bentley, L.P., Young, J.M., Huxman, T.E., Loik, M.E., Tissue, D.T., 
# 2015. Quantifying ecological memory in plant and ecosystem processes. Ecol Lett 18, 221–235. 
# https://doi.org/10.1111/ele.12399

# Parts of the script are borrowed heavily from M. De Kauwe's version of the modelling process

## Data Initialisation and Model Set Up

rm(list=ls()) # Clear workspace
graphics.off() # This closes all of R's graphics windows.
cat("\f") # Clear console

library(rstudioapi) # Source rstudioapi to set working directory as needed
library(rjags) # Source rjags for Bayesian analysis
library(ggplot2) # Source ggplot2 to plot results
library(gridExtra) # Source gridExtra for better plots

current_path <- getActiveDocumentContext()$path # Find location of this script
setwd(dirname(current_path )) # Set this to working directory


# First import the data (csv files stolen from De Kauwe - data is provided in pdf form in Ogle 2015)

# Import the ANPP and precip partitioned by event data
ANPPandPrecip = read.table("data/dataset2.csv", header = TRUE, stringsAsFactors = FALSE)
    # Note that these data are total annual precip (mm) partitioned into the size of the events that produced the precip
    # Event1 = received in rain events with <5mm precip
    # Event2 = received in rain events with 5-15mm precip
    # Event3 = received in rain events with 15-30mm precip 
    # Event4 = received in rain events with >30mm precip
    # NPP is in g/m^2. It is actually the forage produced which in Lauenroth and Sala (1992) is shown to be linearly correlated with ANPP

# Import monthly precip data
Precip = read.table("data/dataset3.csv", header = TRUE, stringsAsFactors = FALSE)
    # This covers 91 years
    # Note the precip totals are in inches - the model converts this to mm

# Define the data required for the model
Data = list('N'=52 # The number of years for which NPP and precip event data are available
            ,'Nlag'=5 # The number of previous years for which precipitation may affect current NPP
            ,'Nyrs'=91 # The number of years for which monthly precipitation data is available
            ,'block'= structure(.Data=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, # Partition months into time blocks e.g. for month 10,11,12 in year 5, group them into time block 38
                                       13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 
                                       23, 24, 25, 25, 26, 26, 27, 27, 28, 28, 
                                       29, 29, 30, 30, 31, 31, 31, 32, 32, 32, 
                                       33, 33, 33, 34, 34, 34, 35, 35, 35, 36, 
                                       36, 36, 37, 37, 37, 38, 38, 38), 
                               .Dim=c(5,12)) # Organise into years and months by use of dimension
            ,'Nblocks'=38 # The number of time blocks the months are partitioned into
            ,'ppt'=Precip[,-1] # Monthly precip data
            ,'YearID'=ANPPandPrecip$YearID # Year ID for NPP
            ,'Event'=ANPPandPrecip[,c(4,5,6,7)] # Yearly precip event data
#            ,'NPP'=ANPPandPrecip[,2] # Yearly NPP data - comment this out to obtain the priors
            )

# Define the parameters for the model operation
samples <- 50000 # samples to be kept after burn in
burn <- samples * 0.1 # iterations for burn in
nadapt <- 100  # number of iterations where the samplers adapt their behaviour to maximise efficiency
nchains <- 4 # The number of MCMC chains to run
thin <- 10 # thinning rate, save every 10th iteration to reduce correlation between consecutive values in the chain

# Decide the variables to track
parameters = c('mu','a','weightOrdered','cum.weight','sumD1','weight') 

# Put the model system into a variable
jags <- jags.model('Model.R', data=Data, n.chains=nchains, n.adapt=nadapt) 

# Generate the MCMC chain (this is basically running the Bayesian analysis)
fit <- coda.samples(jags, n.iter=samples, n.burnin=burn, thin=thin,
                    variable.names=parameters)

# Save the summary of the output as either the prior or posterior
 if (length(Data$NPP)==0){ # if NPP data isn't used in the model runs then the output is the prior distributions
      priorSummary = summary(fit)
      save("priorSummary",file="priorSummary.Rdata")
 } else{ # NPP data is specified and the output is the posterior distributions
      posteriorSummary = summary(fit)
      save("posteriorSummary",file="posteriorSummary.Rdata")
   }

# Load the prior and posterior data
load("priorSummary.RData")
load("posteriorSummary.Rdata")

# Create variables for the parameters of interest
priorStats = data.frame(priorSummary$statistics)
priorQuantiles = data.frame(priorSummary$quantiles)

      priorMu = priorStats[grep("mu",row.names(priorStats)),]
      priorA = priorStats[grep("a",row.names(priorStats)),]
      priorCum.weight = priorStats[grep("cum.weight",row.names(priorStats)),]
      priorSumD1 = priorStats[grep("sumD1",row.names(priorStats)),]
      priorSumD1Quantiles = priorQuantiles[grep("sumD1",row.names(priorStats)),]

posteriorStats = data.frame(posteriorSummary$statistics)
posteriorQuantiles = data.frame(posteriorSummary$quantiles)

      posteriorMu = posteriorStats[grep("mu",row.names(posteriorStats)),]
      posteriorA = posteriorStats[grep("a",row.names(posteriorStats)),]
      posteriorAQuantiles = posteriorQuantiles[grep("a",row.names(posteriorStats)),]
      posteriorCum.weight = posteriorStats[grep("cum.weight",row.names(posteriorStats)),]
      posteriorSumD1 = posteriorStats[grep("sumD1",row.names(posteriorStats)),]
      posteriorSumD1Quantiles = posteriorQuantiles[grep("sumD1",row.names(posteriorStats)),]

  
# Create data frames for these variables to facilitate plotting  
posteriorYearlyWeights = data.frame(YearIntoPast = 0:4, 
                                    Weight = posteriorSumD1$Mean/sum(posteriorSumD1$Mean), 
                                    min = posteriorSumD1Quantiles$X2.5./sum(posteriorSumD1$Mean), 
                                    max = posteriorSumD1Quantiles$X97.5./sum(posteriorSumD1$Mean))
priorYearlyWeights = data.frame(YearIntoPast = 0:4, 
                                Weight = priorSumD1$Mean/sum(priorSumD1$Mean), 
                                min = priorSumD1Quantiles$X2.5./sum(priorSumD1$Mean), 
                                max = priorSumD1Quantiles$X97.5./sum(priorSumD1$Mean))

aDefinitions=factor(c("PPT","E_0-5","E_5-15","E_15-30","E_>30"),levels=c("PPT","E_0-5","E_5-15","E_15-30","E_>30")) # Define the corresponding variables for the alpha parameters

posteriorYearlyA = data.frame(aDefinitions, 
                              Covariates = posteriorA$Mean[2:6], 
                              min = posteriorAQuantiles$X2.5.[2:6], 
                              max = posteriorAQuantiles$X97.5.[2:6])

# Replicate the plot from Ogle et al 2015 for the alpha and yearly weight (page 227)
plot1 <- ggplot(posteriorYearlyWeights,aes(YearIntoPast,Weight,ymin = min, ymax = max)) + 
      geom_ribbon(data=priorYearlyWeights,fill="grey70") +
      geom_line(data=priorYearlyWeights) +
      geom_pointrange(data=posteriorYearlyWeights) 

plot2 <- ggplot(posteriorYearlyWeights,aes(aDefinitions,Covariates,ymin = min, ymax = max)) + 
      geom_pointrange(data=posteriorYearlyA) 

grid.arrange(plot1, plot2, nrow = 1)






## Data Initialisation and Model Set Up

rm(list=ls()) # Clear workspace
graphics.off() # This closes all of R's graphics windows.

library(rjags) # Source rjags

# This script collects and formats the data required to replicate Ogle et al's methodology from their 2015 paper

# Ogle, K., Barber, J.J., Barron-Gafford, G.A., Bentley, L.P., Young, J.M., Huxman, T.E., Loik, M.E., Tissue, 
# D.T., 2015. Quantifying ecological memory in plant and ecosystem processes. Ecol Lett 18, 221–235. 
# https://doi.org/10.1111/ele.12399

# Parts of the script are borrowed heavily from M. De Kauwe's version of the modelling process

# Ogle uses Bayesian analysis to compute the dependency of NPP on rainfall over the preceding months
# This is done by assigning a weight to precipitation from previous months and then using the Bayesian approach 
# to assess the most likely values of these weights. This can then be used to see where, when and to what extent
# NPP depends on previous rain events.


# First import the data (csv files stolen from De Kauwe - data is provided in pdf form in Ogle 2015)

setwd("~/Documents/SAM Modelling") # Set working directory to the model location

# Import the ANPP and precip partitioned by event data
ANPPandPrecip = read.table("data/dataset2.csv", header = TRUE, stringsAsFactors = FALSE)
# Note that these data are total annual precip (mm) partitioned into the size of the events that produced the 
# precip
# Event1 = received in rain events with <5mm precip
# Event2 = received in rain events with 5-15mm precip
# Event3 = received in rain events with 15-30mm precip 
# Event4 = received in rain events with >30mm precip
# NPP is in g/m^2. It is actually the forage produced which in Lauenroth and Sala (1992) is shown to be linearly
# correlated with ANPP

# Import monthly precip data
Precip = read.table("data/dataset3.csv", header = TRUE, stringsAsFactors = FALSE)
# This covers 91 years
# Note the precip totals are in inches - the model converts this to mm

# Define the variables required for the model, that are dependent on the data
inputs = list(N=52, # Years of data available
         Nlag=5, # A lag of up to 5 years is considered
         Nyrs=91, # Year ID goes from 40 to 91
  # The block is partitioning 5 years of 12 months (60months) into 38 timeframes. The last 24 months are 
  # treated individually, the 3rd year groups months into couples, the 4th and 5th years are grouped into
  # 3 months
  # This is because of the assumption that the lags become less important further back so that the weights
  # can be grouped together to reduce the number of parameters investigated
         block = structure(.Data=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 
                               13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 
                               23, 24, 25, 25, 26, 26, 27, 27, 28, 28, 
                               29, 29, 30, 30, 31, 31, 31, 32, 32, 32, 
                               33, 33, 33, 34, 34, 34, 35, 35, 35, 36, 
                               36, 36, 37, 37, 37, 38, 38, 38),
                          .Dim=c(5,12)), # Organise into years and months by use of dimension
         Nblocks = 38 # The number of timeframes the data is grouped into
        )

N = inputs$N
Nlag = inputs$Nlag
Nyrs = inputs$Nyrs
block = inputs$block
Nblocks = inputs$Nblocks
ppt = Precip[,-1]
YearID = ANPPandPrecip$YearID
Event = ANPPandPrecip[,c(4,5,6,7)]
#NPP = ANPPandPrecip[,2]

Data = list('N'=N,'Nlag'=Nlag,'Nyrs'=Nyrs,'block'=block,'Nblocks'=Nblocks,'ppt'=ppt,'YearID'=YearID,
            'Event'=Event)#,'NPP'=NPP)

samples <- 50000 # samples to be kept after burn in
burn <- samples * 0.1 # iterations for burn in
nadapt <- 100  # number of iterations where the samplers adapt their behaviour to 
               # maximise efficiency
nchains <- 4 
thin <- 10 # thinning rate, save every 10th iteration to reduce correlation between 
           # consecutive values in the chain

parameters = c('mu','a','weightOrdered','cum.weight','sumD1','weight') # Decide the variables we want to track
# Put the model system into a variable
jags <- jags.model('Model.R', data=Data, n.chains=nchains, n.adapt=nadapt) 
# This generates the MCMC chain (this is basically running the Bayesian analysis)
fit <- coda.samples(jags, n.iter=samples, n.burnin=burn, thin=thin,
                    variable.names=parameters)

summary = summary(fit)

stats = data.frame(summary$statistics)
quantiles = data.frame(summary$quantiles)

muIdx=grep("mu",row.names(stats))
mu = stats[muIdx,]

cum.weightIdx=grep("cum.weight",row.names(stats))
cum.weight = stats[cum.weightIdx,]

sumD1Idx=grep("sumD1",row.names(stats))
sumD1 = stats[sumD1Idx,]


plot(1:(12*Nlag), cum.weight$Mean, type="l", xlab='Year', ylab='cum.weight')
plot(1:N, mu$Mean, type="l", xlab='Year', ylab='Mean of Mu')
plot(sumD1$Mean/sum(sumD1$Mean))









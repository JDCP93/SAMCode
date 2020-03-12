rm(list=ls())
library(ggplot2)
library(gridExtra)

setwd("/Users/climate/Documents/SAM Modelling")

  
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





rm(list=ls())
library(ggplot2)
library(gridExtra)

setwd("/Users/climate/Documents/SAM Modelling")

  
load("JDCP110220Prior.RData")

  priorSummary = summary(fit)

  priorStats = data.frame(priorSummary$statistics)
  priorQuantiles = data.frame(priorSummary$quantiles)

  muIdx=grep("mu",row.names(priorStats))
  priorMu = priorStats[muIdx,]
  
  aIdx=grep("a",row.names(priorStats))
  priorA = priorStats[aIdx,]

  cum.weightIdx=grep("cum.weight",row.names(priorStats))
  priorCum.weight = priorStats[cum.weightIdx,]

  sumD1Idx=grep("sumD1",row.names(priorStats))
  priorSumD1 = priorStats[sumD1Idx,]
  
  priorSumD1Quantiles = priorQuantiles[sumD1Idx,]
  
  
load("JDCP110220.RData")
  
  posteriorSummary = summary(fit)
  
  posteriorStats = data.frame(posteriorSummary$statistics)
  posteriorQuantiles = data.frame(posteriorSummary$quantiles)
  
  muIdx=grep("mu",row.names(posteriorStats))
  posteriorMu = posteriorStats[muIdx,]
  
  aIdx=grep("a",row.names(posteriorStats))
  posteriorA = posteriorStats[aIdx,]
  posteriorAQuantiles = posteriorQuantiles[aIdx,]
  
  cum.weightIdx=grep("cum.weight",row.names(posteriorStats))
  posteriorCum.weight = posteriorStats[cum.weightIdx,]
  
  sumD1Idx=grep("sumD1",row.names(posteriorStats))
  posteriorSumD1 = posteriorStats[sumD1Idx,]
  posteriorSumD1Quantiles = posteriorQuantiles[sumD1Idx,]

posteriorYearlyWeights = data.frame(YearIntoPast = 0:4, 
                                    Weight = posteriorSumD1$Mean/sum(posteriorSumD1$Mean), 
                                    min = posteriorSumD1Quantiles$X2.5./sum(posteriorSumD1$Mean), 
                                    max = posteriorSumD1Quantiles$X97.5./sum(posteriorSumD1$Mean))
priorYearlyWeights = data.frame(YearIntoPast = 0:4, 
                                Weight = priorSumD1$Mean/sum(priorSumD1$Mean), 
                                min = priorSumD1Quantiles$X2.5./sum(priorSumD1$Mean), 
                                max = priorSumD1Quantiles$X97.5./sum(priorSumD1$Mean))


plot1 <- ggplot(posteriorYearlyWeights,aes(YearIntoPast,Weight,ymin = min, ymax = max)) + 
  geom_ribbon(data=priorYearlyWeights,fill="grey70") +
  geom_line(data=priorYearlyWeights) +
  geom_pointrange(data=posteriorYearlyWeights) 

aDefinitions=c("PPT","0-5","5-15","15-30",">30")
aDefinitions=factor(aDefinitions,levels=aDefinitions)

posteriorYearlyA = data.frame(aDefinitions, 
                              Covariates = posteriorA$Mean[2:6], 
                              min = posteriorAQuantiles$X2.5.[2:6], 
                              max = posteriorAQuantiles$X97.5.[2:6])

plot2 <- ggplot(posteriorYearlyWeights,aes(aDefinitions,Covariates,ymin = min, ymax = max)) + 
  geom_pointrange(data=posteriorYearlyA) 

grid.arrange(plot1, plot2, nrow = 1)







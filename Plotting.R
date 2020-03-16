rm(list=ls())
library(ggplot2)
library(gridExtra)

#Load ANPP and Precip data
ANPPandPrecip = read.table("data/dataset2.csv", header = TRUE, 
                           stringsAsFactors = FALSE)
# Yearly NPP data
NPPobserved=ANPPandPrecip[,2] 
  
# Load the prior and posterior data
load("priorSummary.RData")
load("posteriorSummary.Rdata")

# Create variables for the parameters of interest
priStats = data.frame(priorSummary$statistics)
priQntls = data.frame(priorSummary$quantiles)

priMu = priStats[grep("mu",row.names(priStats)),]
priA = priStats[grep("a",row.names(priStats)),]
priCum.weight = priStats[grep("cum.weight",row.names(priStats)),]
priSumD1 = priStats[grep("sumD1",row.names(priStats)),]
priSumD1Qntls = priQntls[grep("sumD1",row.names(priStats)),]

posStats = data.frame(posteriorSummary$statistics)
posQntls = data.frame(posteriorSummary$quantiles)

posMu = posStats[grep("mu",row.names(posStats)),]
posMuQntls = posQntls[grep("mu",row.names(posStats)),]
posA = posStats[grep("a",row.names(posStats)),]
posAQntls = posQntls[grep("a",row.names(posStats)),]
posCum.weight = posStats[grep("cum.weight",row.names(posStats)),]
posSumD1 = posStats[grep("sumD1",row.names(posStats)),]
posSumD1Qntls = posQntls[grep("sumD1",row.names(posStats)),]


# Create data frames for these variables to facilitate plotting  
posYearlyWeights = data.frame(YearIntoPast = 0:4, 
                              Weight = posSumD1$Mean/sum(posSumD1$Mean), 
                              min = posSumD1Qntls$X2.5./sum(posSumD1$Mean), 
                              max = posSumD1Qntls$X97.5./sum(posSumD1$Mean))
priYearlyWeights = data.frame(YearIntoPast = 0:4, 
                              Weight = priSumD1$Mean/sum(priSumD1$Mean), 
                              min = priSumD1Qntls$X2.5./sum(priSumD1$Mean), 
                              max = priSumD1Qntls$X97.5./sum(priSumD1$Mean))

# Define the corresponding variables for the alpha parameters
aDefinitions=factor(c("PPT","E_0-5","E_5-15","E_15-30","E_>30"),
                    levels=c("PPT","E_0-5","E_5-15","E_15-30","E_>30")) 

posYearlyA = data.frame(aDefinitions, 
                              Covariates = posA$Mean[2:6], 
                              min = posAQntls$X2.5.[2:6], 
                              max = posAQntls$X97.5.[2:6])

# Replicate the plot from Ogle et al 2015 
# for the alpha and yearly weight (page 227)
plot1 <- ggplot(posYearlyWeights,aes(YearIntoPast,
                                     Weight,
                                     ymin = min, 
                                     ymax = max)) + 
  geom_ribbon(data=priYearlyWeights,fill="grey70") +
  geom_line(data=priYearlyWeights) +
  geom_pointrange(data=posYearlyWeights) 

plot2 <- ggplot(posYearlyWeights,aes(aDefinitions,
                                     Covariates,
                                     ymin = min, 
                                     ymax = max)) + 
  geom_pointrange(data=posYearlyA) 

grid.arrange(plot1, plot2, nrow = 1)

# Plot modelled NPP against observed NPP

NPPobs = data.frame(Year=1:nrow(ANPPandPrecip)+1938,
                    NPP_obs = NPPobserved)
NPPmod = data.frame(Year=1:nrow(ANPPandPrecip)+1938,
                    NPP_mod = posMu[,1],
                    NPP_modmin = posMuQntls[,1],
                    NPP_modmax = posMuQntls[,5])

plot3 <- ggplot(NPPobs) +
  geom_line(data=NPPobs,aes(Year,NPP_obs),color='steelblue',size=3) +
  geom_point(data=NPPobs,aes(Year,NPP_obs),color='steelblue',size=3) +
         geom_line(data=NPPmod,aes(Year,NPP_mod)) +
         geom_pointrange(data=NPPmod,aes(Year,
                                         NPP_mod,
                                         ymin=NPP_modmin,
                                         ymax=NPP_modmax)) 



grid.arrange(plot3)

# Calculate value of R2
RSS = sum((posMu[,1]-NPPobserved)^2,na.rm=TRUE)
TSS = sum((NPPobserved-mean(NPPobserved,na.rm=TRUE))^2,na.rm=TRUE)
R2=1-(RSS)/(TSS)

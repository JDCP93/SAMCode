source("SAMFunction.R")

for (i in 1:10){
  # Create the spring/nonspring timeblock
  timeblock=SpringBlock(i)
  Nlag = timeblock$Nlag
  block = timeblock$block
  # For the timeblock created, run the model
  SAM("data/dataset2.csv","data/dataset3.csv",Nlag,block,prior=FALSE)
}

# Find all outputs available
models = list.files(pattern = "2020-03-19")
# Load them
for (i in 1:length(models)){
  load(models[i])
}


# Find all the model outputs we've loaded
models = objects(pattern = "SAM_posterior")

# Put these into a dataframe as it will be easier to work with
MODELS = data.frame(row.names=models)

for (i in 1:length(models)){
  MODELS[i,1:11]=data.frame(t(sapply(get(models[i]),c)))
}
for (j in 6:11){
  MODELS[,j]=sapply(MODELS[,j],c)
}

MODELS[11,]=MODELS[2,]
MODELS = MODELS[-2,]
attr(MODELS,"row.names")[10]= c("SAM_posterior_10_20")

uniWeights = matrix(0,nrow=nrow(MODELS),ncol=40)
for (i in 1:nrow(MODELS)){
  weights = MODELS$monthlyWeights[[i]]$mean
  k=0
  for (j in seq(1,length(weights),by=3)){
    k = k+1
    uniWeights[i,k] = mean(weights[j:j+2])
  }
}

for (i in 1:10){
  uniWeights[i,] = uniWeights[i,]*i/sum(uniWeights[i,])
}

plotWeights = data.frame("Weights"=as.vector(t(uniWeights)),
                         "Block"=rep(1:40,10),
                         "Season"=rep(c("Oct-Dec","Jul-Sep","Apr-Jun","Jan-Mar"),100),
                         "Model"=rep(c("Lag1","Lag2","Lag3","Lag4","Lag5","Lag6","Lag7","Lag8","Lag9","Lag10"),each=40))


plotWeights$Season=factor(plotWeights$Season,levels=c("Oct-Dec","Jul-Sep","Apr-Jun","Jan-Mar"))

plotWeights$Model=factor(plotWeights$Model,levels=c("Lag1","Lag2","Lag3","Lag4","Lag5","Lag6","Lag7","Lag8","Lag9","Lag10"))

plotWeights = plotWeights[plotWeights$Weights!=0,]

plot1 <- ggplot(plotWeights,aes(x=Block,y=Weights,fill=Season)) +
  geom_bar(stat="identity", position=position_dodge()) +
  facet_grid(.~Model,scales = "free_x",switch = "x", space = "free_y")

last_plot()  



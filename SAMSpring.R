source("SAMFunction.R")

for (i in 1:10){
  # Create the spring/nonspring timeblock
  timeblock=SpringBlock(i)
  Nlag = timeblock$Nlag
  block = timeblock$block
  # For the timeblock created, run the model
  SAM("data/dataset2.csv","data/dataset3.csv",Nlag,block,prior=FALSE)
}

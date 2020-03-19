source("SAMFunction.R")




# Spring in Colorado is ~April, May,June
# 
Nlag = 3
block = matrix(c(1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 1,
                 3, 3, 3, 4, 4, 4, 3, 3, 3, 3, 3, 3, 
                 5, 5, 5, 6, 6, 6, 5, 5, 5, 5, 5, 5), 
               nrow=Nlag,ncol=12,byrow = TRUE)
SAM("data/dataset2.csv","data/dataset3.csv",Nlag,block,prior=FALSE)

Nlag = 5
block = matrix(c(1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 1,
                 3, 3, 3, 4, 4, 4, 3, 3, 3, 3, 3, 3, 
                 5, 5, 5, 6, 6, 6, 5, 5, 5, 5, 5, 5, 
                 7, 7, 7, 8, 8, 8, 7, 7, 7, 7, 7, 7, 
                 9, 9, 9, 10, 10, 10, 9, 9, 9, 9, 9, 9), 
               nrow=Nlag,ncol=12,byrow = TRUE)
SAM("data/dataset2.csv","data/dataset3.csv",Nlag,block,prior=FALSE)

Nlag = 5
block = matrix(c(1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 1,
                 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 1, 
                 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 1, 
                 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 1, 
                 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 1), 
               nrow=Nlag,ncol=12,byrow = TRUE)
SAM("data/dataset2.csv","data/dataset3.csv",Nlag,block,prior=FALSE)

Nlag = 3
block = matrix(c(1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 1,
                 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 1, 
                 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 1), 
               nrow=Nlag,ncol=12,byrow = TRUE)
SAM("data/dataset2.csv","data/dataset3.csv",Nlag,block,prior=FALSE)


Nlag = 10
block = matrix(c(1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 1,
                 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 1,
                 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 1,
                 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 1,
                 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 1,
                 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 1,
                 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 1,
                 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 1,
                 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 1,
                 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 1), 
               nrow=Nlag,ncol=12,byrow = TRUE)
SAM("data/dataset2.csv","data/dataset3.csv",Nlag,block,prior=FALSE)

Nlag = 10
block = matrix(c(1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 1,
                 3, 3, 3, 4, 4, 4, 3, 3, 3, 3, 3, 3, 
                 5, 5, 5, 6, 6, 6, 5, 5, 5, 5, 5, 5, 
                 7, 7, 7, 8, 8, 8, 7, 7, 7, 7, 7, 7, 
                 9, 9, 9, 10, 10, 10, 9, 9, 9, 9, 9, 9,
                 11, 11, 11, 12, 12, 12, 11, 11, 11, 11, 11, 11,
                 13, 13, 13, 14, 14, 14, 13, 13, 13, 13, 13, 13,
                 15, 15, 15, 16, 16, 16, 15, 15, 15, 15, 15, 15,
                 17, 17, 17, 18, 18, 18, 17, 17, 17, 17, 17, 17,
                 19, 19, 19, 20, 20, 20, 19, 19, 19, 19, 19, 19), 
               nrow=Nlag,ncol=12,byrow = TRUE)
SAM("data/dataset2.csv","data/dataset3.csv",Nlag,block,prior=FALSE)
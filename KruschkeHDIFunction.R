HDIofMCMC = function( sampleVec , credMass=0.95 ) {
      # Computes highest density interval from a sample of representative values,
      #   estimated as shortest credible interval.
      # Arguments:
      #   sampleVec
      #     is a vector of representative values from a probability distribution.
      #   credMass
      #     is a scalar between 0 and 1, indicating the mass within the credible
      #     interval that is to be estimated.
      # Value:
      #   HDIlim is a vector containing the limits of the HDI
      sortedPts = sort( sampleVec )
      ciIdxInc = ceiling( credMass * length( sortedPts ) )
      nCIs = length( sortedPts ) - ciIdxInc
      ciWidth = rep( 0 , nCIs )
      for ( i in 1:nCIs ) {
            ciWidth[ i ] = sortedPts[ i + ciIdxInc ] - sortedPts[ i ]
      }
      HDImin = sortedPts[ which.min( ciWidth ) ]
      HDImax = sortedPts[ which.min( ciWidth ) + ciIdxInc ]
      HDIlim = c( HDImin , HDImax )
      return( HDIlim )
}



HDIofMCMC(as.matrix(fit[,"SumD1[1]"]))


fit[["a[1]"]]

test=as.matrix(fit)
test1 = test[,c("sumD1[1]","sumD1[2]","sumD1[3]","sumD1[4]","sumD1[5]")]

HDIsumD11 = HDIofMCMC(test1[,1])/sum(priorSumD1$Mean)
HDIsumD12 = HDIofMCMC(test1[,2])/sum(priorSumD1$Mean)
HDIsumD13 = HDIofMCMC(test1[,3])/sum(priorSumD1$Mean)
HDIsumD14 = HDIofMCMC(test1[,4])/sum(priorSumD1$Mean)
HDIsumD15 = HDIofMCMC(test1[,5])/sum(priorSumD1$Mean)

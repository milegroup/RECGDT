GlobalBeats <- function(ECGData)
{
  #Sets global positions for beats based on the positions detected on all of the channels of ECGData.
  #   
  # Args:
  #   ECGData: The structure where the beats were previously detected.
  #
  # Returns:
  #  The data model with the global QRS positions information added.
  
  globalBeats <- vector()
  finalGlobalBeats <- vector()
  auxPos <- vector()
  auxChannel <- vector()
  beat <- data.frame()
  limit <- trunc(ECGData$sFreq / 9.7)
  limit2 <- trunc(ECGData$sFreq / 3.1)
  i <- 1
  
  for (c in 1:ECGData$nLeads)
  {
    if (length(ECGData$lead[[c]]$beat) > 0)
    {
      for (b in 1:(length(ECGData$lead[[c]]$beat)))
      {
        auxPos[i]<- ECGData$lead[[c]]$beat[[b]]$QRS$peak[1]
        auxChannel[i] <- c
        i <- i + 1
      }
    }
    else
    {
      next
    }
  }
  
  auxBeats <- data.frame(auxPos, auxChannel)
  auxBeats <- na.omit(auxBeats)
  auxBeats <- auxBeats[ order(auxBeats[,1]), ]
  i <- 1
  b <- 1
  j <- 1
  totalBeats <- length(auxBeats[,1])
  
  while (j <= totalBeats)
  {
    beat[i, 1] <- auxBeats[j, 1]
    beat[i, 2] <- auxBeats[j, 2]
    
    while ((j < totalBeats) && (auxBeats[j+1, 1] < auxBeats[j, 1]+limit))
    {
      beat[i, 1] <- auxBeats[j, 1]
      beat[i, 2] <- auxBeats[j, 2]
      i <- i + 1
      j <- j + 1
    }
    
    if (i > floor(ECGData$nLeads*0.25))
    {
      globalBeats[b] <- round(median(beat[ , 1]))
      b <- b + 1
      i <- 1
    }
    
    j <- j + 1
  }
  
  k <- 2
  finalGlobalBeats[1] <- globalBeats[1]
  
  for (i in 2:length(globalBeats))
  {
    if(globalBeats[i] > globalBeats[i-1] + limit2)
    {
      finalGlobalBeats[k] <- globalBeats[i]
      k <- k + 1
    }
    
    else
    {
      next
    }
  }
  
  i = 1
  RRDistances <- vector()
  ECGData$beat <- list()
  
  for (i in 1:length(finalGlobalBeats))
  {
    ECGData$beat[[i]] <- list()
    ECGData$beat[[i]]$pos <- finalGlobalBeats[i]
  }
  
  for (j in 1:length(finalGlobalBeats)-1)
  {
    RRDistances[j] <- finalGlobalBeats[j+1] - finalGlobalBeats[j]
  }
  
  ECGData$RRDistance <- mean(RRDistances)/ECGData$sFreq
  cat("RR Distances explained: mean in samples is ", mean(RRDistances), ", mean / sFreq is ", mean(RRDistances)/ECGData$sFreq, "mean in ms is ", mean(RRDistances)/ECGData$sFreq*1000, "rounded mean in ms is ", round((mean(RRDistances)/ECGData$sFreq)*1000), "\n")
  cat("RRDistance is: ", ECGData$RRDistance, "\n")
  cat("Global positions for beats obtained.\n\n")
  
  return(ECGData)
}
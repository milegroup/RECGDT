SmoothBasalBeat <- function(basalBeat)
{
  #Applies smoothing procedures to the basal beat.
  #   
  # Args:
  #   basalBeat: The basal beat raw signal to smooth.
  #
  # Returns:
  #  The smooth signal for the basal beat.
  
  cont <- 0
  endPoint <- length(basalBeat)-1
  maxValue <- basalBeat[1]
  minValue <- basalBeat[1]
  
  for (i in 2:endPoint)
  {
    if ((basalBeat[i]-basalBeat[i-1])*(basalBeat[i]-basalBeat[i+1]) > 0)
    {
      cont <- cont+1
      
      if (basalBeat[i] > maxValue)
      {
        maxValue <- basalBeat[i]
      }
      
      else
      {
        if (basalBeat[i] < minValue)
        {
          minValue <- basalBeat[i]
        }
      }
    }
  }

  if(cont > 45)
  {
    
    for (i in 2:endPoint)
    {
      
      if ((basalBeat[i]-basalBeat[i-1])*(basalBeat[i]-basalBeat[i+1]) > 0 && basalBeat[i] != maxValue && basalBeat[i] != minValue && abs(basalBeat[i]-basalBeat[i-1]) < 20)
      {
        basalBeat[i] <- (basalBeat[i-1]+basalBeat[i+1])/2
      }
      
    }
  }
  
  return(basalBeat)
}

CreateBasalBeat <- function(ECGData, nChannel = 1, nBeat, left, right, beats = 15)
{
  #Create the basal beat for a given beat.
  #   
  # Args:
  #   ECGData: The structure where the peaks positions and heights are stored.
  #   nChannel: which channel of the signal will be used.
  #   nBeat: which beat of the channel will be used.
  #   left: ms of signal to take on the left of each beat position.
  #   right: ms of signal to take on the right of each beat position.
  #   beats: number of beats before and after nBeat used to obtain the basal beat.
  #
  # Returns:
  #  The data model with the basal beat added.
  
  if (missing(ECGData))
  {
    stop("You must specify the ECGData parameter")
  }
  
  if (missing(nBeat))
  {
    stop("You must specify the nBeat parameter")
  }
  
  if (missing(left))
  {
    stop("You must specify the left parameter")
  }
  
  if (missing(right))
  {
    stop("You must specify the right parameter")
  }
  
  avgBeat <- vector()
  nSamplesLeft <- round(ECGData$sFreq*left/1000)
  nSamplesRight <- round(ECGData$sFreq*right/1000)
  leftBeats <- beats
  rightBeats <- beats
  
  if (nBeat-beats < 0)
  {
    leftBeats <- nBeat-1
    rightBeats <- beats+(beats-nBeat+1)
    cat("Sorry, there aren't", beats, "beats before the beat nBeat (", nBeat, ")\n")
    cat("\tBasal beat created using the", leftBeats, "previous beat(s) and the", rightBeats, "next one(s)\n")
  }
  
  if (nBeat+beats > length(ECGData$lead[[nChannel]]$beat))
  {
    leftBeats <- beats+(beats-(length(ECGData$lead[[nChannel]]$beat)-nBeat))
    rightBeats <- length(ECGData$lead[[nChannel]]$beat)-nBeat
    cat("Sorry, there aren't", beats, "beats after the beat nBeat (", nBeat, ")\n")
    cat("\tBasal beat created using the", leftBeats, "previous beat(s) and the", rightBeats, "next one(s)\n")
  }
  
  if (ECGData$lead[[nChannel]]$beat[[nBeat-leftBeats]]$QRS$peak-nSamplesLeft < 0)
  {
    stop("Values out of signal range. You must specify a lower value for left or a higher starting nBeat")
  }
  
  if (ECGData$lead[[nChannel]]$beat[[nBeat+rightBeats]]$QRS$peak+nSamplesRight > as.numeric(ECGData$nSamples))
  {
    stop("Values (", ECGData$lead[[nChannel]]$beat[[nBeat+rightBeats]]$QRS$peak+nSamplesRight,") out of signal range (", ECGData$nSamples, "). You must specify a lower value for right or a lower starting nBeat")
  }
  
  corrBeats <- list()
  
  for (i in 1:leftBeats+rightBeats+1)
  {
    corrBeats[[i]] <- vector()
  }

  ECGDataDetrend <- vector()
  y <- 41
  len <- length(ECGData$lead[[nChannel]]$val)
  ECGDataDetrend <- as.vector(filter(ECGData$lead[[nChannel]]$val, filter = 1/y*rep(1, y), method = "convolution", sides = 2))
  ECGDataDetrend[1:((y-1)/2)] = ECGDataDetrend[(y-1)/2+1]
  ECGDataDetrend[(len-(y-1)/2+1):len] = ECGDataDetrend[len-(y-1)/2]
  ECGDataDetrend <- ECGData$lead[[nChannel]]$val-ECGDataDetrend
  i <- 1
  
  for (j in (0-nSamplesLeft):nSamplesRight)
  {
    o <- 1
    
    for (k in (nBeat-leftBeats):(nBeat+rightBeats))
    {
      corrBeats[[o]] <- c(corrBeats[[o]], ECGDataDetrend[(ECGData$lead[[nChannel]]$beat[[k]]$QRS$peak)+j])
      o <- o + 1
    } 
    
    i <- i + 1
  }
  
  corrValues <- vector()
  v <- 1
  lag <- vector()
  middleBeat <- floor(length(corrBeats)/2)
  
  for (k in 1:length(corrBeats))
  {
    
    if(k != middleBeat)
    {
      corrResults <- ccf(corrBeats[[middleBeat]], corrBeats[[k]], plot = FALSE)
      corrValues[[v]] <- as.numeric(max(corrResults$acf))
      acfPos <- which(corrResults$acf == as.numeric(max(corrResults$acf)))
      lag[[v]] <- as.numeric(corrResults$lag[[acfPos]])
    }
    
    else
    {
      corrValues[[v]] <- 1
      lag[[v]] <- 0
    }
    
    v <- v + 1
    
  } 
  
  i <- 1
  corrSum <- sum(corrValues)
  
  for (j in (0-nSamplesLeft):nSamplesRight)
  { 
    added <- 0
    o <- 1
    
    for (k in (nBeat-leftBeats):(nBeat+rightBeats))
    {
      added <- added+(ECGDataDetrend[(ECGData$lead[[nChannel]]$beat[[k]]$QRS$peak)+j+lag[[o]]]*(corrValues[[o]]/corrSum))
      o <- o + 1
    }
    
    avgBeat[i] <- added
    i <- i + 1
  }
  
  avgBeat <- SmoothBasalBeat(avgBeat)
  avgBeat <- SmoothBasalBeat(avgBeat)
  avgBeat <- SmoothBasalBeat(avgBeat)
  avgBeat <- SmoothBasalBeat(avgBeat)

  ECGData$beat[[nBeat]]$basalBeat[[nChannel]]$basalBeatSignal <- avgBeat
  ECGData$beat[[nBeat]]$basalBeat[[nChannel]]$basalBeatRPeakPos <- nSamplesLeft
  
  position <- 1  
  plot.new()
  
  leftBound <- ECGData$lead[[nChannel]]$beat[[nBeat]]$QRS$peak-nSamplesLeft
  rightBound <- ECGData$lead[[nChannel]]$beat[[nBeat]]$QRS$peak+nSamplesRight
  
  meanBeat <- mean(ECGDataDetrend[leftBound:rightBound])
  meanAvgBeat <- mean(avgBeat)
  diffMean <- meanBeat - meanAvgBeat
  
  for (i in 1:length(avgBeat))
  {
    avgBeat[i] <- avgBeat[i] + diffMean
  }
  
  x <- 1:length(ECGData$beat[[nBeat]]$basalBeat[[nChannel]]$basalBeatSignal)
  y1 <- avgBeat[1:length(avgBeat)]
  y2 <- ECGDataDetrend[leftBound:rightBound]      
  minimum1 <- min(y1)
  maximum1 <- max(y1)
  minimum2 <- min(y2)
  maximum2 <- max(y2)
  
  RPeak <- ECGData$beat[[nBeat]]$basalBeat[[nChannel]]$basalBeatRPeakPos
  
  if(minimum1 < minimum2)
  {
    minimum <- minimum1
  }
  
  else
  {
    minimum <- minimum2
  }
  
  if(maximum1 > maximum2)
  {
    maximum <- maximum1
  }
  
  else
  {
    maximum <- maximum2
  }
  
  #Plots in grey the basal beat for the beat represented on the plot
  plot(x, y2, col = "indianred4", ylim = c(minimum, maximum), type = "l", lwd = 2, main = paste("Basal beat for the beat", nBeat, "of the record:", ECGData$id, "/ Channel:", ECGData$lead[[nChannel]]$id), xlab = "Samples per beat", ylab = "Values in mV")
  legend("topright", legend = c("Basal beat (grey), original beat from signal (red)"), text.font=3, cex=1, xjust = 300, yjust = 300)
  #Plots in red the real beat from the ECG signal
  points(x, y1, col = "gray42", type = "l", lwd = 4.25)
  points(x[RPeak], y1[RPeak], col = "darkred", pch = 13, lwd = 2.75, cex = 1.7)
  
  text(RPeak, ECGData$beat[[nBeat]]$basalBeat[[nChannel]]$basalBeatSignal[RPeak]+20, labels = "R Peak", cex = 0.8, font = 2, col= "darkred")
  
  return(ECGData)
}   

CreateGlobalBasalBeat <- function(ECGData, nChannel = 1, nBeat, left, right, beats = 15)
{
  #Create the basal beat for a given beat, using the information of all channels for beat positions.
  #   
  # Args:
  #   ECGData: The structure where the peaks positions and heights are stored.
  #   nChannel: which channel of the signal will be used.
  #   nBeat: which beat of the channel will be used.
  #   left: ms of signal to take on the left of each beat position.
  #   right: ms of signal to take on the right of each beat position.
  #   beats: number of beats before and after nBeat used to obtain the basal beat.
  #
  # Returns:
  #  The data model with the global basal beat added.
  
  if (missing(ECGData))
  {
    stop("You must specify the ECGData parameter")
  }
  
  if (missing(nBeat))
  {
    stop("You must specify the nBeat parameter")
  }
  
  if (missing(left))
  {
    stop("You must specify the left parameter")
  }
  
  if (missing(right))
  {
    stop("You must specify the right parameter")
  }
  
  avgBeat <- vector()
  nSamplesLeft <- round(ECGData$sFreq*left/1000)
  nSamplesRight <- round(ECGData$sFreq*right/1000)
  leftBeats <- beats
  rightBeats <- beats
  
  if (nBeat-beats < 0)
  {
    leftBeats <- nBeat-1
    rightBeats <- beats+(beats-nBeat+1)
    cat("Sorry, there aren't", beats, "beats before the beat nBeat (", nBeat, ")\n")
    cat("\tBasal beat created using the", leftBeats, "previous beat(s) and the", rightBeats, "next one(s)\n")
  }
  
  if (nBeat+beats > length(ECGData$beat))
  {
    leftBeats <- beats+(beats-(length(ECGData$beat)-nBeat))
    rightBeats <- length(ECGData$beat)-nBeat
    cat("Sorry, there aren't", beats, "beats after the beat nBeat (", nBeat, ")\n")
    cat("\tBasal beat created using the", leftBeats, "previous beat(s) and the", rightBeats, "next one(s)\n")
  }
  
  if (ECGData$beat[[nBeat-leftBeats]]$pos-nSamplesLeft < 0)
  {
    stop("Values out of signal range. You must specify a lower value for left or a higher starting nBeat")
  }
  
  if (ECGData$beat[[nBeat+rightBeats]]$pos+nSamplesRight > as.numeric(ECGData$nSamples))
  {
    stop("Values (", ECGData$beat[[nBeat+rightBeats]]$pos+nSamplesRight,") out of signal range (", ECGData$nSamples, "). You must specify a lower value for right or a lower starting nBeat")
  }
  
  corrBeats <- list()
  
  for (i in 1:leftBeats+rightBeats+1)
  {
    corrBeats[[i]] <- vector()
  }
  
  ECGDataDetrend <- vector()
  
  y <- 21
  len <- length(ECGData$lead[[nChannel]]$val)
  ECGDataDetrend <- as.vector(filter(ECGData$lead[[nChannel]]$val, filter = 1/y*rep(1, y), method = "convolution", sides = 2))
  ECGDataDetrend[1:((y-1)/2)] = ECGDataDetrend[(y-1)/2+1]
  ECGDataDetrend[(len-(y-1)/2+1):len] = ECGDataDetrend[len-(y-1)/2]
  ECGDataDetrend <- ECGData$lead[[nChannel]]$val-ECGDataDetrend
  i <- 1
  
  for (j in (0-nSamplesLeft):nSamplesRight)
  {
    o <- 1
    for (k in (nBeat-leftBeats):(nBeat+rightBeats))
    {
      corrBeats[[o]] <- c(corrBeats[[o]], ECGDataDetrend[(ECGData$beat[[k]]$pos)+j])
      o <- o + 1
    } 
    i <- i + 1
  }
  
  corrValues <- vector()
  v <- 1
  lag <- vector()
  middleBeat <- floor(length(corrBeats)/2)
  
  for (k in 1:length(corrBeats))
  {
    if(k != middleBeat)
    {
      corrResults <- ccf(corrBeats[[middleBeat]], corrBeats[[k]], plot = FALSE)
      corrValues[[v]] <- as.numeric(max(corrResults$acf))
      acfPos <- which(corrResults$acf == as.numeric(max(corrResults$acf)))
      lag[[v]] <- as.numeric(corrResults$lag[[acfPos]])
    }
    
    else
    {
      corrValues[[v]] <- 1
      lag[[v]] <- 0
    }
    
    v <- v + 1
  } 
  
  i <- 1
  corrSum <- sum(corrValues)
  
  for (j in (0-nSamplesLeft):nSamplesRight)
  { 
    added <- 0
    o <- 1
    
    for (k in (nBeat-leftBeats):(nBeat+rightBeats))
    {
      added <- added+(ECGDataDetrend[(ECGData$beat[[k]]$pos)+j+lag[[o]]]*(corrValues[[o]]/corrSum))
      o <- o + 1
    } 
    
    avgBeat[i] <- added
    i <- i + 1
  }
  
  avgBeat <- SmoothBasalBeat(avgBeat)
  avgBeat <- SmoothBasalBeat(avgBeat)
  avgBeat <- SmoothBasalBeat(avgBeat)
  avgBeat <- SmoothBasalBeat(avgBeat)
  
  ECGData$beat[[nBeat]]$basalBeat[[nChannel]]$basalBeatSignal <- avgBeat
  ECGData$beat[[nBeat]]$basalBeat[[nChannel]]$basalBeatRPeakPos <- nSamplesLeft
  position <- 1  
  plot.new()
  leftBound <- ECGData$beat[[nBeat]]$pos-nSamplesLeft
  rightBound <- ECGData$beat[[nBeat]]$pos+nSamplesRight
  meanBeat <- mean(ECGDataDetrend[leftBound:rightBound])
  meanAvgBeat <- mean(avgBeat)
  diffMean <- meanBeat - meanAvgBeat
  
  for (i in 1:length(avgBeat))
  {
    avgBeat[i] <- avgBeat[i] + diffMean
  }
  
  ECGData$beat[[nBeat]]$basalBeat[[nChannel]]$avgBeat <- avgBeat
  ECGData$beat[[nBeat]]$DetrendPlot <- ECGDataDetrend[leftBound:rightBound]

  x <- 1:length(ECGData$beat[[nBeat]]$basalBeat[[nChannel]]$basalBeatSignal)
  y1 <- avgBeat[1:length(avgBeat)]
  y2 <- ECGDataDetrend[leftBound:rightBound]      
  minimum1 <- min(y1)
  maximum1 <- max(y1)
  minimum2 <- min(y2)
  maximum2 <- max(y2)
  
  RPeak <- ECGData$beat[[nBeat]]$basalBeat[[nChannel]]$basalBeatRPeakPos
  
  if(minimum1 < minimum2)
  {
    minimum <- minimum1
  }
  
  else
  {
    minimum <- minimum2
  }
  
  if(maximum1 > maximum2)
  {
    maximum <- maximum1
  }
  
  else
  {
    maximum <- maximum2
  }
  
  return(ECGData)

} 

CreateBasalSignal <- function(ECGData, nChannel = 1, left, right, beats = 5)
{
  #Create the basal beats signal for a given channel, using the information of all channels for beat positions.
  #   
  # Args:
  #   ECGData: The structure where the peaks positions and heights are stored.
  #   nChannel: which channel of the signal will be used.
  #   left: ms of signal to take on the left of each beat position.
  #   right: ms of signal to take on the right of each beat position.
  #   beats: number of beats before and after nBeat used to obtain the basal beats.
  #
  # Returns:
  #  The basal signal for the given channel.
  
  basalSignal <- vector()
  basalAux <- vector()
  leftLim <- beats+1
  rightLim <- length(ECGData$beat)-leftLim 
  
  for (i in leftLim:rightLim)
  {
    basalAux <- CreateGlobalBasalBeat(ECGData, nChannel, i, left, right, beats)
    basalSignal <- append(basalSignal, basalAux)
  }
  
  return(basalSignal)
}

AdaptedBasalBeat <- function(ECGData, nChannel = 1, nBeat, beats = 15)
{
  #Create an adapted basal beat for a given beat.
  #   
  # Args:
  #   ECGData: The structure where the peaks positions and heights are stored.
  #   nChannel: which channel of the signal will be used.
  #   nBeat: which beat of the channel will be used.
  #   beats: number of beats before and after nBeat used to obtain the basal beat.
  #
  # Returns:
  #  The data model with the adapted basal beat added.
  
  if (missing(ECGData))
  {
    stop("You must specify the ECGData parameter")
  }
  
  if (missing(nBeat))
  {
    stop("You must specify the nBeat parameter")
  }
  
  RRDistances <- vector()
  i <- 1
  
  for (j in (nBeat-5):(nBeat+4))
  {
    RRDistances[i] <- ECGData$beat[[j+1]]$pos - ECGData$beat[[j]]$pos
    i <- i+1
  }
  
  basalWindow <- (mean(RRDistances)/ECGData$sFreq)*1000
  
  msBefore <- round(0.275 * basalWindow)

  if(msBefore > 370)
  {
    msBefore <- 370
  }
  
  msAfter <- round(0.57 * basalWindow)
  ECGData <- CreateGlobalBasalBeat(ECGData, nChannel, nBeat, msBefore, msAfter, beats)
  
  return(ECGData)
}

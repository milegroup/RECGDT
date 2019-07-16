BeatDetectionPreprocessor <- function(ECGData, channel, windowSize, ...)
{
  #Preprocessing tasks to get the signal ready to the beat detection phase.
  # 
  # Args:
  #   ECGData: The structure where the ECG record is stored.
  #   channel: The channel where the preprocessing is applied.
  #   windowSize: The length of the window used in the moving window integrator step.
  #
  # Returns:
  #  The signal processed and ready to beat detection.
  #
  
  if (missing(ECGData))
  {
    stop("You must specify the ECGData parameter")
  }
  
  if (missing(channel))
  {
    stop("You must specify the channel parameter")
  }
  
  if (ECGData$Verbose)
  {
    cat("   Preprocessing the channel", channel, "\n")
  }
  
  preProcessedSignal <- list()
  channelLength <- length(ECGData$lead[[channel]]$val)
  
  #Normalization of the original signal.
  ECGData$lead[[channel]]$val <- ECGData$lead[[channel]]$val/max(abs(ECGData$lead[[channel]]$val))
  
  #Low pass filter of the original signal.
  lowPassFilter <- mat.or.vec(channelLength, 1)
  out <- .C("lowPassFilter", lowPassFilter = as.double(lowPassFilter), as.double(ECGData$lead[[channel]]$val), channelLength)
  lowPassFilter <- out$lowPassFilter
  
  #High pass filter of the low pass filter signal.
  highPassFilter <- mat.or.vec(channelLength, 1)
  out <- .C("highPassFilter", highPassFilter = as.double(highPassFilter), as.double(lowPassFilter), channelLength)
  highPassFilter <- out$highPassFilter
  
  #Derivative function of the high pass filter signal.
  derivative <- mat.or.vec(channelLength, 1)
  out <- .C("derivative", derivative = as.double(derivative), as.double(highPassFilter), channelLength)
  preProcessedSignal$derivative <- out$derivative
  
  #Squaring function of the derivative signal.
  derivativeSquare <- out$derivative*out$derivative
  
  #Moving-window integrator of the squared derivative signal.
  mwi <- mat.or.vec(channelLength, 1)
  out <- .C("mwi", mwi = as.double(mwi), as.double(derivativeSquare), channelLength, windowSize)
  
  #Normalization of the mwi signal.
  preProcessedSignal$mwi <- out$mwi/max(abs(out$mwi))
  
  return(preProcessedSignal)
}

PeakDetection <- function(signal, sFreq, windowSize = 222, ...)
{
  #Detects local maximum peaks in the signal. A peak is considered the maximum when it is the higher peak inside a window.
  #   
  # Args:
  #   signal: The signal where the peaks will be detected.
  #   sFreq: The sampling frequency of the signal.
  #   windowSize: The window size in ms where the maximum is searched.
  #   
  # Returns:
  #   A data frame containing the peaks positions and heights.
  #
  
  signalLength <- length(signal)
  positions <- mat.or.vec(signalLength, 1)
  heights <- mat.or.vec(signalLength, 1) 
  windowSamples <- windowSize/1000*sFreq
  out <- .C("peakDetection", as.double(signal), positions = as.integer(positions), heights = as.double(heights), as.integer(signalLength), as.integer(windowSamples))
  positions <- out$positions[which(out$positions != 0)]
  heights <- out$heights[which(out$heights != 0)]
  positions <- positions[1:length(heights)]
  signalPeaks <- data.frame(positions, heights)  
  return(signalPeaks)  
}

RootMeanSquared <- function(signal,...)
{
  #Performs the square root of the mean of the squared signal.
  #   
  # Args:
  #   signal: The signal to apply the transformation.
  #   
  # Returns:
  #   The square root of the mean of the squared signal.
  #
  
  rms <- (sqrt(sum(signal*signal)/length(signal)))
  return(rms)
}

TwoFirstPeaks <- function(signal, threshold, derivative, sFreq, ...)
{
  # Get the two first peaks which exceed the threshold.
  # 
  # Args:
  #   signal: ECG signal where to detect the peaks.
  #   threshold: Threshold to search for peaks which are greater than it.
  #   derivative: The derivative signal from the preproccesing stage, for false QRS detection removal.
  #   sFreq: ECG signal sampling frequency.
  #
  # Returns:
  #  A data frame containing the two first peaks that match the requirements.
  #
  
  if (missing(signal))
  {
    stop("You must specify the signal parameter")
  }
  
  if (missing(threshold))
  {
    stop("You must specify the threshold parameter")
  }
  
  if (missing(derivative))
  {
    stop("You must specify the derivative parameter")
  }
  
  if (missing(sFreq))
  {
    stop("You must specify the sFreq parameter")
  }
  
  surpassingThreshold <- (signal > threshold)
  posDifferences <- diff(surpassingThreshold, differences = 1)
  leftPositions <- which(posDifferences > 0)
  rightPositions <- which(posDifferences < 0)
  
  if(is.na(rightPositions[1]))
  {
    rightPositions[1] <- length(signal)
  }
  
  height1 <- max(signal[leftPositions[1]:rightPositions[1]])
  position1 <- which(signal[leftPositions[1]:rightPositions[1]] == height1)
  position1 <- position1 + leftPositions[1]
  pos <- 2
  height2 <- max(signal[leftPositions[2]:rightPositions[2]])
  position2 <- which(signal[leftPositions[pos]:rightPositions[pos]] == height2)
  position2 <- position2 + leftPositions[pos]
  
  minRefractoryPeriod <- sFreq*0.2
  maxRefractoryPeriod <- sFreq *0.36
 
  while (((position2-position1) < minRefractoryPeriod) || (((position2-position1) > minRefractoryPeriod) && ((position2-position1) < maxRefractoryPeriod) && (derivative[position2]*2) < (derivative[position1])))
  {
    pos <- pos+1
    height2 <- max(signal[leftPositions[pos]:rightPositions[pos]])
    position2 <- which(signal[leftPositions[pos]:rightPositions[pos]] == height2)
    position2 <- position2+leftPositions[pos]
  }
  
  QRSWindow <- 100
  QRSWindowSamples <- trunc(QRSWindow*sFreq/1000)
  startPoint1 <- position1-QRSWindowSamples
  
  if (startPoint1 < 0)
  {
    startPoint1 <- 0
  }
  
  endPoint1 <- position1+QRSWindowSamples
  startPoint2 <- position2-QRSWindowSamples
  endPoint2 <- position2+QRSWindowSamples  
  height1 <- max(signal[startPoint1:endPoint1])
  position1 <- which(max(signal[startPoint1:endPoint1]) == height1)+startPoint1
  height2 <- max(signal[startPoint2:endPoint2])
  position2 <- which(max(signal[startPoint2:endPoint2]) == height2)+startPoint2
  heights <- c(height1, height2)
  positions <- c(position1, position2)
  twoFirstPeaks <- data.frame(positions, heights)
  return(twoFirstPeaks)
}

BeatDetectionPanTompkins <- function(ECGData, preProcessedSignal, channelLength, windowSize, channel, ...)
{
  # Performs the Pan & Tompkins QRS detection algorithm.
  # 
  # Args:
  #   ECGData: The structure where the ECG record is stored.
  #   preProcessedSignal: The signal processed and ready to beat detection.
  #   channelLength: The length of the channel which contents the signal.
  #   windowSize: The length of the window used in the moving window integrator step.
  #   channel: The channel where the preprocessing is applied.
  #
  # Returns:
  #  The data model with the detected QRS positions.
  #
  
  if (missing(ECGData))
  {
    stop("You must specify the ECGData parameter")
  }
  
  if (missing(preProcessedSignal))
  {
    stop("You must specify the preProcessedSignal parameter")
  }
  
  if (ECGData$Verbose == TRUE)
  {
    cat("   Applying Pan & Tompkins algorithm \n")
  }
  
  derAllPeaks <- PeakDetection(preProcessedSignal$derivative, ECGData$sFreq, windowSize = 222)
  derInitialThreshold <- RootMeanSquared(preProcessedSignal$derivative[1:2^16])

  derTwoFirstPeaks  <- TwoFirstPeaks(preProcessedSignal$derivative, derInitialThreshold, preProcessedSignal$derivative, ECGData$sFreq)
  
  mwiAllPeaks <- PeakDetection(preProcessedSignal$mwi, ECGData$sFreq, windowSize = 222)
  mwiInitialThreshold <- RootMeanSquared(preProcessedSignal$mwi[1:2^16])
  mwiTwoFirstPeaks <- TwoFirstPeaks(preProcessedSignal$mwi, mwiInitialThreshold, preProcessedSignal$derivative, ECGData$sFreq)
  
  derivative <- preProcessedSignal$derivative
  allPeaksArraySize <- length(derAllPeaks$positions)
  signalPeakPositions <- mat.or.vec(channelLength, 1)
  signalPeakHeights <- mat.or.vec(channelLength, 1)
  SPKI <- 0.0
  NPKI <- 0.0
  
  out <- .C("panTompkinsAlgorithm", as.double(derivative),
            as.integer(ECGData$sFreq),
            as.integer(derTwoFirstPeaks$positions),
            as.double(derTwoFirstPeaks$heights),
            as.double(derInitialThreshold),
            as.integer(derAllPeaks$positions),
            as.double(derAllPeaks$heights),
            as.integer(allPeaksArraySize),
            signalPeakPositions = as.integer(signalPeakPositions),
            signalPeakHeights = as.double(signalPeakHeights),
            SPKI = as.double(SPKI),
            NPKI = as.double(NPKI),
            as.integer(windowSize),
            channelNum = as.integer(channel))
  
  positions <- out$signalPeakPositions[which(out$signalPeakPositions != 0)]
  heights   <- out$signalPeakHeights[which(out$signalPeakPositions != 0)]
  derPeaks <- data.frame(positions, heights)
  
  allPeaksArraySize <- length(mwiAllPeaks$positions)
  signalPeakPositions <- mat.or.vec(channelLength, 1)
  signalPeakHeights <- mat.or.vec(channelLength, 1)
  SPKI <- 0.0
  NPKI <- 0.0
  
  out <- .C("panTompkinsAlgorithm", as.double(derivative),
            as.integer(ECGData$sFreq),
            as.integer(mwiTwoFirstPeaks$positions),
            as.double(mwiTwoFirstPeaks$heights),
            as.double(mwiInitialThreshold),
            as.integer(mwiAllPeaks$positions),
            as.double(mwiAllPeaks$heights),
            as.integer(allPeaksArraySize),
            signalPeakPositions = as.integer(signalPeakPositions),
            signalPeakHeights = as.double(signalPeakHeights),
            SPKI = as.double(SPKI),
            NPKI = as.double(NPKI),
            as.integer(windowSize),
            channelNum = as.integer(channel))
  
  positions <- out$signalPeakPositions[which(out$signalPeakPositions != 0)]
  heights <- out$signalPeakHeights[which(out$signalPeakPositions != 0)]
  mwiPeaks <- data.frame(positions, heights)
  
  #Discard the positions which are not in both signals.
  signalPeakPositions <- mat.or.vec(channelLength, 1)  
  signalPeakHeights   <- mat.or.vec(channelLength, 1)
  
  out <- .C("sameBeat", as.integer(mwiPeaks$positions),
            as.double(mwiPeaks$heights),
            as.integer(derPeaks$positions),
            as.double(derPeaks$heights),
            as.integer(length(mwiPeaks$positions)),
            as.integer(length(derPeaks$positions)),
            signalPeakPositions = as.integer(signalPeakPositions),
            signalPeakHeights = as.double(signalPeakHeights),
            as.integer(windowSize))
  
  positions <- out$signalPeakPositions[which(out$signalPeakPositions != 0)]
  heights <- out$signalPeakHeights[which(out$signalPeakPositions != 0)]
  
  if ( length(positions) != length(heights))
  {
    stop("Number of signal peak positions doesn't match with number of signal peak heights. There are", length(positions), "peak positions and", length(heights), "peak heights\n")
  }
  
  positions <- positions - 22 
  
  QRSWindow <- 120
  QRSWindowSamples <- trunc(ECGData$sFreq*QRSWindow/1000)
  
  for (i in 1:length(positions))
  {       
    startPoint <- positions[i]-QRSWindowSamples
    
    if (startPoint < 0)
    {
      startPoint <- 0
    }
    
    endPoint <- positions[i]+QRSWindowSamples 
    heights[i] <- max(ECGData$lead[[channel]]$val[startPoint:endPoint])
    positions[i] <- which(max(ECGData$lead[[channel]]$val[startPoint:endPoint]) == heights[i])[1] + positions[i]
  }
  
  #Save final QRS positions.
  qrsPeaks <- data.frame(positions, heights)
  return(qrsPeaks)    
}

QRSDetection <- function(ECGData, channel = 1, ...)
{
  # Performs the Pan & Tompkins QRS detection algorithm.
  # 
  # Args:
  #   ECGData: The structure where the ECG record is stored.
  #   channel: The channel where the detection is applied.
  #
  # Returns:
  #  The data model with the QRS positions information added.
  
  if (missing(ECGData))
  {
    stop("You must specify the ECGData parameter")
  }
  
  windowSize <- 150
  ECGData$sFreq <- as.integer(ECGData$sFreq)
  windowSamples <- trunc(ECGData$sFreq*windowSize)/1000
  
  channelLength <- length(ECGData$lead[[channel]]$val)
  preProcessedSignal <- BeatDetectionPreprocessor(ECGData, channel, windowSamples)
  peaks <- BeatDetectionPanTompkins(ECGData, preProcessedSignal, channelLength, windowSamples, channel)  
  beatInterval <- vector()
  for (j in 1:(length(peaks$positions)-1))
  {
    beatInterval[j] <- peaks$positions[j+1]-peaks$positions[j]
  }
  
  medianBeatInterval <- median(beatInterval) 
  
  k <- 1
  for (i in 2:length(peaks$positions))
  {
    if ((peaks$positions[i]-peaks$positions[i-1]) > (medianBeatInterval*0.4))
    {
      ECGData$lead[[channel]]$beat[[k]] <- list()
      ECGData$lead[[channel]]$beat[[k]]$QRS$peak <- peaks$positions[i]
      k <- k+1
    }
    else
    {
      next
    }

  }
  
  if (ECGData$Verbose)
  {
    cat("   QRS positions added to the ECGData structure\n")
  }
  
  return(ECGData)
}

QRSDetectionAllCh <- function(ECGData)
{
  # Performs the Pan & Tompkins QRS detection algorithm in all of the ECG channels.
  # 
  # Args:
  #   ECGData: The structure where the ECG record is stored.
  #
  # Returns:
  #  The data model with the QRS positions information added for all of the ECG channels.
  
  if (missing(ECGData))
  {
    stop("You must specify the ECGData parameter")
  }
  
  cat("\nPREPARING TO DETECT BEATS ON ALL OF THE CHANNELS OF THE RECORD\n")
  for (i in 1:ECGData$nLeads)
  {
    cat("\n-Detecting beats on the channel", i, "(", ECGData$lead[[i]]$id, ")\n")
    ECGData <- QRSDetection(ECGData, i)
  }
  
  cat("\nQRS positions added to all of the channels of the record\n\n")
  return(ECGData)
}
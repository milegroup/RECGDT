GetRRDistances <- function(ECGData)
{
  #Obtains the distances between beats
  #   
  # Args:
  #   ECGData: The structure with the signal.
  #
  # Returns:
  #   The distance in samples between R peaks. 
  
  RDist <- vector()
  
  for (i in 1:(length(ECGData$beat)-1))
  {
    RDist[i] <- ECGData$beat[[i+1]]$pos - ECGData$beat[[i]]$pos
  }
  
  ECGData$RRDistance <- mean(RDist)
  return(ECGData)
  
}

WaveletTransform <- function(ECGData, channel, beat)
{
  #Applies the wavelet transform to the signal in the first five scales
  #   
  # Args:
  #   ECGData: The structure with the signal.
  #   channel: The channel on the signal to obtain wavelet scales.
  #   beat: the number of the beat which basal is getting its wavelets.
  #
  # Returns:
  #   The wavelet transforms for the signal. 
  
  if (length(ECGData$beat[[beat]]$basalBeat[[channel]]$basalBeatSignal) < 1)
  {
    stop("Basal beat for the beat specified does not exist. You must obtain the basal beat first")
  }
  
  signal <- ECGData$beat[[beat]]$basalBeat[[channel]]$basalBeatSignal
  waveletTransform <- list()
  wavelets <- list()
  waveletTransform$W1 <- mat.or.vec(length(signal), 1)
  waveletTransform$S1 <- mat.or.vec(length(signal), 1)
  waveletTransform$W2 <- mat.or.vec(length(signal)/2, 1)
  waveletTransform$S2 <- mat.or.vec(length(signal)/2, 1)
  waveletTransform$W3 <- mat.or.vec(length(signal)/4, 1)
  waveletTransform$S3 <- mat.or.vec(length(signal)/4, 1)
  waveletTransform$W4 <- mat.or.vec(length(signal)/8, 1)
  waveletTransform$S4 <- mat.or.vec(length(signal)/8, 1)
  waveletTransform$W5 <- mat.or.vec(length(signal)/16, 1)
  waveletTransform$S5 <- mat.or.vec(length(signal)/16, 1)
  
  out <- .C("getQuadraticSplineWavelet250", as.double(signal),
            as.integer(length(signal)),
            W1 = as.double(waveletTransform$W1),
            S1 = as.double(waveletTransform$S1),
            W2 = as.double(waveletTransform$W2),
            S2 = as.double(waveletTransform$S2),
            W3 = as.double(waveletTransform$W3),
            S3 = as.double(waveletTransform$S3))
  
  wavelets$W1 <- out$W1
  wavelets$S1 <- out$S1
  wavelets$W2 <- out$W2
  wavelets$S2 <- out$S2
  wavelets$W3 <- out$W3
  wavelets$S3 <- out$S3
  
  return(wavelets)
}

QRSDelineator <- function(ECGData, channel, beat, wavelets)
{
  # Performs the QRS Complex delineation.
  # 
  # Args:
  #   ECGData: The structure with the beat where the QRS Complex is delineated.
  #   channel: The channel on the signal where the QRS delineation positions will be stored.
  #   beat: The number of the beat in which basal beat the QRS Complex is delineated.
  #   wavelets: The wavelet transforms for the data model signal. 
  #
  # Returns:
  #   The data model with the QRS Complex delineation information added.
  
  waveletScale1 <- wavelets$W1
  waveletScale2 <- wavelets$W2
  
  QRSPosition <- ECGData$beat[[beat]]$basalBeat[[channel]]$basalBeatRPeakPos
  QRSLength <- 0.12 * ECGData$sFreq
  intervalSize <- 50
  QRSOnset <- mat.or.vec(1, 1) 
  QRSOffset <- mat.or.vec(1, 1)
  QPeak <- mat.or.vec(1, 1)
  SPeak <- mat.or.vec(1, 1)
  
  out <- .C("delineateQRS", as.double(waveletScale1),
            as.double(waveletScale2),
            as.integer(length(waveletScale1)),
            as.integer(length(waveletScale2)), 
            as.integer(QRSPosition),
            as.integer(length(QRSPosition)),
            as.integer(QRSLength),
            as.integer(intervalSize), 
            QRSOnset = as.integer(QRSOnset),
            QRSOffset = as.integer(QRSOffset), 
            QPeak =  as.integer(QPeak),
            SPeak = as.integer(SPeak))
  
  if(ECGData$Verbose)
  {	
    cat("\n QRS complex, Q and S waves for the beat have been delineated")   
  }
  
  ECGData$beat[[beat]]$basalBeat[[channel]]$QRS$QRSOnset <- as.integer(out$QRSOnset)
  ECGData$beat[[beat]]$basalBeat[[channel]]$QRS$QRSOffset <- as.integer(out$QRSOffset)
  ECGData$beat[[beat]]$basalBeat[[channel]]$QRS$QPeak <- as.integer(out$QPeak)
  ECGData$beat[[beat]]$basalBeat[[channel]]$QRS$SPeak <- as.integer(out$SPeak)
 
  return(ECGData)
}

PWaveDelineator <- function(ECGData, channel, beat, wavelets)
{
  # Performs the P Wave delineation.
  # 
  # Args:
  #   ECGData: The structure with the beat where the P Wave is delineated.
  #   channel: The channel on the signal where the P Wave delineation positions will be stored.
  #   beat: The number of the beat in which basal beat the P Wave is delineated.
  #   wavelets: The wavelet transforms for the data model signal.
  #
  # Returns:
  #   The data model with the P Wave delineation information added.
  
  waveletScale3 <- wavelets$W3
  waveletScale4 <- wavelets$W4
  waveletScale5 <- wavelets$W5
  RPositions <- vector()
  
  for (i in 1:length(ECGData$beat))
  {
    RPositions[i] <- ECGData$beat[[i]]$pos
  }
  
  QRSOnset <-  ECGData$beat[[beat]]$basalBeat[[channel]]$QRS$QRSOnset
  RRArraySize <- 10
  RInterval <- RPositions[1:RRArraySize]
  RRMean <- mean(diff(RInterval, 1))
  onsetPos <- mat.or.vec(1, 1)
  offsetPos <- mat.or.vec(1, 1)
  PPosition <- mat.or.vec(1, 1)
  basalBeatLength <- length(ECGData$beat[[beat]]$basalBeat[[channel]]$basalBeatSignal)-200
  
  out <- .C("delineatePWave", as.double(waveletScale3),
            as.double(waveletScale4),
            as.double(waveletScale5),
            as.integer(length(waveletScale3)),
            as.integer(length(waveletScale4)), 
            as.integer(length(waveletScale5)),
            as.integer(RPositions),
            as.integer(length(RPositions)),
            as.integer(QRSOnset), 
            as.integer(RRMean),
            as.integer(RRArraySize),
            onsetPos = as.integer(onsetPos),
            offsetPos = as.integer(offsetPos),
            PPosition = as.integer(PPosition),
            basalBeatLength = as.integer(basalBeatLength))
  
  if(ECGData$Verbose)
  {	
    cat("\n P Wave for the beat has been delineated:", "P Onset -", out$onsetPos[which(out$onsetPos != 0)], " // P Peak -", out$PPosition[which(out$PPosition != 0)], " // P Offset -", out$offsetPos[which(out$offsetPos != 0)], "\n")   
  }
  
  ECGData$beat[[beat]]$basalBeat[[channel]]$PWave$POnset <- out$onsetPos[which(out$onsetPos != 0)]
  ECGData$beat[[beat]]$basalBeat[[channel]]$PWave$POffset <- out$offsetPos[which(out$offsetPos != 0)]
  ECGData$beat[[beat]]$basalBeat[[channel]]$PWave$PPeak <- out$PPosition[which(out$PPosition != 0)]
  
  if(is.na(ECGData$beat[[beat]]$basalBeat[[channel]]$PWave$POnset[1]))
  {
    ECGData$beat[[beat]]$basalBeat[[channel]]$PWave$POnset <- 0
  }
  
  if(is.na(ECGData$beat[[beat]]$basalBeat[[channel]]$PWave$PPeak[1]))
  {
    ECGData$beat[[beat]]$basalBeat[[channel]]$PWave$PPeak <- 0
  }
  
  if(is.na(ECGData$beat[[beat]]$basalBeat[[channel]]$PWave$POffset[1]))
  {
    ECGData$beat[[beat]]$basalBeat[[channel]]$PWave$POffset <- 0
  }
  
  return(ECGData)
}

TWaveDelineator <- function(ECGData, channel, beat, wavelets)
{
  # Performs the T Wave delineation.
  # 
  # Args:
  #   ECGData: The structure with the beat where the T Wave is delineated.
  #   channel: The channel on the signal where the T Wave delineation positions will be stored.
  #   beat: The number of the beat in which basal beat the T Wave is delineated.
  #   wavelets: The wavelet transforms for the data model signal. 
  #
  # Returns:
  #   The data model with the T Wave delineation information added.
  
  waveletScale3 <- wavelets$W3
  waveletScale4 <- wavelets$W4
  waveletScale5 <- wavelets$W5
  RPositions <- vector()
  
  for (i in 1:length(ECGData$beat))
  {
    RPositions[i] <- ECGData$beat[[i]]$pos
  }
  
  QRSOnset <- ECGData$beat[[beat]]$basalBeat[[channel]]$QRS$QRSOnset
  QRSOffset <- ECGData$beat[[beat]]$basalBeat[[channel]]$QRS$QRSOffset
  RRArraySize <- 10
  RInterval <- RPositions[1:RRArraySize]
  RRMean <- mean(diff(RInterval, 1))
  onsetPos <- mat.or.vec(1, 1)
  offsetPos <- mat.or.vec(1, 1)
  TPosition <- mat.or.vec(1, 1)
  
  basalBeatLength <- length(ECGData$beat[[beat]]$basalBeat[[channel]]$basalBeatSignal)-200
  
  out <- .C("delineateTWave", as.double(waveletScale3),
            as.double(waveletScale4),
            as.double(waveletScale5),
            as.integer(length(waveletScale3)),
            as.integer(length(waveletScale4)), 
            as.integer(length(waveletScale5)),
            as.integer(RPositions),
            as.integer(length(RPositions)),
            as.integer(QRSOnset), 
            as.integer(QRSOffset),
            as.integer(RRMean),
            as.integer(RRArraySize),
            onsetPos = as.integer(onsetPos),
            offsetPos = as.integer(offsetPos),
            TPosition = as.integer(TPosition),
            basalBeatLength = as.integer(basalBeatLength))
  
  if(ECGData$Verbose)
  {	
    cat("\n T Wave for the beat has been delineated:", "T Onset -", out$onsetPos[which(out$onsetPos != 0)], " // T Peak -", out$TPosition[which(out$TPosition != 0)], " // T Offset -", out$offsetPos[which(out$offsetPos != 0)], "\n")   
  }
  
  ECGData$beat[[beat]]$basalBeat[[channel]]$TWave$TOnset <- out$onsetPos[which(out$onsetPos != 0)]	
  ECGData$beat[[beat]]$basalBeat[[channel]]$TWave$TOffset <- out$offsetPos[which(out$offsetPos != 0)]	
  ECGData$beat[[beat]]$basalBeat[[channel]]$TWave$TPeak <- out$TPosition[which(out$TPosition != 0)]
  
  if(is.na(ECGData$beat[[beat]]$basalBeat[[channel]]$TWave$TOnset[1]))
  {
    ECGData$beat[[beat]]$basalBeat[[channel]]$TWave$TOnset <- 0
  }
  
  if(is.na(ECGData$beat[[beat]]$basalBeat[[channel]]$TWave$TPeak[1]))
  {
    ECGData$beat[[beat]]$basalBeat[[channel]]$TWave$TPeak <- 0
  }
  
  if(is.na(ECGData$beat[[beat]]$basalBeat[[channel]]$TWave$TOffset[1]))
  {
    ECGData$beat[[beat]]$basalBeat[[channel]]$TWave$TOffset <- 0
  }
  
  return(ECGData)
}

ECGDelineator <- function(ECGData, channel, beat)
{
  # Performs the beat delineation.
  # 
  # Args:
  #   ECGData: The structure with the beat to be delineated.
  #   channel: The channel on the signal where the beat will be delineated.
  #   beat: The number of the beat to be delineated.
  #
  # Returns:
  #   The data model with the delineation information added.
  
  if (missing(ECGData))
  {
    stop("You must specify the ECGData parameter")
  }
  
  if (missing(beat))
  {
    stop("You must specify the beat parameter")
  }
  
  #Obtaining global beats positions in case they haven´t been obtained previously
  
  if (length(ECGData$beat) < 1)
  {
    cat("Global beats positions for the record not found. Obtaining them...\n")
    ECGData <- GlobalBeats(ECGData)
    cat("Global beats positions obtained\n")
    cat("A basal beat created by the user for the specified beat not found. Obtaining adapted basal beat\n")
    
    ECGData <- AdaptedBasalBeat(ECGData, channel, beat, 5)

    cat("Basal beat created\n")
    
  }
  
  #Creating basal beat in case it doesn´t exist
  
  if (length(ECGData$beat[[beat]]$basalBeat[[channel]]$basalBeatSignal) < 1)
  {
    cat("A basal beat created by the user for the specified beat not found. Obtaining adapted basal beat\n")
    
    ECGData <- AdaptedBasalBeat(ECGData, channel, beat, 5)

    cat("Basal beat created\n")
    
  }
  
  basalNoise <- rep(1, 200)
  originalBasalBeat <- ECGData$beat[[beat]]$basalBeat[[channel]]$basalBeatSignal
  ECGData$beat[[beat]]$basalBeat[[channel]]$basalBeatSignal <- append(ECGData$beat[[beat]]$basalBeat[[channel]]$basalBeatSignal, basalNoise)
  wavelets <- WaveletTransform(ECGData, channel, beat)
  
  for (i in 1:length(wavelets$W4))
  {
    wavelets$W4 <- wavelets$W3
  }

  for (i in 1:length(wavelets$S4))
  {
    wavelets$S4 <- wavelets$S3 
  }

  for (i in 1:length(wavelets$W5))
  {
    wavelets$W5 <- wavelets$W3 
  }

  for (i in 1:length(wavelets$S5))
  {
    wavelets$S5 <- wavelets$S3
  }
  
  ECGData <- QRSDelineator(ECGData, channel, beat, wavelets)
  ECGData <- PWaveDelineator(ECGData, channel, beat, wavelets)
  ECGData <- TWaveDelineator(ECGData, channel, beat, wavelets)
  
  if(length(ECGData$beat[[beat]]$basalBeat[[channel]]$QRS$QPeak) > 0 && length(ECGData$beat[[beat]]$basalBeat[[channel]]$QRS$SPeak) > 0)
  {
    maximo <- as.numeric(max(abs(ECGData$beat[[beat]]$basalBeat[[channel]]$basalBeatSignal[ECGData$beat[[beat]]$basalBeat[[channel]]$QRS$QPeak:ECGData$beat[[beat]]$basalBeat[[channel]]$QRS$SPeak])))
    posicion <- which(as.numeric(abs(ECGData$beat[[beat]]$basalBeat[[channel]]$basalBeatSignal)) == maximo)
    ECGData$beat[[beat]]$basalBeat[[channel]]$basalBeatRPeakPos <- posicion
  }
  
  ECGData$beat[[beat]]$basalBeat[[channel]]$basalBeatSignal <- originalBasalBeat

  return(ECGData)
}

PlotDetection <- function(ECGData, nChannel = 1, nSamplesPlot = 5000, ...)
{
  #Plots the QRS positions in the ECG signal.
  #   
  # Args:
  #   ECGData: The structure where the peaks positions and heights are stored.
  #   nChannel: which channel of the signal will be plotted.
  #   nSamplesPlot: Number of samples to plot in each iteration.
  #
  
  if (missing(ECGData))
  {
    stop("You must specify the ECGData parameter")
  }
  
  position <- 1  
  beatPositions <- c()
  plot.new()
  
  #Plots continuously in red the ECG signal
  while (position < length(ECGData$lead[[nChannel]]$val))
  {
    for (j in 1:length(ECGData$lead[[nChannel]]$beat))
    {
      beatPositions[j] <- ECGData$lead[[nChannel]]$beat[[j]]$QRS$peak
    }
    
    annotationPositions <- beatPositions[which(beatPositions >= position & beatPositions <= (position + nSamplesPlot - 1))]
    plot(position:(position + nSamplesPlot), ECGData$lead[[nChannel]]$val[position:(position + nSamplesPlot)], col = "firebrick", lwd = 1.85, type = "l", main = paste("Plotting detected beats on the record:", ECGData$id, "/ Channel:", ECGData$lead[[nChannel]]$id), xlab = "ECG signal samples", ylab = "Values in mV")
    abline(h = 0, col = "dimgrey")
    
    #Highlights the positions of the detected beats in green
    for (i in 1:length(annotationPositions))
    {
      abline(v = annotationPositions[i], col = "darkolivegreen3", lwd = 2.75)
    }
    
    Sys.sleep(1.5)
    cat("Showing", nSamplesPlot, "of", ECGData$nSamples, "samples: Samples", position, "to", position+nSamplesPlot, "\n")
    position <- position + nSamplesPlot
  }
}

PlotDetectionSegment <- function(ECGData, nChannel = 1, nSampleStart, nSampleEnd, ...)
{
  #Plots the QRS positions in a segment of the ECG signal.
  #   
  # Args:
  #   ECGData: The structure where the ECG record is stored.
  #   nChannel: which channel of the signal will be plotted.
  #   nSampleStart: The number of the first sample to plot.
  #   nSampleEnd: The number of the last sample to plot.
  #
  
  if (missing(ECGData))
  {
    stop("You must specify the ECGData parameter")
  }
  
  beatPositions <- c()
  plot.new()
  
  if(nSampleStart <= nSampleEnd)
  {
    position <- nSampleStart
    endPoint <- nSampleEnd
  }
  else
  {
    position <- nSampleEnd
    endPoint <- nSampleStart
  }
  
  if(endPoint - position > 10000)
  {
    stop("You must specify values of start and end points that differ less than 10.000 samples")
  }
  
  if(endPoint - position < 10)
  {
    stop("You must specify values of start and end points that differ more than 10 samples")
  }
  
  #Plots in red the segment of the ECG signal between the positions position and endPoint
  for (j in 1:length(ECGData$lead[[nChannel]]$beat))
  {
    beatPositions[j] <- ECGData$lead[[nChannel]]$beat[[j]]$QRS$peak
  }
  
  annotationPositions <- beatPositions[which(beatPositions >= position & beatPositions <= endPoint)]
  plot(position:endPoint, ECGData$lead[[nChannel]]$val[position:endPoint], col = "firebrick", lwd = 1.85, type = "l", main = paste("Plotting detected beats on the record:", ECGData$id, "/ Channel:", ECGData$lead[[nChannel]]$id), xlab = "ECG signal samples", ylab = "Values in mV")
  abline(h = 0, col = "dimgrey")
  cat("Number of beats detected on this segment:", length(annotationPositions), "\n")
  
  #Highlights the positions of the detected beats in green  
  for (i in 1:length(annotationPositions))
  {
    cat("Position of the beat", i, "on this segment:", annotationPositions[i], "\n")
    abline(v = annotationPositions[i], col = "darkolivegreen3", lwd = 2.75)
  }
  
  if (nChannel == 1 || nChannel == 2 || nChannel == 3 || nChannel == 6 || nChannel == 10 || nChannel == 11 || nChannel == 12)
  {
    legend("topright", legend = "Detected mono-channel beats", col = "darkolivegreen3", cex = 1.05, bg = "gray88", box.lwd = 0,  lwd = 2.75)
  }
  else
  {
    if (nChannel == 4 || nChannel == 5 || nChannel == 7 || nChannel == 8 || nChannel == 9)
    {
      legend("bottomright", legend = "Detected mono-channel beats", col = "darkolivegreen3", cex = 1.05, bg = "gray88", box.lwd = 0,  lwd = 2.75)
    }
  }
  
  cat("Showing samples from", position, "to", endPoint, "( of", ECGData$nSamples, ")\n")
}

PlotGlobalDetection <- function(ECGData, nChannel = 1, nSamplesPlot = 5000, ...)
{
  #Plots the Global QRS positions in the ECG signal.
  #   
  # Args:
  #   ECGData: The structure where the peaks positions and heights are stored.
  #   nChannel: which channel of the signal will be plotted.
  #   nSamplesPlot: Number of samples to plot in each iteration.
  #
  
  if (missing(ECGData))
  {
    stop("You must specify the ECGData parameter")
  }
  
  position <- 1  
  beatPositions <- c()
  plot.new()
  
  #Plots continuously in red the ECG signal
  while (position < length(ECGData$lead[[nChannel]]$val))
  {
    for (j in 1:length(ECGData$beat))
    {
      beatPositions[j] <- ECGData$beat[[j]]$pos
    }
    
    annotationPositions <- beatPositions[which(beatPositions >= position & beatPositions <= (position + nSamplesPlot - 1))]
    plot(position:(position + nSamplesPlot), ECGData$lead[[nChannel]]$val[position:(position + nSamplesPlot)], col = "firebrick", lwd = 1.85, type = "l", main = paste("Plotting global beats on the record:", ECGData$id, "/ Channel:", ECGData$lead[[nChannel]]$id), xlab = "ECG signal samples", ylab = "Values in mV")
    abline(h = 0, col = "dimgrey")
    
    #Highlights the positions of the detected beats in green
    for (i in 1:length(annotationPositions))
    {
      abline(v = annotationPositions[i], col = "deepskyblue4", lwd = 2.25, lty = 6)
    }
    
    Sys.sleep(0.5)
    cat("Showing", nSamplesPlot, "of", ECGData$nSamples, "samples: Samples", position, "to", position+nSamplesPlot, "\n")
    position <- position + nSamplesPlot
  }
}

PlotGlobalDetectionSegment <- function(ECGData, nChannel = 1, nSampleStart, nSampleEnd, ...)
{
  #Plots the Global QRS positions in a specified segment of the ECG signal.
  #   
  # Args:
  #   ECGData: The structure where the ECG record is stored.
  #   nChannel: which channel of the signal will be plotted.
  #   nSampleStart: The number of the first sample to plot.
  #   nSampleEnd: The number of the last sample to plot.
  #
  
  if (missing(ECGData))
  {
    stop("You must specify the ECGData parameter")
  }
  
  beatPositions <- c()
  plot.new()
  
  if(nSampleStart <= nSampleEnd)
  {
    position <- nSampleStart
    endPoint <- nSampleEnd
  }
  else
  {
    position <- nSampleEnd
    endPoint <- nSampleStart
  }
  
  if(endPoint - position > 10000)
  {
    stop("You must specify values of start and end points that differ less than 10.000 samples")
  }
  
  if(endPoint - position < 10)
  {
    stop("You must specify values of start and end points that differ more than 10 samples")
  }
  
  #Plots in red the segment of the ECG signal between the positions position and endPoint
  for (j in 1:length(ECGData$beat))
  {
    beatPositions[j] <- ECGData$beat[[j]]$pos
  }
  
  annotationPositions <- beatPositions[which(beatPositions >= position & beatPositions <= endPoint)]
  plot(position:endPoint, ECGData$lead[[nChannel]]$val[position:endPoint], col = "firebrick", lwd = 1.85, type = "l", main = paste("Plotting global beats on the record:", ECGData$id, "/ Channel:", ECGData$lead[[nChannel]]$id), xlab = "ECG signal samples", ylab = "Values in mV")
  abline(h = 0, col = "dimgrey")
  cat("Number of beats detected on this segment:", length(annotationPositions), "\n")
  
  #Highlights the positions of the detected beats in green  
  for (i in 1:length(annotationPositions))
  {
    cat("Position of the beat", i, "on this segment:", annotationPositions[i], "\n")
    abline(v = annotationPositions[i], col = "deepskyblue4", lwd = 2.25, lty = 6)
  }
  
  if (nChannel == 1 || nChannel == 2 || nChannel == 3 || nChannel == 6 || nChannel == 10 || nChannel == 11 || nChannel == 12)
  {
    legend("topright", legend = "Detected global beats", col = "deepskyblue4", cex = 1.05, bg = "gray88", box.lwd = 0,  lwd = 2.25, lty = 6)
  }
  else
  {
    if (nChannel == 4 || nChannel == 5 || nChannel == 7 || nChannel == 8 || nChannel == 9)
    {
      legend("bottomright", legend = "Detected global beats", col = "deepskyblue4", cex = 1.05, bg = "gray88", box.lwd = 0,  lwd = 2.25, lty = 6)
    }
  }
  
  cat("Showing samples from", position, "to", endPoint, "( of", ECGData$nSamples, ")\n")
}

PlotDetectionAvg <- function(ECGData, nChannel = 1)
{
  #Plots the QRS positions in the ECG signal plus the mean beat per every 15 beats.
  #   
  # Args:
  #   ECGData: The structure where the peaks positions and heights are stored.
  #   nChannel: which channel of the signal will be plotted.
  #
  
  if (missing(ECGData))
  {
    stop("You must specify the ECGData parameter")
  }
  
  position <- 1  
  plot.new()
  beatLength <- c()
  avgBeat <- c()
  
  for (l in 2:length(ECGData$lead[[nChannel]]$beat)-1)
  {
    beatLength[l] <- ECGData$lead[[nChannel]]$beat[[l+1]]$QRS$peak-ECGData$lead[[nChannel]]$beat[[l]]$QRS$peak
  }
  
  meanLength <- mean(beatLength)    
  k <- 1
  
  while (k <= length(ECGData$lead[[nChannel]]$beat))
  {
    for (i in 1:meanLength)
    {     
      added <- 0
      
      for (j in k:(k+14))
      {
        added <- added+ECGData$lead[[nChannel]]$val[(ECGData$lead[[nChannel]]$beat[[j]]$QRS$peak)+i-1]
      }
      
      avgBeat[i] <- round(added/15)
    }
    
    avgBeatWindow <- rep(avgBeat, 15)
    x <- 1:length(avgBeatWindow)
    y1 <- avgBeatWindow[1:length(avgBeatWindow)]
    y2 <- ECGData$lead[[nChannel]]$val[ECGData$lead[[nChannel]]$beat[[k]]$QRS$peak:(ECGData$lead[[nChannel]]$beat[[k]]$QRS$peak+length(avgBeatWindow)-1)]
    minimum <- min(y2)
    maximum <- max(y2)
    #Plots continuously in grey the mean beat for the beats represented on the plot on each moment
    plot(x, y1, col = "azure4", ylim = c(minimum, maximum), type = "l", lwd = 2, main = paste("Mean beat of record:", ECGData$id, "/ Channel:", ECGData$lead[[nChannel]]$id), xlab = "Samples per beat", ylab = "Values in mV")
    #Plots continuously in red the ECG signal
    points(x, y2, col = "indianred4", type = "l")
    k <- k+15
    Sys.sleep(0.5)
  }
}

PlotDetectionAvg30 <- function(ECGData, nChannel = 1)
{
  #Plots the QRS positions in the ECG signal plus the mean beat per every 30 beats (one beat, the previous 15 and the next 14).
  #   
  # Args:
  #   ECGData: The structure where the peaks positions and heights are stored.
  #   nChannel: which channel of the signal will be plotted.
  #
  
  if (missing(ECGData))
  {
    stop("You must specify the ECGData parameter")
  }
  
  position <- 1  
  plot.new()
  beatLength <- c()
  avgBeat <- c()
  
  for (l in 2:length(ECGData$lead[[nChannel]]$beat)-1)
  {
    beatLength[l] <- ECGData$lead[[nChannel]]$beat[[l+1]]$QRS$peak-ECGData$lead[[nChannel]]$beat[[l]]$QRS$peak
  }
  
  meanLength <- mean(beatLength)    
  k <- 16
  
  while (k <= length(ECGData$lead[[nChannel]]$beat))
  {
    for (i in 1:meanLength)
    {     
      added <- 0
      
      for (j in (k-15):(k+14))
      {
        added <- added+ECGData$lead[[nChannel]]$val[(ECGData$lead[[nChannel]]$beat[[j]]$QRS$peak)+i-1]
      }
      
      avgBeat[i] <- round(added/30)
    }
    
    avgBeatWindow <- rep(avgBeat, 30)
    x <- 1:length(avgBeatWindow)
    y1 <- avgBeatWindow[1:length(avgBeatWindow)]
    y2 <- ECGData$lead[[nChannel]]$val[ECGData$lead[[nChannel]]$beat[[k]]$QRS$peak:(ECGData$lead[[nChannel]]$beat[[k]]$QRS$peak+length(avgBeatWindow)-1)]
    minimum <- min(y2)
    maximum <- max(y2)
    #Plots continuously in grey the mean beat for the beats represented on the plot on each moment
    plot(x, y1, col = "azure4", ylim = c(minimum, maximum), type = "l", lwd = 2, main = paste("Mean beat of record:", ECGData$id, "/ Channel:", ECGData$lead[[nChannel]]$id), xlab = "Samples per beat", ylab = "Values in mV")
    #Plots continuously in red the ECG signal
    points(x, y2, col = "indianred4", type = "l")
    k <- k+30
    Sys.sleep(0.5)
  }
}

PlotBasalBeat <- function(ECGData, nChannel = 1, nBeat)
{
  #Plots the basal beat of the current one plus the real beat in the ECG signal.
  #   
  # Args:
  #   ECGData: The structure where the peaks positions and heights are stored.
  #   nChannel: which channel of the signal will be plotted.
  #   nBeat: which beat of the channel will be plotted with its basal beat.
  #
  
  if (missing(ECGData))
  {
    stop("You must specify the ECGData parameter")
  }
  
  if (missing(nBeat))
  {
    stop("You must specify the nBeat parameter")
  }
  
  position <- 1  
  plot.new()
  
  x <- 1:length(ECGData$beat[[nBeat]]$basalBeat[[nChannel]]$basalBeatSignal)
  y1 <- ECGData$beat[[nBeat]]$basalBeat[[nChannel]]$avgBeat[1:length(ECGData$beat[[nBeat]]$basalBeat[[nChannel]]$avgBeat)]
  y2 <- ECGData$beat[[nBeat]]$DetrendPlot
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
  #Plots in red the real beat from the ECG signal
  points(x, y1, col = "gray42", type = "l", lwd = 4.25)
  points(x[RPeak], y1[RPeak], col = "darkred", pch = 13, lwd = 2.75, cex = 1.7)
  text(RPeak, ECGData$beat[[nBeat]]$basalBeat[[nChannel]]$basalBeatSignal[RPeak]+20, labels = "R Peak", cex = 0.8, font = 2, col= "darkred")
  legend("topright", xjust = 10, yjust = 10, legend = c("Basal beat", "Original beat from signal"), col = c("gray42", "indianred4"), cex = 1.15, bg = "gray88", box.lwd = 0,  lwd = 2.75:2)

}

PlotBasalSignal <- function(ECGData, nChannel = 1, basalSignal)
{
  #Plots the basal signal combined with the real one.
  #   
  # Args:
  #   ECGData: The structure where the peaks positions and heights are stored.
  #   nChannel: which channel of the signal will be plotted.
  #   basalSignal: basal signal to be plotted.
  #
  
  if (missing(ECGData))
  {
    stop("You must specify the ECGData parameter")
  }
  
  position <- 1  
  plot.new()
  
  while (position < length(ECGData$lead[[nChannel]]$val))
  {
    y1 <- basalSignal[position:(position+600)]
    cat(length(y1), "\n")
    y2 <- ECGData$lead[[nChannel]]$val[position:(position+600)]
    cat(length(y2), "\n")
    meany2 <- mean(y2)
    meany1 <- mean(y1)
    diffMean <- meany2 - meany1
    
    for (i in 1:length(y1))
    {
      y1[i] <- y1[i] + diffMean
    }
    
    x <- 1:601
    minimum <- min(y2)
    maximum <- max(y2)
    #Plots continuously in grey the mean beat for the beats represented on the plot on each moment
    plot(x, y1, col = "azure4", ylim = c(minimum, maximum), type = "l", lwd = 2, main = paste("Mean beat of record:", ECGData$id, "/ Channel:", ECGData$lead[[nChannel]]$id), xlab = "Samples per beat", ylab = "Values in mV")
    #Plots continuously in red the ECG signal
    points(x, y2, col = "indianred4", type = "l")
    position <- position+600
    Sys.sleep(0.5)
  }
}

PlotMonoMultiDetectionSegment <- function(ECGData, nChannel = 1, nSampleStart, nSampleEnd, ...)
{
  #Plots the QRS positions in a segment of the ECG signal from monochannel and multichannel detection.
  #   
  # Args:
  #   ECGData: The structure where the ECG record is stored.
  #   nChannel: which channel of the signal will be plotted.
  #   nSampleStart: The number of the first sample to plot.
  #   nSampleEnd: The number of the last sample to plot.
  #
  
  if (missing(ECGData))
  {
    stop("You must specify the ECGData parameter")
  }
  
  beatPositions <- c()
  beatGlobalPositions <- c()
  plot.new()
  
  if(nSampleStart <= nSampleEnd)
  {
    position <- nSampleStart
    endPoint <- nSampleEnd
  }
  else
  {
    position <- nSampleEnd
    endPoint <- nSampleStart
  }
  
  if(endPoint - position > 10000)
  {
    stop("You must specify values of start and end points that differ less than 10.000 samples")
  }
  
  if(endPoint - position < 10)
  {
    stop("You must specify values of start and end points that differ more than 10 samples")
  }
  
  #Plots in red the segment of the ECG signal between the positions position and endPoint
  
  for (j in 1:length(ECGData$lead[[nChannel]]$beat))
  {
    beatPositions[j] <- ECGData$lead[[nChannel]]$beat[[j]]$QRS$peak
  }
  
  for (k in 1:length(ECGData$beat))
  {
    beatGlobalPositions[k] <- ECGData$beat[[k]]$pos
  }
  
  annotationPositions <- beatPositions[which(beatPositions >= position & beatPositions <= endPoint)]
  annotationGlobalPositions <- beatGlobalPositions[which(beatGlobalPositions >= position & beatGlobalPositions <= endPoint)]
  plot(position:endPoint, ECGData$lead[[nChannel]]$val[position:endPoint], col = "firebrick", lwd = 1.85, type = "l", main = paste("Plotting detected & global detected beats on the record:", ECGData$id, "/ Channel:", ECGData$lead[[nChannel]]$id), xlab = "ECG signal samples", ylab = "Values in mV")
  abline(h = 0, col = "dimgrey")
  cat("Number of beats detected on this segment:", length(annotationPositions), "\n")
  cat("Number of global beats detected on this segment:", length(annotationGlobalPositions), "\n")
  
  #Highlights the positions of the detected beats in green  
  for (h in 1:length(annotationPositions))
  {
    cat("Position of the beat", h, "on this segment:", annotationPositions[h], "\n")
    abline(v = annotationPositions[h], col = "darkolivegreen3", lwd = 2.75)
  }
  
  #Highlights the positions of the global detected beats in blue  
  for (i in 1:length(annotationGlobalPositions))
  {
    cat("Position of the global beat", i, "on this segment:", annotationGlobalPositions[i], "\n")
    abline(v = annotationGlobalPositions[i], col = "deepskyblue4", lwd = 2.25, lty = 6)
  }
  
  if (nChannel == 1 || nChannel == 2 || nChannel == 3 || nChannel == 6 || nChannel == 10 || nChannel == 11 || nChannel == 12)
  {
    legend("topright", legend = c("Detected mono-channel beats", "Detected global beats"), col = c("darkolivegreen3", "deepskyblue4"), cex = 1.05, bg = "gray88", box.lwd = 0,  lwd = 2.75:2.25, lty = 1:6)
  }
  else
  {
    if (nChannel == 4 || nChannel == 5 || nChannel == 7 || nChannel == 8 || nChannel == 9)
    {
      legend("bottomright", legend = c("Detected mono-channel beats", "Detected global beats"), col = c("darkolivegreen3", "deepskyblue4"), cex = 1.05, bg = "gray88", box.lwd = 0,  lwd = 2.75:2.25, lty = 1:6)
    }
  }
  
  cat("Showing samples from", position, "to", endPoint, "( of", ECGData$nSamples, ")\n")
  
}

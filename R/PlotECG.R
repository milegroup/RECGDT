PlotECG <- function(ECGData, nChannel = 1, nSamplesPlot = 5000, ...)
{
  #Plots the ECG signal
  #   
  # Args:
  #   ECGData: The structure where the ECG record is stored.
  #   nChannel: which channel of the signal will be plotted.
  #   nSamplesPlot: Number of samples to plot in each iteration.
  #
  
  if (missing(ECGData))
  {
    stop("You must specify the ECGData parameter")
  }
  
  position <- 1  
  plot.new()
  
  # Plots continuously the ECG signal
  while (position < length(ECGData$lead[[nChannel]]$val))
  {
    plot(position:(position + nSamplesPlot), ECGData$lead[[nChannel]]$val[position:(position + nSamplesPlot)], col = "firebrick", lwd = 1.85, type = "l", main = paste("Plotted record:", ECGData$id, "/ Channel:", ECGData$lead[[nChannel]]$id), xlab = "ECG signal samples", ylab = "Values in mV")
    abline(h = 0, col = "dimgrey")
    Sys.sleep(1.5)
    cat("Showing", nSamplesPlot, "of", ECGData$nSamples, "samples: Samples", position, "to", position+nSamplesPlot, "\n")
    position <- position + nSamplesPlot
  }
}

PlotECGSegment <- function(ECGData, nChannel = 1, nSampleStart, nSampleEnd, ...)
{
  #Plots a specified segment of the ECG signal
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
  
  plot(position:endPoint, ECGData$lead[[nChannel]]$val[position:endPoint], col = "firebrick", lwd = 1.85, type = "l", main = paste("Plotted record:", ECGData$id, "/ Channel:", ECGData$lead[[nChannel]]$id), xlab = "ECG signal samples", ylab = "Values in mV")
  abline(h = 0, col = "dimgrey")
  cat("Showing samples from", position, "to", endPoint, "( of", ECGData$nSamples, ")\n")
}

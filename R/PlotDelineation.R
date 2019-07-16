PlotDelineation <- function(ECGData, channel, nBeat)
{
  #Plots the basal beat of the current one plus its highlighted P Wave, QRS Complex and T Wave.
  #   
  # Args:
  #   ECGData: The structure where the peaks positions and heights are stored.
  #   channel: which channel to look for the delineated positions.
  #   nBeat: which beat of the channel will have its basal beat plotted with its waves delineated.
  #
  
  if (missing(ECGData))
  {
    stop("You must specify the ECGData parameter")
  }
  
  if (missing(nBeat))
  {
    stop("You must specify the nBeat parameter")
  }
  
  if (missing(channel))
  {
    stop("You must specify the channel parameter")
  }
  
  position <- 1  
  plot.new()
  
  x <- 1:length(ECGData$beat[[nBeat]]$basalBeat[[channel]]$basalBeatSignal)
  y <- ECGData$beat[[nBeat]]$basalBeat[[channel]]$basalBeatSignal[1:length(ECGData$beat[[nBeat]]$basalBeat[[channel]]$basalBeatSignal)]
  yPWave <- ECGData$beat[[nBeat]]$basalBeat[[channel]]$basalBeatSignal[ECGData$beat[[nBeat]]$basalBeat[[channel]]$PWave$POnset[1]:ECGData$beat[[nBeat]]$basalBeat[[channel]]$PWave$POffset[1]]
  yQRS <- ECGData$beat[[nBeat]]$basalBeat[[channel]]$basalBeatSignal[ECGData$beat[[nBeat]]$basalBeat[[channel]]$QRS$QRSOnset[1]:ECGData$beat[[nBeat]]$basalBeat[[channel]]$QRS$QRSOffset[1]]
  yTWave <- ECGData$beat[[nBeat]]$basalBeat[[channel]]$basalBeatSignal[ECGData$beat[[nBeat]]$basalBeat[[channel]]$TWave$TOnset[1]:ECGData$beat[[nBeat]]$basalBeat[[channel]]$TWave$TOffset[1]]
  RPeak <- ECGData$beat[[nBeat]]$basalBeat[[channel]]$basalBeatRPeakPos
  minimum <- min(y)-25
  maximum <- max(y)+25
  
  plot(x, y, type = "l", col = "azure4", ylim = c(minimum, maximum), lwd = 2, main = paste("QRS complex, P and T Waves delineated on the beat", nBeat, "\nof the record:", ECGData$id), xlab = "Samples per beat", ylab = "Values in mV")
  lines(x[ECGData$beat[[nBeat]]$basalBeat[[channel]]$PWave$POnset[1]:ECGData$beat[[nBeat]]$basalBeat[[channel]]$PWave$POffset[1]], yPWave, type="l", col="mediumseagreen", cex=2, lwd = 4)
  lines(x[ECGData$beat[[nBeat]]$basalBeat[[channel]]$QRS$QRSOnset[1]:ECGData$beat[[nBeat]]$basalBeat[[channel]]$QRS$QRSOffset[1]], yQRS, type="l", col="firebrick2", cex=2, lwd = 4)
  lines(x[ECGData$beat[[nBeat]]$basalBeat[[channel]]$TWave$TOnset[1]:ECGData$beat[[nBeat]]$basalBeat[[channel]]$TWave$TOffset[1]], yTWave, type="l", col="dodgerblue2", cex=2, lwd = 4)
  points(x[ECGData$beat[[nBeat]]$basalBeat[[channel]]$PWave$PPeak[1]], y[ECGData$beat[[nBeat]]$basalBeat[[channel]]$PWave$PPeak[1]], col = "darkgreen", pch = 13, lwd = 2.75, cex = 1.7)
  text(ECGData$beat[[nBeat]]$basalBeat[[channel]]$PWave$PPeak[1], ECGData$beat[[nBeat]]$basalBeat[[channel]]$basalBeatSignal[ECGData$beat[[nBeat]]$basalBeat[[channel]]$PWave$PPeak[1]]+20, labels = "P Peak", cex = 0.8, font = 2, col= "darkgreen")
  points(x[ECGData$beat[[nBeat]]$basalBeat[[channel]]$QRS$QPeak[1]], y[ECGData$beat[[nBeat]]$basalBeat[[channel]]$QRS$QPeak[1]], col = "darkred", pch = 13, lwd = 2.75, cex = 1.7)
  text(ECGData$beat[[nBeat]]$basalBeat[[channel]]$QRS$QPeak[1], ECGData$beat[[nBeat]]$basalBeat[[channel]]$basalBeatSignal[ECGData$beat[[nBeat]]$basalBeat[[channel]]$QRS$QPeak[1]]+20, labels = "Q Peak", cex = 0.8, font = 2, col= "darkred")
  points(x[RPeak], y[RPeak], col = "darkred", pch = 13, lwd = 2.75, cex = 1.7)
  text(RPeak, ECGData$beat[[nBeat]]$basalBeat[[channel]]$basalBeatSignal[RPeak]+20, labels = "R Peak", cex = 0.8, font = 2, col= "darkred")
  points(x[ECGData$beat[[nBeat]]$basalBeat[[channel]]$QRS$SPeak[1]], y[ECGData$beat[[nBeat]]$basalBeat[[channel]]$QRS$SPeak[1]], col = "darkred", pch = 13, lwd = 2.75, cex = 1.7)
  text(ECGData$beat[[nBeat]]$basalBeat[[channel]]$QRS$SPeak[1], ECGData$beat[[nBeat]]$basalBeat[[channel]]$basalBeatSignal[ECGData$beat[[nBeat]]$basalBeat[[channel]]$QRS$SPeak[1]]+20, labels = "S Peak", cex = 0.8, font = 2, col= "darkred")
  points(x[ECGData$beat[[nBeat]]$basalBeat[[channel]]$TWave$TPeak[1]], y[ECGData$beat[[nBeat]]$basalBeat[[channel]]$TWave$TPeak[1]], col = "dodgerblue4", pch = 13, lwd = 2.75, cex = 1.7)
  text(ECGData$beat[[nBeat]]$basalBeat[[channel]]$TWave$TPeak[1],  ECGData$beat[[nBeat]]$basalBeat[[channel]]$basalBeatSignal[ECGData$beat[[nBeat]]$basalBeat[[channel]]$TWave$TPeak[1]]+20, labels = "T Peak", cex = 0.8, font = 2, col= "dodgerblue4")
  legend("topright", legend = c("Basal beat","P Wave", "QRS Complex", "T Wave"), col = c("azure4", "mediumseagreen", "firebrick2", "dodgerblue2"), cex = 1.15, bg = "gray88", box.lwd = 0,  lwd = 2)

}

PlotDelineationPDF <- function(ECGData, channel, nBeat, recordName)
{
  #Plots the basal beat of the current one plus its highlighted P Wave, QRS Complex and T Wave, and exports the result to a PDF file.
  #   
  # Args:
  #   ECGData: The structure where the peaks positions and heights are stored.
  #   channel: which channel to look for the delineated positions.
  #   nBeat: which beat of the channel will have its basal beat plotted with its waves delineated.
  #   recordName: record id to name the pdf file
  #
  
  if (missing(ECGData))
  {
    stop("You must specify the ECGData parameter")
  }
  
  if (missing(nBeat))
  {
    stop("You must specify the nBeat parameter")
  }
  
  if (missing(channel))
  {
    stop("You must specify the channel parameter")
  }
  
  if (missing(recordName))
  {
    stop("You must specify the recordName parameter")
  }
  
  position <- 1  
  plot.new()
  
  x <- 1:length(ECGData$beat[[nBeat]]$basalBeat[[channel]]$basalBeatSignal)
  y <- ECGData$beat[[nBeat]]$basalBeat[[channel]]$basalBeatSignal[1:length(ECGData$beat[[nBeat]]$basalBeat[[channel]]$basalBeatSignal)]
  yPWave <- ECGData$beat[[nBeat]]$basalBeat[[channel]]$basalBeatSignal[ECGData$beat[[nBeat]]$basalBeat[[channel]]$PWave$POnset[1]:ECGData$beat[[nBeat]]$basalBeat[[channel]]$PWave$POffset[1]]
  yQRS <- ECGData$beat[[nBeat]]$basalBeat[[channel]]$basalBeatSignal[ECGData$beat[[nBeat]]$basalBeat[[channel]]$QRS$QRSOnset[1]:ECGData$beat[[nBeat]]$basalBeat[[channel]]$QRS$QRSOffset[1]]
  yTWave <- ECGData$beat[[nBeat]]$basalBeat[[channel]]$basalBeatSignal[ECGData$beat[[nBeat]]$basalBeat[[channel]]$TWave$TOnset[1]:ECGData$beat[[nBeat]]$basalBeat[[channel]]$TWave$TOffset[1]]
  RPeak <- ECGData$beat[[nBeat]]$basalBeat[[channel]]$basalBeatRPeakPos
  minimum <- min(y)-25
  maximum <- max(y)+25
  
  outputFile <- paste('/home/vmvisunha/Escritorio/ECGTesis/Plots/Delineation/', recordName, '.pdf', sep = "")
  pdf(outputFile, width = 9, height = 5)
  plot(x, y, type = "l", col = "azure4", ylim = c(minimum, maximum), lwd = 2, main = paste("QRS complex, P and T Waves delineated on the beat", nBeat, "\nof the record:", ECGData$id), xlab = "Samples per beat", ylab = "Values in mV")
  lines(x[ECGData$beat[[nBeat]]$basalBeat[[channel]]$PWave$POnset[1]:ECGData$beat[[nBeat]]$basalBeat[[channel]]$PWave$POffset[1]], yPWave, type="l", col="mediumseagreen", cex=2, lwd = 4)
  lines(x[ECGData$beat[[nBeat]]$basalBeat[[channel]]$QRS$QRSOnset[1]:ECGData$beat[[nBeat]]$basalBeat[[channel]]$QRS$QRSOffset[1]], yQRS, type="l", col="firebrick2", cex=2, lwd = 4)
  lines(x[ECGData$beat[[nBeat]]$basalBeat[[channel]]$TWave$TOnset[1]:ECGData$beat[[nBeat]]$basalBeat[[channel]]$TWave$TOffset[1]], yTWave, type="l", col="dodgerblue2", cex=2, lwd = 4)
  points(x[ECGData$beat[[nBeat]]$basalBeat[[channel]]$PWave$PPeak[1]], y[ECGData$beat[[nBeat]]$basalBeat[[channel]]$PWave$PPeak[1]], col = "darkgreen", pch = 13, lwd = 2.75, cex = 1.7)
  text(ECGData$beat[[nBeat]]$basalBeat[[channel]]$PWave$PPeak[1], ECGData$beat[[nBeat]]$basalBeat[[channel]]$basalBeatSignal[ECGData$beat[[nBeat]]$basalBeat[[channel]]$PWave$PPeak[1]]+20, labels = "P Peak", cex = 0.8, font = 2, col= "darkgreen")
  points(x[ECGData$beat[[nBeat]]$basalBeat[[channel]]$QRS$QPeak[1]], y[ECGData$beat[[nBeat]]$basalBeat[[channel]]$QRS$QPeak[1]], col = "darkred", pch = 13, lwd = 2.75, cex = 1.7)
  text(ECGData$beat[[nBeat]]$basalBeat[[channel]]$QRS$QPeak[1], ECGData$beat[[nBeat]]$basalBeat[[channel]]$basalBeatSignal[ECGData$beat[[nBeat]]$basalBeat[[channel]]$QRS$QPeak[1]]+20, labels = "Q Peak", cex = 0.8, font = 2, col= "darkred")
  points(x[RPeak], y[RPeak], col = "darkred", pch = 13, lwd = 2.75, cex = 1.7)
  text(RPeak, ECGData$beat[[nBeat]]$basalBeat[[channel]]$basalBeatSignal[RPeak]+20, labels = "R Peak", cex = 0.8, font = 2, col= "darkred")
  points(x[ECGData$beat[[nBeat]]$basalBeat[[channel]]$QRS$SPeak[1]], y[ECGData$beat[[nBeat]]$basalBeat[[channel]]$QRS$SPeak[1]], col = "darkred", pch = 13, lwd = 2.75, cex = 1.7)
  text(ECGData$beat[[nBeat]]$basalBeat[[channel]]$QRS$SPeak[1], ECGData$beat[[nBeat]]$basalBeat[[channel]]$basalBeatSignal[ECGData$beat[[nBeat]]$basalBeat[[channel]]$QRS$SPeak[1]]+20, labels = "S Peak", cex = 0.8, font = 2, col= "darkred")
  points(x[ECGData$beat[[nBeat]]$basalBeat[[channel]]$TWave$TPeak[1]], y[ECGData$beat[[nBeat]]$basalBeat[[channel]]$TWave$TPeak[1]], col = "dodgerblue4", pch = 13, lwd = 2.75, cex = 1.7)
  text(ECGData$beat[[nBeat]]$basalBeat[[channel]]$TWave$TPeak[1],  ECGData$beat[[nBeat]]$basalBeat[[channel]]$basalBeatSignal[ECGData$beat[[nBeat]]$basalBeat[[channel]]$TWave$TPeak[1]]+20, labels = "T Peak", cex = 0.8, font = 2, col= "dodgerblue4")
  legend("topright", legend = c("P Wave highlighted in green, QRS complex in red and T Wave in blue"), text.font=3, cex=0.75, xjust = 30, yjust = 30)
  dev.off()
  
}

PlotReferenceDelineation <- function(ECGData, nBeat, referenceAnnotationFile, referenceAnnotationPath=".")
{
  #Plots the basal beat of the current one plus its highlighted Reference P Wave, QRS Complex and T Wave.
  #   
  # Args:
  #   ECGData: The structure where the basal beat is stored.
  #   nBeat: which beat of the channel will have its basal beat plotted with its waves delineated.
  #   referenceAnnotationFile: The file where the reference annotations are stored.
  #   referenceAnnotationPath: The path to the file where the reference annotations are stored.
  #
  
  if (missing(ECGData))
  {
    stop("You must specify the ECGData parameter")
  }
  
  if (missing(nBeat))
  {
    stop("You must specify the nBeat parameter")
  }
  
  if (missing(referenceAnnotationFile))
  {
    stop("You must specify the referenceAnnotationFile parameter")
  }
  
  if (missing(referenceAnnotationPath))
  {
    stop("You must specify the referenceAnnotationPath parameter")
  }
  
  dir = getwd()
  on.exit(setwd(dir))
  setwd(referenceAnnotationPath)
  
  position <- 1
  signalRPeak <- ECGData$beat[[nBeat]]$pos
  plot.new()
  
  x <- 1:length(ECGData$beat[[nBeat]]$basalBeat[[channel]]$basalBeatSignal)
  y <- ECGData$beat[[nBeat]]$basalBeat[[channel]]$basalBeatSignal[1:length(ECGData$beat[[nBeat]]$basalBeat[[channel]]$basalBeatSignal)]
  
  cat("Reading the annotation file", referenceAnnotationFile, "from the path", referenceAnnotationPath, "\n")
  
  referenceAnnotationReading <- read.table(referenceAnnotationFile, sep = "", skip=0, fill=TRUE)
  
  referenceAnnotationPositions = referenceAnnotationReading$V2
  referenceAnnotationTypes = referenceAnnotationReading$V3
  
  referencePOnset <- -10
  referencePPeak <- -10
  referencePOffset <- -10
  referenceQRSOnset <- -10
  referenceRPeak <- -10
  referenceQRSOffset <- -10
  referenceTOnset <- -10
  referenceTPeak <- -10
  referenceTOffset <- -10
  
  for (i in 1:length(referenceAnnotationPositions))
  {
    
    if((abs(referenceAnnotationPositions[i]-signalRPeak) < 50) && (referenceAnnotationTypes[i] == "N"))
    {
      referencePOnset <- referenceAnnotationPositions[i-4]
      referencePPeak <- referenceAnnotationPositions[i-3]
      referencePOffset <- referenceAnnotationPositions[i-2]
      referenceQRSOnset <- referenceAnnotationPositions[i-1]
      referenceQRSPeak <- referenceAnnotationPositions[i]
      referenceQRSOffset <- referenceAnnotationPositions[i+1]
      referenceTOnset <- referenceAnnotationPositions[i+2]
      referenceTPeak <- referenceAnnotationPositions[i+3]
      
      if(referenceAnnotationTypes[i+4] == "t")
      {
        referenceTOffset <- referenceAnnotationPositions[i+5]
      }
      else
      {
        referenceTOffset <- referenceAnnotationPositions[i+4]
      }
    }
  }
  
  basalPOnset <- abs(ECGData$beat[[nBeat]]$basalBeat[[channel]]$basalBeatRPeakPos-(signalRPeak-referencePOnset))
  basalPPeak <- abs(ECGData$beat[[nBeat]]$basalBeat[[channel]]$basalBeatRPeakPos-(signalRPeak-referencePPeak))
  basalPOffset <- abs(ECGData$beat[[nBeat]]$basalBeat[[channel]]$basalBeatRPeakPos-(signalRPeak-referencePOffset))
  basalQRSOnset <- abs(ECGData$beat[[nBeat]]$basalBeat[[channel]]$basalBeatRPeakPos-(signalRPeak-referenceQRSOnset))
  basalRPeak <- abs(ECGData$beat[[nBeat]]$basalBeat[[channel]]$basalBeatRPeakPos-(signalRPeak-referenceRPeak))
  basalQRSOffset <- abs(ECGData$beat[[nBeat]]$basalBeat[[channel]]$basalBeatRPeakPos+(referenceQRSOffset-signalRPeak))
  basalTOnset <- abs(ECGData$beat[[nBeat]]$basalBeat[[channel]]$basalBeatRPeakPos+(referenceTOnset-signalRPeak))
  basalTPeak <-abs(ECGData$beat[[nBeat]]$basalBeat[[channel]]$basalBeatRPeakPos+(referenceTPeak-signalRPeak))
  basalTOffset <- abs(ECGData$beat[[nBeat]]$basalBeat[[channel]]$basalBeatRPeakPos+(referenceTOffset-signalRPeak))
  
  yPWave <- ECGData$beat[[nBeat]]$basalBeat[[channel]]$basalBeatSignal[basalPOnset:basalPOffset]
  yQRS <- ECGData$beat[[nBeat]]$basalBeat[[channel]]$basalBeatSignal[basalQRSOnset:basalQRSOffset]
  yTWave <- ECGData$beat[[nBeat]]$basalBeat[[channel]]$basalBeatSignal[basalTOnset:basalTOffset]
  
  RPeak <- basalRPeak
  minimum <- min(y)-25
  maximum <- max(y)+25
  
  plot(x, y, type = "l", col = "azure4", ylim = c(minimum, maximum), lwd = 2, main = paste("QRS complex, P and T Waves reference positions on the beat", nBeat, "\nof the record:", ECGData$id), xlab = "Samples per beat", ylab = "Values in mV")
  lines(x[basalPOnset:basalPOffset], yPWave, type="l", col ="mediumseagreen", cex=2, lwd = 4)
  lines(x[basalQRSOnset:basalQRSOffset], yQRS, type="l", col ="firebrick2", cex=2, lwd = 4)
  lines(x[basalTOnset:basalTOffset], yTWave, type="l", col ="dodgerblue2", cex=2, lwd = 4)
  points(x[basalPPeak], y[basalPPeak], col = "darkgreen", pch = 13, lwd = 2.75, cex = 1.7)
  text(basalPPeak, basalPPeak+20, labels = "P Peak", cex = 0.8, font = 2, col = "darkgreen")
  points(x[basalRPeak], y[basalRPeak], col = "darkred", pch = 13, lwd = 2.75, cex = 1.7)
  text(basalRPeak, basalRPeak+20, labels = "QRS Peak", cex = 0.8, font = 2, col = "darkred")
  points(x[basalTPeak], y[basalTPeak], col = "dodgerblue4", pch = 13, lwd = 2.75, cex = 1.7)
  text(basalTPeak, basalTPeak+20, labels = "T Peak", cex = 0.8, font = 2, col = "dodgerblue4")
  legend("topright", legend = c("P Wave highlighted in green, QRS complex in red and T Wave in blue"), text.font=3, cex=0.75, xjust = 30, yjust = 30)
  
}

PlotReferenceDelineationRealSignal <- function(ECGData, nBeat, referenceAnnotationFile, referenceAnnotationPath=".")
{
  #Plots the signal segment of the current beat plus its highlighted Reference P Wave, QRS Complex and T Wave.
  #   
  # Args:
  #   ECGData: The structure where the signal is stored.
  #   nBeat: which beat of the channel will have its signal segment plotted with its waves delineated.
  #   referenceAnnotationFile: The file where the reference annotations are stored.
  #   referenceAnnotationPath: The path to the file where the reference annotations are stored.
  #
  
  if (missing(ECGData))
  {
    stop("You must specify the ECGData parameter")
  }
  
  if (missing(nBeat))
  {
    stop("You must specify the nBeat parameter")
  }
  
  if (missing(referenceAnnotationFile))
  {
    stop("You must specify the referenceAnnotationFile parameter")
  }
  
  if (missing(referenceAnnotationPath))
  {
    stop("You must specify the referenceAnnotationPath parameter")
  }
  
  dir = getwd()
  on.exit(setwd(dir))
  setwd(referenceAnnotationPath)
  
  position <- 1
  signalRPeak <- ECGData$beat[[nBeat]]$pos
  plot.new()
  
  cat("Reading the annotation file", referenceAnnotationFile, "from the path", referenceAnnotationPath, "\n")
  
  referenceAnnotationReading <- read.table(referenceAnnotationFile, sep = "", skip=0)
  
  referenceAnnotationPositions = referenceAnnotationReading$V2
  referenceAnnotationTypes = referenceAnnotationReading$V3
  
  referencePOnset <- -10
  referencePPeak <- -10
  referencePOffset <- -10
  referenceQRSOnset <- -10
  referenceQRSOffset <- -10
  referenceTOnset <- -10
  referenceTPeak <- -10
  referenceTOffset <- -10
  
  for (i in 1:length(referenceAnnotationPositions))
  {
    
    if((abs(referenceAnnotationPositions[i]-signalRPeak) < 50) && (referenceAnnotationTypes[i] == "N"))
    {
      referencePOnset <- referenceAnnotationPositions[i-4]
      referencePPeak <- referenceAnnotationPositions[i-3]
      referencePOffset <- referenceAnnotationPositions[i-2]
      referenceQRSOnset <- referenceAnnotationPositions[i-1]
      referenceQRSPeak <- referenceAnnotationPositions[i]
      referenceQRSOffset <- referenceAnnotationPositions[i+1]
      referenceTOnset <- referenceAnnotationPositions[i+2]
      referenceTPeak <- referenceAnnotationPositions[i+3]
      
      if(referenceAnnotationTypes[i+4] == "t")
      {
        referenceTOffset <- referenceAnnotationPositions[i+5]
      }
      else
      {
        referenceTOffset <- referenceAnnotationPositions[i+4]
      }
    }
  }
  
  firstPos <- (signalRPeak-ECGData$beat[[nBeat]]$basalBeat[[channel]]$basalBeatRPeakPos)
  lastPos <- firstPos+length(ECGData$beat[[nBeat]]$basalBeat[[channel]]$basalBeatSignal)
  x <- firstPos:lastPos
  y <- ECGData$lead[[1]]$val[firstPos:lastPos]
  yPWave <- ECGData$lead[[1]]$val[referencePOnset:referencePOffset]
  yQRS <- ECGData$lead[[1]]$val[referenceQRSOnset:referenceQRSOffset]
  yTWave <- ECGData$lead[[1]]$val[referenceTOnset:referenceTOffset]
  minimum <- min(y)-25
  maximum <- max(y)+25
  
  plot(x, y, type = "l", col = "azure4", ylim = c(minimum, maximum), lwd = 2, main = paste("QRS complex, P and T Waves reference positions on the beat", nBeat, "\nof the record:", ECGData$id), xlab = "Samples per beat", ylab = "Values in mV")
  lines(x[referencePOnset:referencePOffset], yPWave, type="l", col ="mediumseagreen", cex=2, lwd = 4)
  lines(x[referenceQRSOnset:referenceQRSOffset], yQRS, type="l", col ="firebrick2", cex=2, lwd = 4)
  lines(x[referenceTOnset:referenceTOffset], yTWave, type="l", col ="dodgerblue2", cex=2, lwd = 4)
  points(x[referencePPeak], y[referencePPeak], col = "darkgreen", pch = 13, lwd = 2.75, cex = 1.7)
  text(referencePPeak, referencePPeak+20, labels = "P Peak", cex = 0.8, font = 2, col = "darkgreen")
  points(x[referenceQRSPeak], y[referenceQRSPeak], col = "darkred", pch = 13, lwd = 2.75, cex = 1.7)
  text(referenceQRSPeak, referenceQRSPeak+20, labels = "QRS Peak", cex = 0.8, font = 2, col = "darkred")
  points(x[referenceTPeak], y[referenceTPeak], col = "dodgerblue4", pch = 13, lwd = 2.75, cex = 1.7)
  text(referenceTPeak, referenceTPeak+20, labels = "T Peak", cex = 0.8, font = 2, col = "dodgerblue4")
  legend("topright", legend = c("P Wave highlighted in green, QRS complex in red and T Wave in blue"), text.font=3, cex=0.75, xjust = 30, yjust = 30)

}

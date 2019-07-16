ExportDelineation <- function(ECGData, channel, nBeat, fileName, path =".")
{
  # Saves the delineation positions in an ASCII file (txt file)
  # 
  # Args:
  #   ECGData: Data model containing the beat delineation positions.
  #   channel: which channel of the record has the beat positions to export.
  #   nBeat: which beat of the signal will have its positions exported.
  #   fileName: Name of the file to be written.
  #   path: Where to save the file.
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
  
  if (missing(fileName))
  {
    stop("You must specify the fileName parameter")
  }
  
  POnsetSignal <- -10
  PPeakSignal <- -10
  POffsetSignal <- -10
  QRSOnsetSignal <- -10
  QPeakSignal <- -10
  SPeakSignal <- -10
  QRSOffsetSignal <- -10
  TOnsetSignal <- -10
  TPeakSignal <- -10
  TOffsetSignal <- -10
  
  POnsetSignal <- ECGData$beat[[nBeat]]$pos-(ECGData$beat[[nBeat]]$basalBeat[[channel]]$basalBeatRPeakPos-ECGData$beat[[nBeat]]$basalBeat[[channel]]$PWave$POnset)
  PPeakSignal <- ECGData$beat[[nBeat]]$pos-(ECGData$beat[[nBeat]]$basalBeat[[channel]]$basalBeatRPeakPos-ECGData$beat[[nBeat]]$basalBeat[[channel]]$PWave$PPeak)
  POffsetSignal <- ECGData$beat[[nBeat]]$pos-(ECGData$beat[[nBeat]]$basalBeat[[channel]]$basalBeatRPeakPos-ECGData$beat[[nBeat]]$basalBeat[[channel]]$PWave$POffset)
  QRSOnsetSignal <- ECGData$beat[[nBeat]]$pos-(ECGData$beat[[nBeat]]$basalBeat[[channel]]$basalBeatRPeakPos-ECGData$beat[[nBeat]]$basalBeat[[channel]]$QRS$QRSOnset)
  QPeakSignal <- ECGData$beat[[nBeat]]$pos-(ECGData$beat[[nBeat]]$basalBeat[[channel]]$basalBeatRPeakPos-ECGData$beat[[nBeat]]$basalBeat[[channel]]$QRS$QPeak)
  SPeakSignal <- ECGData$beat[[nBeat]]$pos+(ECGData$beat[[nBeat]]$basalBeat[[channel]]$QRS$SPeak-ECGData$beat[[nBeat]]$basalBeat[[channel]]$basalBeatRPeakPos)
  QRSOffsetSignal <- ECGData$beat[[nBeat]]$pos+(ECGData$beat[[nBeat]]$basalBeat[[channel]]$QRS$QRSOffset-ECGData$beat[[nBeat]]$basalBeat[[channel]]$basalBeatRPeakPos) 
  TOnsetSignal <- ECGData$beat[[nBeat]]$pos+(ECGData$beat[[nBeat]]$basalBeat[[channel]]$TWave$TOnset-ECGData$beat[[nBeat]]$basalBeat[[channel]]$basalBeatRPeakPos)
  TPeakSignal <- ECGData$beat[[nBeat]]$pos+(ECGData$beat[[nBeat]]$basalBeat[[channel]]$TWave$TPeak-ECGData$beat[[nBeat]]$basalBeat[[channel]]$basalBeatRPeakPos)
  TOffsetSignal <- ECGData$beat[[nBeat]]$pos+(ECGData$beat[[nBeat]]$basalBeat[[channel]]$TWave$TOffset-ECGData$beat[[nBeat]]$basalBeat[[channel]]$basalBeatRPeakPos)
  
  extension <- ".annd"
  
  #Concatenates the path and file name
  outputFile <- paste(path, fileName, "_ch", channel, extension, sep = "")
  
  if (ECGData$Verbose)
  {
    message <- paste("Saving beat waves positions in the file", outputFile, "\n", sep=" ")
    cat(message)
  }
  
  beatPositions <- data.frame()
  beatPositions <- cat("--- Wave positions for the beat", nBeat, "of the record", ECGData$id, "in signal, channel", channel, "---", file = outputFile, sep = " ", append = TRUE)
  beatPositions <- cat("\n\nP Wave: ", file = outputFile, sep = "\n", append = TRUE)
  beatPositions <- cat("\n\tP Onset: ", POnsetSignal, file = outputFile, sep = "\n\t\t\t\t", append = TRUE)
  beatPositions <- cat("\tP Peak: ", PPeakSignal, file = outputFile, sep = "\n\t\t\t\t", append = TRUE)
  beatPositions <- cat("\tP Offset: ", POffsetSignal, file = outputFile, sep = "\n\t\t\t\t", append = TRUE)
  beatPositions <- cat("\n\nQRS Complex: ", file = outputFile, sep = "\n", append = TRUE)
  beatPositions <- cat("\n\tQRS Onset: ", QRSOnsetSignal, file = outputFile, sep = "\n\t\t\t\t", append = TRUE)
  beatPositions <- cat("\tQ Peak: ", QPeakSignal, file = outputFile, sep = "\n\t\t\t\t", append = TRUE)
  beatPositions <- cat("\tR Peak: ", ECGData$beat[[nBeat]]$pos, file = outputFile, sep = "\n\t\t\t\t", append = TRUE)
  beatPositions <- cat("\tS Peak: ", SPeakSignal, file = outputFile, sep = "\n\t\t\t\t", append = TRUE)
  beatPositions <- cat("\tQRS Offset: ", QRSOffsetSignal, file = outputFile, sep = "\n\t\t\t\t", append = TRUE)
  beatPositions <- cat("\n\nT Wave: ", file = outputFile, sep = "\n", append = TRUE)
  beatPositions <- cat("\n\tT Onset: ", TOnsetSignal, file = outputFile, sep = "\n\t\t\t\t", append = TRUE)
  beatPositions <- cat("\tT Peak: ", TPeakSignal, file = outputFile, sep = "\n\t\t\t\t", append = TRUE)
  beatPositions <- cat("\tT Offset: ", TOffsetSignal, file = outputFile, sep = "\n\t\t\t\t", append = TRUE)
  beatPositions <- cat("\n\n\t\t\t\t\t\t\t\t---\t\t\t\t\t\t\t\t\n\n", file = outputFile, sep = " ", append = TRUE)
  beatPositions <- cat("--- Wave positions for the beat", nBeat, "of the record", ECGData$id, "in basal beat, channel", channel, "---", file = outputFile, sep = " ", append = TRUE)
  beatPositions <- cat("\n\nP Wave: ", file = outputFile, sep = "\n", append = TRUE)
  beatPositions <- cat("\n\tP Onset: ", ECGData$beat[[nBeat]]$basalBeat[[channel]]$PWave$POnset, file = outputFile, sep = "\n\t\t\t\t", append = TRUE)
  beatPositions <- cat("\tP Peak: ", ECGData$beat[[nBeat]]$basalBeat[[channel]]$PWave$PPeak, file = outputFile, sep = "\n\t\t\t\t", append = TRUE)
  beatPositions <- cat("\tP Offset: ", ECGData$beat[[nBeat]]$basalBeat[[channel]]$PWave$POffset, file = outputFile, sep = "\n\t\t\t\t", append = TRUE)
  beatPositions <- cat("\n\nQRS Complex: ", file = outputFile, sep = "\n", append = TRUE)
  beatPositions <- cat("\n\tQRS Onset: ", ECGData$beat[[nBeat]]$basalBeat[[channel]]$QRS$QRSOnset, file = outputFile, sep = "\n\t\t\t\t", append = TRUE)
  beatPositions <- cat("\tQ Peak: ", ECGData$beat[[nBeat]]$basalBeat[[channel]]$QRS$QPeak, file = outputFile, sep = "\n\t\t\t\t", append = TRUE)
  beatPositions <- cat("\tR Peak: ", ECGData$beat[[nBeat]]$basalBeat[[channel]]$basalBeatRPeakPos, file = outputFile, sep = "\n\t\t\t\t", append = TRUE)
  beatPositions <- cat("\tS Peak: ", ECGData$beat[[nBeat]]$basalBeat[[channel]]$QRS$SPeak, file = outputFile, sep = "\n\t\t\t\t", append = TRUE)
  beatPositions <- cat("\tQRS Offset: ", ECGData$beat[[nBeat]]$basalBeat[[channel]]$QRS$QRSOffset, file = outputFile, sep = "\n\t\t\t\t", append = TRUE)
  beatPositions <- cat("\n\nT Wave: ", file = outputFile, sep = "\n", append = TRUE)
  beatPositions <- cat("\n\tT Onset: ", ECGData$beat[[nBeat]]$basalBeat[[channel]]$TWave$TOnset, file = outputFile, sep = "\n\t\t\t\t", append = TRUE)
  beatPositions <- cat("\tT Peak: ", ECGData$beat[[nBeat]]$basalBeat[[channel]]$TWave$TPeak, file = outputFile, sep = "\n\t\t\t\t", append = TRUE)
  beatPositions <- cat("\tT Offset: ", ECGData$beat[[nBeat]]$basalBeat[[channel]]$TWave$TOffset, file = outputFile, sep = "\n\t\t\t\t", append = TRUE)
  write.table(beatPositions, file = outputFile, col.names = FALSE, quote = FALSE, sep = "", append = TRUE)
}


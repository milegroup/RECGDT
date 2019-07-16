ExportBeats <- function(ECGData, nChannel, fileName, path =".")
{
  # Saves the QRS positions in an ASCII file (txt file)
  # 
  # Args:
  #   ECGData: Data model containing QRS positions.
  #   nChannel: which channel of the signal will have its beats exported.
  #   fileName: Name of the file to be written.
  #   path: Where to save the file.
  #
  
  if (missing(ECGData))
  {
    stop("You must specify the ECGData parameter")
  }
  
  if (missing(fileName))
  {
    stop("You must specify the fileName parameter")
  }
  
  #Concatenates the path and file name
  outputFile <- paste(path, fileName, sep = "")
  
  if (ECGData$Verbose)
  {
    message <- paste("Saving beats data in the file", outputFile, "\n", sep=" ")
    cat(message)
  }
  
  beatValues <- data.frame()
  
  for (i in 1:length(ECGData$lead[[nChannel]]$beat))
  {
    beatValues <- cat(ECGData$lead[[nChannel]]$beat[[i]]$QRS$peak, file = outputFile, sep = "\n", append = TRUE)
  }
  
  write.table(beatValues, file = outputFile, col.names = FALSE, quote = FALSE, sep = "", append = TRUE)
}

ExportBeatsAllCh <- function(ECGData, fileName, path =".")
{
  # Saves the QRS positions of all of the ECG channels in ASCII files (txt files)
  # 
  # Args:
  #   ECGData: Data model containing QRS positions.
  #   fileName: Root of the names of the files to be written.
  #   path: Where to save the files.
  #
  
  if (missing(ECGData))
  {
    stop("You must specify the ECGData parameter")
  }
  
  cat("\nPREPARING TO EXPORT BEATS DETECTED ON ALL OF THE CHANNELS OF THE RECORD\n")
  
  for (i in 1:ECGData$nLeads)
  {
    fileChannelName <- paste(fileName, "_", i, sep="")
    cat("\n-Exporting beats detected on the channel", i, "(", ECGData$lead[[i]]$id, ")\n")
    ExportBeats(ECGData, i, fileChannelName, path)
  }
  
  cat("\nBeats positions saved in files for all of the channels of the record\n")
}

ExportGlobalBeats <- function(ECGData, fileName, path =".")
{
  # Saves the QRS positions in an ASCII file (txt file)
  # 
  # Args:
  #   ECGData: Data model where the beats were detected.
  #   fileName: Name of the file to be written.
  #   path: Where to save the file.
  #
  
  if (missing(ECGData))
  {
    stop("You must specify the ECGData parameter")
  }
  
  if (missing(fileName))
  {
    stop("You must specify the fileName parameter")
  }
  
  beatValues <- data.frame()
  
  cat("\nPREPARING TO EXPORT GLOBAL DETECTED BEATS\n")
  
  for (i in 1:ECGData$nLeads)
  {
    fileChannelName <- paste(fileName, "_", i, sep="")
    outputFile <- paste(path, fileChannelName, sep="")
    cat("\n-Exporting global detected beats to the channel", i, "(", ECGData$lead[[i]]$id, ")\n")
    
    for (j in 1:length(ECGData$beat))
    {
      beatValues <- cat(ECGData$beat[[j]]$pos, file = outputFile, sep = "\n", append = TRUE)
    }
    
    write.table(beatValues, file = outputFile, col.names = FALSE, quote = FALSE, sep = "", append = TRUE)
  }

}
ExportECG <- function(ECGData, fileName, path =".")
{
  # Saves the QRS positions in an ASCII file (txt file)
  # 
  # Args:
  #   ECGData: Data model containing QRS positions.
  #   fileName: Name of the file to be written.
  #   path: Where to save the file.
  #
  # HEADER:
  #
  #<Header>
  #  id=...
  #  sFreq=...
  #  nLeads=...
  #  nSamples=...
  #  channels=...
  #  gender=...
  #  conditions=...
  #</Header>
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
    message <- paste("Saving ECG data in the file", outputFile, "\n", sep=" ")
    cat(message)
  }
  
  channelNames <- ""
  for (i in 1:ECGData$nLeads)
  {
    if (ECGData$lead[[i]]$id != " ")
    {
      channelNames <- paste(channelNames, ECGData$lead[[i]]$id, ",", sep = "")
    }
    else
    {
      channelNames <- c("Ch. Num. ", i, sep = ",")
    }
  }
  
  #Save the header into the ASCII file 
  cat("<Header>", file = outputFile, sep = "\n", append = TRUE)
  cat("  id=", ECGData$id, file = outputFile, sep = "", append = TRUE)
  cat("", file = outputFile, sep = "\n", append = TRUE)
  cat("  sFreq=", ECGData$sFreq, file = outputFile, sep = "", append = TRUE)
  cat("", file = outputFile, sep = "\n", append = TRUE)
  cat("  nLeads=", ECGData$nLeads, file = outputFile, sep = "", append = TRUE)
  cat("", file = outputFile, sep = "\n", append = TRUE)
  cat("  nSamples=", ECGData$nSamples, file = outputFile, sep = "", append = TRUE)
  cat("", file = outputFile, sep = "\n", append = TRUE)
  cat("  channels=", channelNames, file = outputFile, sep = "", append = TRUE)
  cat("", file = outputFile, sep = "\n", append = TRUE)
  cat("  gender=", ECGData$gender, file = outputFile, sep = "", append = TRUE)
  cat("", file = outputFile, sep = "\n", append = TRUE)
  cat("  conditions=", ECGData$conditions, file = outputFile, sep = "", append = TRUE)
  cat("", file = outputFile, sep = "\n", append = TRUE)
  cat("</Header>", file = outputFile, sep = "\n", append = TRUE)

  #Save the values into the ASCII file 
  idsCol <- as.numeric(ECGData$nSamples)
  idsCol <- idsCol-1
  dataValues <- data.frame(row.names=0:idsCol)
  
  for (i in 1:ECGData$nLeads)
  {
    dataValues <- cbind(dataValues, ECGData$lead[[i]]$val)
  }
  
  write.table(dataValues, file = outputFile, col.names = FALSE, quote = FALSE, sep = "\t\t", append = TRUE)
}
LoadECGAscii <-function(ECGData, recordName, cal=1)
{ 
  # Reads a data file in ascii format and gets the ECG signal from it.
  # Then it stores the data in a structure called ECGData.
  # 
  # Args:
  #   ECGData: The structure where it stores the ECG record.
  #   recordName: The file where the ECG signal is stored.
  #   cal: The value to adapt to mV the quantities in the file. Its default value is 1.
  #
  # Returns:
  #   The data model containing the ECG data (ECGData). 
  #
  
  if (ECGData$Verbose)
  {
    cat("Reading the data file", recordName, "\n")
  }
  
  header <- scan(recordName, what="character", nlines = 1)
  hasHeader<- length(grep("<Header>", header))
  
  if (hasHeader>0)
  {
    # Skip the first lines (header lines) and read them separately
    ECGReading <- read.table(recordName, sep = "", skip=9) 
  }
  else
  {
    # Do not skip the first lines
    ECGReading <- read.table(recordName, sep = "", skip=0)
  }
  
  added=length(ECGReading$V1)
  
  if (added == 0)
  {
    if (ECGData$Verbose)
    {
      cat("No ECG data was loaded\n")
    }
  } 
  else
  {
    numCols <- ncol(ECGReading)
    val1 <- ECGReading[[1]][1]
    val2 <- ECGReading[[1]][2]
    val3 <- ECGReading[[1]][3]
    val4 <- ECGReading[[1]][4] 
    
    if(val1 == 0 && val2 == 1 && val3 == 2 && val4 == 3)
    { 
      nColumns <- numCols - 1
    }
    
    else
    {
      nColumns <- numCols
    }
    
    for (i in 1:nColumns)
    {
      ECGData$lead[[i]] <- list()
      ECGData$lead[[i]]$id <- list()
      ECGData$lead[[i]]$val <- list()
    }
    
    if (hasHeader>0)
    {
      ECGData <- ReadHeader(ECGData, recordName)
    }
    
    if (nColumns < numCols)
    {  
      if(cal != 1)
      {        
        for(j in 2:numCols)
        {
          ECGData$lead[[j-1]]$val <- ECGReading[[j]]*cal
        }
      }
      
      else
      {
        for (j in 2:numCols)
        {
          ECGData$lead[[j-1]]$val <- ECGReading[[j]]
        }
      }
    }
    else
    {
      if(cal != 1)
      {        
        for(j in 1:numCols)
        {
          ECGData$lead[[j]]$val <- ECGReading[[j]]*cal
        }
      }
      
      else
      {
        for(j in 1:numCols)
        {
          ECGData$lead[[j]]$val <- ECGReading[[j]]
        }
      }
    }
    
    if (ECGData$Verbose)
    {
      cat("Loaded ECG data from file\n")
    }
  }
  return(ECGData)
}

ReadHeader <- function(ECGData, recordName)
{
  # Reads a data file in ascii format and gets the header values from it.
  # Then it stores the header contents in a the ECGData structure.
  # 
  # Args:
  #   ECGData: The structure where it stores the values obtained from the header.
  #	  recordName: The file where the ECG signal is stored, whose header we want to get.
  #
  # Returns:
  #  The data model with the ECG file header (ECGData). 
  #
  readLines <- 1
  headerLine <- scan(recordName, what="character", skip = readLines, nlines = 1)
  isEndOfHeader<- length(grep("</Header>", headerLine))
  
  while (isEndOfHeader == 0)
  {
    headerField <- strsplit(headerLine, "=")
    field <- toString(headerField[[1]][1])
    value <- headerField[[1]][2]
    
    if (field == "channels")
    {
      channelNames <- strsplit(value, ",")
      
      for (i in 1:length(channelNames[[1]]))
      {
        ECGData$lead[[i]]$id <- toString(channelNames[[1]][i])
      }
    }
    else
    {
      if (field == "sFreq" || field == "nLeads" || field == "nSamples")
      {
        ECGData[[field]] <- as.numeric(value)
      }
      else
      {
        ECGData[[field]] <- value
      }
    }
    readLines <- readLines + 1
    headerLine <- scan(recordName, what="character", skip = readLines, nlines = 1)
    isEndOfHeader<- length(grep("</Header>", headerLine))
  }   
  
  return(ECGData)
}                            

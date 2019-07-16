LoadECGWFDB <- function(ECGData, recordHeader, ECGFileFormat, recordPath = "./", ADCGain = 200, ...)
{
  #Reads an ECG file in Physionet format. Header and data file must be in the same path
  #   
  # Args:
  #   ECGData: The structure where it stores the ECG record.
  #   recordHeader: The header file of the file where the ECG signal is stored.
  #   ECGFileFormat: The file format used in the record archive.
  #   recordPath: The path to the file where the ECG signal is stored.
  #   ADCGain: The specific gain value of the A-D converter.
  #
  # Returns:
  #   The data model containing the ECG data (ECGData). 
  
  if (missing(ECGData))
  {
    stop("You must specify the ECGData parameter")
  }
  
  if (missing(recordHeader))
  {
    stop("You must specify the recordHeader parameter")
  }
  
  dir = getwd()
  on.exit(setwd(dir))
  setwd(recordPath)
  
  if (!file.exists(recordHeader))
  {
    stop("The header file specified does not exist")
  }
  
  recordName <- strsplit(recordHeader, ".", fixed = TRUE)
  recordName <- paste(recordName[[1]][1], ".dat", sep = "")
  
  if (!file.exists(recordName))
  {
    stop("The data file of the header file", recordHeader, "does not exists, or it is not in the same path")
  }
  
  #Reads the header file
  ECGData <- LoadECGWFDBHeader(ECGData, recordHeader, ECGFileFormat)
  
  if (missing(ECGFileFormat))
  {
    ECGFileFormat <- ECGData$lead[[1]]$ECGFileFormat
  }
    
  #Reads the data file
  ECGData <- LoadECGWFDBData(ECGData, recordName, ECGFileFormat)
  #Converts the values stored in ECGData
  ECGData <- ConvertToPhysicalUnits(ECGData)
  
  return(ECGData)
}

LoadECGWFDBHeader <- function(ECGData, recordHeader, ECGFileFormat)
{
  #Reads the header file
  #   
  # Args:
  #   ECGData: The structure where it stores the ECG header info.
  #   recordHeader: The header file of the file where the ECG signal is stored.
  #   ECGFileFormat: The file format used in the record archive.
  #
  # Returns:
  #   The data model containing the header info of the ECG data (ECGData). 
  
  if (ECGData$Verbose)
  {
    cat("Reading the data file", recordHeader, "\n")
  }  
  
  firstLine <- scan(recordHeader, what = "character", nlines = 1)
  header <- list()
  firstLine <- strsplit(firstLine, " ")
  header$id <- firstLine[[1]][1]
  header$nLeads <- as.numeric(firstLine[[2]][1])
  header$sFreq <- as.numeric(firstLine[[3]][1])
  header$nSamples <- as.numeric(firstLine[[4]][1])
  channels <- read.table(recordHeader, skip = 1)
  
  for (i in 1:header$nLeads)
  {
    header$lead[[i]] <- list()
    header$lead[[i]]$filePathName <- channels[[1]][i]
    
    if (missing(ECGFileFormat))
    {
      header$lead[[i]]$ECGFileFormat <- as.numeric(channels[[2]][i])
      ECGFileFormat <- header$lead[[i]]$ECGFileFormat
    }  
    
    if (ECGFileFormat != 16 && ECGFileFormat != 212)
    {
      stop("File format not valid within this application. Please try with files in 16 or 212 format\n")
    }
    
    header$lead[[i]]$ADCGain <- as.numeric(channels[[3]][i])
    header$lead[[i]]$ADCBitResolution <- as.numeric(channels[[4]][i])
    header$lead[[i]]$ADCZero <- as.numeric(channels[[5]][i])
    header$lead[[i]]$InitialValue <- as.numeric(channels[[6]][i])
    header$lead[[i]]$checkSum <- as.numeric(channels[[7]][i])
    header$lead[[i]]$blockSize <- as.numeric(channels[[8]][i])
    header$lead[[i]]$id <- channels[[9]][i]
  }
  
  #Stores the header values into the ECGData structure
  ECGData$nLeads <- header$nLeads
  ECGData$sFreq <- header$sFreq
  ECGData$nSamples <- header$nSamples
  ECGData$id <- header$id
  
  for (i in 1:header$nLeads)
  {
    ECGData$lead[[i]]$ADCGain <- header$lead[[i]]$ADCGain
    ECGData$lead[[i]]$ADCBitResolution <- header$lead[[i]]$ADCBitResolution
    ECGData$lead[[i]]$ADCZero <- header$lead[[i]]$ADCZero
    ECGData$lead[[i]]$InitialValue <- header$lead[[i]]$InitialValue
    ECGData$lead[[i]]$checkSum <- header$lead[[i]]$checkSum
    ECGData$lead[[i]]$blockSize <- header$lead[[i]]$blockSize
    ECGData$lead[[i]]$id <- header$lead[[i]]$id
  }
  
  return(ECGData)
}

LoadECGWFDBData <- function(ECGData, recordName, ECGFileFormat)
{
  # Reads a data file in Physionet format and gets the ECG signal from it.
  # Then it stores the data in a structure called ECGData.
  # 
  # Args:
  #   ECGData: The structure where it stores the ECG data.
  #   recordName: The file where the ECG signal is stored.
  #   ECGFileFormat: The file format used in the record archive.
  #
  # Returns:
  #   The data model containing the ECG data (ECGData). 
  #
  
  if (missing(ECGFileFormat))
  {
    stop("You must specify the format parameter")
  }
  
  if (ECGData$Verbose)
  {
    cat("Reading the data file ", recordName, "\n")
  } 
  
  #Creating channels arrays with the number of samples of the ECG file as size  
  samplesChannel1 <- mat.or.vec(ECGData$nSamples, 1)
  samplesChannel2 <- mat.or.vec(ECGData$nSamples, 1)
  out <- .C("getFileSize", recordName, fileSize = as.integer(0))
  fileSize <- out$fileSize  
  
  if (ECGFileFormat == 16)
  {
    out <- .C("readFile16", recordName, samplesChannel1 = as.double(samplesChannel1), samplesChannel2 = as.double(samplesChannel2), fileSize)
  }
  
  else
  {
    if (ECGFileFormat == 212)
    {
      out <- .C("readFile212", recordName, samplesChannel1 = as.double(samplesChannel1), samplesChannel2 = as.double(samplesChannel2), fileSize)
    }
    
    else
    {
      cat("File format not valid within this application. Please try with files in 16 or 212 format\n")
    }
  }
  
  #Retrieving the samples of the ECG file and storing them in the ECGData model structure
  ECGData$lead[[1]]$val <- out$samplesChannel1
  ECGData$lead[[2]]$val <- out$samplesChannel2
  return(ECGData)
}

ConvertToPhysicalUnits <- function(ECGData)
{
  # Updates the values stored in the structure ECGData.
  # 
  # Args:
  #   ECGData: The structure where it stores the ECG data.
  #
  # Returns:
  #   The data model containing the ECG data with the updated values (ECGData). 
  #
  
  i <- 0 
  
  while (i < ECGData$nLeads)
  {
    dataSize <- length(ECGData$lead[[i+1]]$val)
    ADCGain <- ECGData$lead[[i+1]]$ADCGain 
    ADCZero <- ECGData$lead[[i+1]]$ADCZero
    out <- .C("convertToPhysicalUnits", as.integer(dataSize), as.integer(ADCGain), as.integer(ADCZero), val = as.double(ECGData$lead[[i+1]]$val))
    ECGData$lead[[i+1]]$val <- out$val
    i <- i + 1
  }
  
  return(ECGData)
}
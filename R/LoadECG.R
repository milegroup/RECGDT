LoadECG <- function(ECGData, recordName, recordPath, cal, fileType, ECGFileFormat)
{
  # Redirects to the functions responsible of reading and getting the ECG signal from a file, depending of the file type (WFDB or ASCII).
  # 
  # Args:
  #   ECGData: The structure where it stores the ECG record.
  #	  recordName: The file name, in an ASCII file type, or the path to the file where the ECG signal is stored, in the case of a WFDB file type.
  #   recordPath: The path to the file where the ECG signal is stored (used for ASCII files).
  #   cal: The value to adapt to mV the quantities in the file. Its default value is 1.
  #   fileType: The file format used in the recordName archive (used for WFDB files).
  #   ECGFileFormat: This parameter should be "WFDB" to load an WFDB file or "ASCII" to load an ASCII one.
  #
  # Returns:
  #   The data model containing the ECG data (ECGData). 
  #
  
  if (missing(recordName))
  {
    stop("You must specify the recordName parameter")
  }
  
  if (length(grep("wfdb", ECGFileFormat, ignore.case = TRUE)) > 0)
  {
    ECGData <- LoadECGWFDB(ECGData, recordName, fileType, recordPath)
  }
  
  else 
  {
    if (length(grep("ascii", ECGFileFormat, ignore.case = TRUE)) > 0)
    {
      ECGData <- LoadECGAscii(ECGData, recordName, recordPath, cal)
    }
    
    else
    {
      stop("You must specify WFDB or ASCII for fileType parameter")
    }
  }
  
  return(ECGData)
}
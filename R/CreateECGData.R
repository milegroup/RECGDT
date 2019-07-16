CreateECGData <- function(Verbose = FALSE, ...) {
  # Creates an structure for storing an ECG record
  # 
  # Args:
  #   Verbose: Enable verbose mode (True or False)
  #
  # Returns:
  #  The created data model 
  #
  
  if (Verbose) {
    cat("** Creating an empty ECG structure  **\n")
  }
  
  ECGData <- list()
  
  ECGData$id <- NULL # ID or Record name.
  
  ECGData$sFreq <- NULL # Sampling frequency.
  
  ECGData$nLeads <- NULL # Number of leads.
  
  ECGData$nSamples <- NULL # Number of samples for the entire ECG record.
  
  ECGData$gender <- NULL # Patient's gender (if available).
  
  ECGData$conditions <- NULL # Patient's known conditions (if available).
  
  ECGData$date <- NULL # Date in YYYY-MM-DD format.
  
  ECGData$time <- NULL # Time in HH:MM:SS format.
  
  ECGData$RRDistance <- NULL # Average distance between adjacent R Peaks.
  
  ECGData$lead <- list() # Information of each lead with the following fields:
  # ECGData$lead[[nChannel]]$id # Lead name or code.
  # ECGData$lead[[nChannel]]$ECGFileFormat # File format of the current ECG record.
  # ECGData$lead[[nChannel]]$ADCGain # Analog-to-Digital Converter (ADC) gain error of the current ECG record.
  # ECGData$lead[[nChannel]]$ADCBitResolution # Analog-to-Digital Converter (ADC) resolution used for the current ECG record.
  # ECGData$lead[[nChannel]]$ADCZero # Analog-to-Digital Converter (ADC) zero value for the current ECG record.
  # ECGData$lead[[nChannel]]$initialValue # Initial value for the current ECG record.
  # ECGData$lead[[nChannel]]$checkSum # Checksum for the current ECG record.
  # ECGData$lead[[nChannel]]$blockSize # Block size for the current ECG record.
  # ECGData$lead[[nChannel]]$val <- c() # Array containing the signal samples values in mV for the current lead.
  # ECGData$lead[[nChannel]]$beat <- c() # Array containing the position of each beat inside the lead.
  
  ECGData$beat <- c() # Array containing information of each global beat detected on the ECG record.
  # ECGData$beat[[nBeat]]$pos # R Wave Peak position (inside the entire signal).
  # ECGData$beat[[nBeat]]$basalBeat <- c() # Array containing arrays with contents of the basal beat generated for the present beat on each channel.
	# ECGData$beat[[nBeat]]$basalBeat[[nChannel]]$basalBeatSignal <- c() # Array containing the samples values in mV for the basal beat generated for the present beat using the current channel signal.
	# ECGData$beat[[nBeat]]$basalBeat[[nChannel]]$basalBeatRPeakPos # The equivalent position for the R Peak inside the basal beat.
	# ECGData$beat[[nBeat]]$basalBeat[[nChannel]]$avgBeat <- c() # Array containing the samples values in mV for the centered signal segment of the basal beat, to compare against the real signal.
	# ECGData$beat[[nBeat]]$basalBeat[[nChannel]]$QRS <- list() # List containing information of QRS Complex positions inside the present beat.
		# ECGData$beat[[nBeat]]$basalBeat[[nChannel]]$QRS$peak # R Wave Peak position (inside the entire signal).
		# ECGData$beat[[nBeat]]$basalBeat[[nChannel]]$QRS$QRSOnset # QRS Complex Onset position (inside the basal beat signal).
		# ECGData$beat[[nBeat]]$basalBeat[[nChannel]]$QRS$QRSOffset # QRS Complex Offset position (inside the basal beat signal).
		# ECGData$beat[[nBeat]]$basalBeat[[nChannel]]$QRS$QPeak # Q Wave Peak position (inside the basal beat signal).
		# ECGData$beat[[nBeat]]$basalBeat[[nChannel]]$QRS$SPeak # S Wave Peak position (inside the basal beat signal).
	# ECGData$beat[[nBeat]]$basalBeat[[nChannel]]$PWave <- list() # List containing information of P Wave positions inside the present beat.
		# ECGData$beat[[nBeat]]$basalBeat[[nChannel]]$PWave$PPeak # P Wave Peak position (inside the basal beat signal).
		# ECGData$beat[[nBeat]]$basalBeat[[nChannel]]$PWave$POnset # P Wave Onset position (inside the basal beat signal).
		# ECGData$beat[[nBeat]]$basalBeat[[nChannel]]$PWave$POffset # P Wave Offset position (inside the basal beat signal).
	# ECGData$beat[[nBeat]]$basalBeat[[nChannel]]$TWave <- list() # List containing information of T Wave positions inside the present beat.
		# ECGData$beat[[nBeat]]$basalBeat[[nChannel]]$TWave$TPeak # T Wave Peak position (inside the basal beat signal).
		# ECGData$beat[[nBeat]]$basalBeat[[nChannel]]$TWave$TOnset # T Wave Onset position (inside the basal beat signal).
		# ECGData$beat[[nBeat]]$basalBeat[[nChannel]]$TWave$TOffset # T Wave Offset position (inside the basal beat signal).
  # ECGData$beat[[nBeat]]$detrendPlot #Beat signal segment without trends.
  
  ECGData$diagnostics  <- list() # Information of the probability of each illness for the present record.
    ECGData$diagnostics$bradycardia # Probability of Bradycardia.
    ECGData$diagnostics$ischemia # Probability of Ischemia.
    ECGData$diagnostics$myocardialInfarction # Probability of Myocardial Infarction.
    ECGData$diagnostics$tachycardia # Probability of Tachycardia.
    ECGData$diagnostics$ventricularHypertrophy # Probability of Ventricular Hypertrophy.
    ECGData$diagnostics$wpwSyndrome # Probability of Wolff-Parkinson-White Syndrome.
  
  ECGData$annot <- list() # List containing other annotations different from beat ones with the following fields:
    ECGData$annot$pos # The annotation positions, measured in samples.
    ECGData$annot$type # String containing the type or code of the annotation.
    ECGData$annot$value # If the annotation has a numerical value, this value.
    ECGData$annot$comment # Other additional annotations comments.
  
  ECGData$Verbose <- Verbose # Verbose information for data structure creation.
  
  if (Verbose) {
    cat("  ECG data structure created\n")
  }
  
  return(ECGData)
}
DiagnoseDiseases <- function(ECGData, modelBradycardia, modelIschemia, modelMyocardialInfarction, modelTachycardia, modelVentricularHypertrophy, modelWPWSyndrome)
{
  # Obtains diagnosis probabilities for the studied diseases
  # 
  # Args:
  #   ECGData: The structure with the beats to be delineated and used in diagnosis.
  #   modelBradycardia: Model with the parameters used in the diagnosis of Bradycardia.
  #   modelIschemia: Model with the parameters used in the diagnosis of Ischemia.
  #   modelMyocardialInfarction: Model with the parameters used in the diagnosis of Myocardial Infarction.
  #   modelTachycardia: Model with the parameters used in the diagnosis of Tachycardia.
  #   modelVentricularHypertrophy: Model with the parameters used in the diagnosis of Ventricular Hypertrophy.
  #   modelWPWSyndrome: Model with the parameters used in the diagnosis of Wolff-Parkinson-White Syndrome.
  # Returns:
  #  The structure with the diagnosis probability for each disease added.
  #
  
  library(pROC)
  library(mgcv)
  library(caTools)
  
  if (missing(ECGData))
  {
    stop("You must specify the ECGData parameter")
  }
  
  diagnosedBeats <- c()
  diagnosedBeats[1] <- 15
  diagnosedBeats[2] <- ceiling(length(ECGData$beat)*0.25)
  diagnosedBeats[3] <- ceiling(length(ECGData$beat)*0.5)
  diagnosedBeats[4] <- ceiling(length(ECGData$beat)*0.75)
  diagnosedBeats[5] <- length(ECGData$beat)-15
  DiagnosticValuesRecord <- c()
  DiagnosticValuesChannel <- c()
  Distancia.RR <- c()
  Duracion.PR <- c()
  Duracion.QRS <- c()
  Duracion.onda.Q <- c()
  Altura.onda.Q <- c()
  Altura.onda.R <- c()
  Altura.onda.S <- c()
  Duracion.Qtc <- c()
  Depresion.ST <- c()
  Elevacion.ST <- c()
  ST <- c()
  baseline <- 0
  
  #Before each diagnosis, probabilities are reset to 0
  ECGData$diagnostics$bradycardia <- 0
  ECGData$diagnostics$ischemia <- 0
  ECGData$diagnostics$myocardialInfarction <- 0
  ECGData$diagnostics$tachycardia <- 0
  ECGData$diagnostics$ventricularHypertrophy <- 0
  ECGData$diagnostics$wpwSyndrome <- 0
  
  for (i in 1:ECGData$nLeads)
  {
    for (j in 1:5)
    {
      ECGData <- AdaptedBasalBeat(ECGData, i, diagnosedBeats[j], 5)
      ECGData <- ECGDelineator(ECGData, i, diagnosedBeats[j])
      
      if(ECGData$beat[[diagnosedBeats[j]]]$basalBeat[[i]]$PWave$POnset > 0)
      {
        valuePOnset <- ECGData$beat[[diagnosedBeats[j]]]$basalBeat[[i]]$basalBeatSignal[ECGData$beat[[diagnosedBeats[j]]]$basalBeat[[i]]$PWave$POnset]
      }
      else
      {
        valuePOnset <- 0
      }
      
      if(ECGData$beat[[diagnosedBeats[j]]]$basalBeat[[i]]$PWave$PPeak > 0)
      {
        valuePPeak <- ECGData$beat[[diagnosedBeats[j]]]$basalBeat[[i]]$basalBeatSignal[ECGData$beat[[diagnosedBeats[j]]]$basalBeat[[i]]$PWave$PPeak]
      }
      else
      {
        valuePPeak <- 0
      }
      
      if(ECGData$beat[[diagnosedBeats[j]]]$basalBeat[[i]]$PWave$POffset > 0)
      {
        valuePOffset <- ECGData$beat[[diagnosedBeats[j]]]$basalBeat[[i]]$basalBeatSignal[ECGData$beat[[diagnosedBeats[j]]]$basalBeat[[i]]$PWave$POffset]
      }
      else
      {
        valuePOffset <- 0
      }
      
      if(ECGData$beat[[diagnosedBeats[j]]]$basalBeat[[i]]$QRS$QRSOnset > 0)
      {
        valueQRSOnset <- ECGData$beat[[diagnosedBeats[j]]]$basalBeat[[i]]$basalBeatSignal[ECGData$beat[[diagnosedBeats[j]]]$basalBeat[[i]]$QRS$QRSOnset]
      }
      else
      {
        valueQRSOnset <- 0
      }
      
      if (ECGData$beat[[diagnosedBeats[j]]]$basalBeat[[i]]$QRS$QPeak > 0)
      {
        valueQPeak <- ECGData$beat[[diagnosedBeats[j]]]$basalBeat[[i]]$basalBeatSignal[ECGData$beat[[diagnosedBeats[j]]]$basalBeat[[i]]$QRS$QPeak]
      }
      else
      {
        valueQPeak <- 0
      }
      
      if(ECGData$beat[[diagnosedBeats[j]]]$basalBeat[[i]]$basalBeatRPeakPos > 0)
      {
        valueRPeak <- ECGData$beat[[diagnosedBeats[j]]]$basalBeat[[i]]$basalBeatSignal[ECGData$beat[[diagnosedBeats[j]]]$basalBeat[[i]]$basalBeatRPeakPos]
      }
      else
      {
        valueRPeak <- 0
      }
      
      if(ECGData$beat[[diagnosedBeats[j]]]$basalBeat[[i]]$QRS$SPeak > 0)
      {
        valueSPeak <- ECGData$beat[[diagnosedBeats[j]]]$basalBeat[[i]]$basalBeatSignal[ECGData$beat[[diagnosedBeats[j]]]$basalBeat[[i]]$QRS$SPeak]
      }
      else
      {
        valueSPeak <- 0
      }
      
      if(ECGData$beat[[diagnosedBeats[j]]]$basalBeat[[i]]$QRS$QRSOffset > 0)
      {
        valueQRSOffset <- ECGData$beat[[diagnosedBeats[j]]]$basalBeat[[i]]$basalBeatSignal[ECGData$beat[[diagnosedBeats[j]]]$basalBeat[[i]]$QRS$QRSOffset]
      }
      else
      {
        valueQRSOffset <- 0
      }
      
      if(ECGData$beat[[diagnosedBeats[j]]]$basalBeat[[i]]$TWave$TOnset > 0)
      {
        valueTOnset <- ECGData$beat[[diagnosedBeats[j]]]$basalBeat[[i]]$basalBeatSignal[ECGData$beat[[diagnosedBeats[j]]]$basalBeat[[i]]$TWave$TOnset]
      }
      else
      {
        valueTOnset <- 0
      }
      
      if(ECGData$beat[[diagnosedBeats[j]]]$basalBeat[[i]]$TWave$TPeak > 0)
      {
        valueTPeak <- ECGData$beat[[diagnosedBeats[j]]]$basalBeat[[i]]$basalBeatSignal[ECGData$beat[[diagnosedBeats[j]]]$basalBeat[[i]]$TWave$TPeak]
      }
      else
      {
        valueTPeak <- 0
      }
      
      if(ECGData$beat[[diagnosedBeats[j]]]$basalBeat[[i]]$TWave$TOffset)
      {
        valueTOffset <- ECGData$beat[[diagnosedBeats[j]]]$basalBeat[[i]]$basalBeatSignal[ECGData$beat[[diagnosedBeats[j]]]$basalBeat[[i]]$TWave$TOffset]
      }
      else
      {
        valueTOffset <- 0
      }
      
      baseline <- sum(ECGData$beat[[diagnosedBeats[j]]]$basalBeat[[i]]$basalBeatSignal[ECGData$beat[[diagnosedBeats[j]]]$basalBeat[[i]]$PWave$POffset:ECGData$beat[[diagnosedBeats[j]]]$basalBeat[[i]]$QRS$QRSOnset])/(ECGData$beat[[diagnosedBeats[j]]]$basalBeat[[i]]$QRS$QRSOnset-ECGData$beat[[diagnosedBeats[j]]]$basalBeat[[i]]$PWave$POffset+1)
      
      heightPWave <- vector()
      heightQWave <- vector()
      heightRWave <- vector()
      heightSWave <- vector()
      heightTWave <- vector()
      
      if((valuePPeak > 0 && baseline > 0) || (valuePPeak < 0 && baseline < 0))
      {
        heightPWave <- abs(abs(valuePPeak)-abs(baseline))
      }
      else
      {
        if ((valuePPeak > 0 && baseline < 0) || (valuePPeak < 0 && baseline > 0))
        {
          heightPWave <- abs(valuePPeak)+abs(baseline)
        }
      }
      
      if((valueQPeak > 0 && baseline > 0) || (valueQPeak < 0 && baseline < 0))
      {
        heightQWave <- abs(abs(valueQPeak)-abs(baseline))
      }
      else
      {
        if ((valueQPeak > 0 && baseline < 0) || (valueQPeak < 0 && baseline > 0))
        {
          heightQWave <- abs(valueQPeak)+abs(baseline)
        }
      }
      
      if((valueRPeak > 0 && baseline > 0) || (valueRPeak < 0 && baseline < 0))
      {
        heightRWave <- abs(abs(valueRPeak)-abs(baseline))
      }
      else
      {
        if ((valueRPeak > 0 && baseline < 0) || (valueRPeak < 0 && baseline > 0))
        {
          heightRWave <- abs(valueRPeak)+abs(baseline)
        }
      }
      
      if((valueSPeak > 0 && baseline > 0) || (valueSPeak < 0 && baseline < 0))
      {
        heightSWave <- abs(abs(valueSPeak)-abs(baseline))
      }
      else
      {
        if ((valueSPeak > 0 && baseline < 0) || (valueSPeak < 0 && baseline > 0))
        {
          heightSWave <- abs(valueSPeak)+abs(baseline)
        }
      }
      
      if((valueTPeak > 0 && baseline > 0) || (valueTPeak < 0 && baseline < 0))
      {
        heightTWave <- abs(abs(valueTPeak)-abs(baseline))
      }
      else
      {
        if ((valueTPeak > 0 && baseline < 0) || (valueTPeak < 0 && baseline > 0))
        {
          heightTWave <- abs(valueTPeak)+abs(baseline)
        }
      }
      
      PRDuration <- (ECGData$beat[[diagnosedBeats[j]]]$basalBeat[[i]]$QRS$QRSOnset-ECGData$beat[[diagnosedBeats[j]]]$basalBeat[[i]]$PWave$POnset)/ECGData$sFreq
      QRSDuration <- (ECGData$beat[[diagnosedBeats[j]]]$basalBeat[[i]]$QRS$QRSOffset-ECGData$beat[[diagnosedBeats[j]]]$basalBeat[[i]]$QRS$QRSOnset)/ECGData$sFreq
      QDuration <- (which.min(abs(abs(as.numeric(ECGData$beat[[diagnosedBeats[j]]]$basalBeat[[i]]$basalBeatSignal[(ECGData$beat[[diagnosedBeats[j]]]$basalBeat[[i]]$QRS$QRSOnset+5):ECGData$beat[[diagnosedBeats[j]]]$basalBeat[[i]]$basalBeatRPeakPos]))-abs(as.numeric(ECGData$beat[[diagnosedBeats[j]]]$basalBeat[[i]]$basalBeatSignal[ECGData$beat[[diagnosedBeats[j]]]$basalBeat[[i]]$QRS$QRSOnset]))))+4)/ECGData$sFreq
      QTcDuration <- ((ECGData$beat[[diagnosedBeats[j]]]$basalBeat[[i]]$TWave$TOffset-ECGData$beat[[diagnosedBeats[j]]]$basalBeat[[i]]$QRS$QRSOnset)/ECGData$sFreq)/sqrt(ECGData$RRDistance)
      STDepression <- 0
      STElevation <- 0
      
      if((valueQRSOffset - baseline) < 0)
      {
        STDepression <- valueQRSOffset - baseline
      }
      else
      {
        STElevation <- valueQRSOffset - baseline
      }
      
      TSignQRS <- (valueTPeak < valueTOnset && valueTPeak < valueTOffset && valueRPeak < valueQRSOnset && valueRPeak < valueQRSOffset) || (valueTPeak > valueTOnset && valueTPeak > valueTOffset && valueRPeak > valueQRSOnset && valueRPeak > valueQRSOffset)
      
      DiagnosticValuesRecord <- c(DiagnosticValuesRecord, ECGData$id)
      DiagnosticValuesChannel <- c(DiagnosticValuesChannel, ECGData$lead[[i]]$id)
      Distancia.RR <- c(Distancia.RR, ECGData$RRDistance)
      Duracion.PR <- c(Duracion.PR, PRDuration)
      Duracion.QRS <- c(Duracion.QRS, QRSDuration)
      Duracion.onda.Q <- c(Duracion.onda.Q, QDuration)
      Altura.onda.Q <- c(Altura.onda.Q, heightQWave)
      Altura.onda.R <- c(Altura.onda.R, heightRWave)
      Altura.onda.S <- c(Altura.onda.S, heightSWave)
      Duracion.Qtc <- c(Duracion.Qtc, QTcDuration)
      Depresion.ST <- c(Depresion.ST, STDepression)
      Elevacion.ST <- c(Elevacion.ST, STElevation)
      
      if(TSignQRS == TRUE)
      {
        ST <- c(ST, 1)
      }
      else
      {
        ST <- c(ST, 0)
      }
    }
  }

  newCase <- data.frame(DiagnosticValuesRecord, DiagnosticValuesChannel, Distancia.RR, Duracion.PR, Duracion.QRS, Duracion.onda.Q, Altura.onda.Q, Altura.onda.R, Altura.onda.S, Duracion.Qtc, Depresion.ST, Elevacion.ST, ST)
  print(newCase)
  ECGData$diagnostics$bradycardia <- mean(predict(modelBradycardia,type = "response",newdata=newCase))
  ECGData$diagnostics$ischemia <- mean(predict(modelIschemia,type = "response",newdata=newCase))
  ECGData$diagnostics$myocardialInfarction <- mean(predict(modelMyocardialInfarction,type = "response",newdata=newCase))
  ECGData$diagnostics$tachycardia <- mean(predict(modelTachycardia,type = "response",newdata=newCase))
  ECGData$diagnostics$ventricularHypertrophy <- mean(predict(modelVentricularHypertrophy,type = "response",newdata=newCase))
  ECGData$diagnostics$wpwSyndrome <- mean(predict(modelWPWSyndrome,type = "response",newdata=newCase))
  
  return(ECGData)
  
}
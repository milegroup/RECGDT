int isATWave(int *samplingFrequency, int *previousPosition, int *currentPosition, double *derivateSignal, int *DELAY)
{
  double minRefractoryPeriod = *samplingFrequency*0.2;
  
  if ((*currentPosition-*previousPosition) < minRefractoryPeriod)
  {
    return(1);
  }
  else
  {
    return(0);
  }
}

void panTompkinsAlgorithm(double *derivateSignal, int *samplingFrequency, int *peakPositions, double *peakHeights, double *threshold, int *allPeakPositions, double *allPeakHeights, int *allPeakArraySize, int *signalPeakPositions, double *signalPeakHeights, double * SPKI, double * NPKI, int *windowSize, int *channelNum)
{
  int RR_ARRAY_SIZE = 8;
  signalPeakPositions[0] = peakPositions[0];
  signalPeakPositions[1] = peakPositions[1];
  signalPeakHeights[0] = peakHeights[0];
  signalPeakHeights[1] = peakHeights[1];
  double RRAverage1 = peakPositions[1]-peakPositions[0];
  double RRAverage2 = peakPositions[1]-peakPositions[0];
  double RRLowLimit = 0.92*RRAverage2;
  double RRHighLimit = 1.16*RRAverage2;
  double RRMissedLimit = 1.66*RRAverage2;
  double recentRRBeats [RR_ARRAY_SIZE];
  double normalRecentRRBeats[RR_ARRAY_SIZE];
  recentRRBeats[0] = peakPositions[0];
  recentRRBeats[1] = peakPositions[1];
  normalRecentRRBeats[0] = peakPositions[0];
  normalRecentRRBeats[1] = peakPositions[1];
  double threshold2 = 0.0;
  *SPKI = (peakHeights[0]+peakHeights[1])/2;
  *NPKI = 0.0;
  int i = 0;
  int bound = peakPositions[1];
  int value = allPeakPositions[i];
  
  while (value < bound)
  {
    if (allPeakHeights[i] < *SPKI)
    {
      *NPKI = 0.125 * allPeakHeights[i] + 0.875 * *NPKI;
    }
    
    i++;
    value = allPeakPositions[i];
  }
  
  *threshold = *NPKI+0.25*(*SPKI-*NPKI);
  threshold2 = *threshold/2;
  int peakIndex = 2;
  int avg1Index = 2;
  int avg2Index = 2;
  int lastPeakPosition = peakPositions[1];
  int maxPosition = 0;
  int maxHeight = 0;
  double sum = 0.0;
  
  while (i < *allPeakArraySize)
  {
    if ( allPeakHeights[i] > *threshold )
    {
      int previous = signalPeakPositions[peakIndex-1];
      int current = allPeakPositions[i];
      
      if (isATWave(samplingFrequency, &previous, &current, derivateSignal, windowSize))
      {
        *NPKI = 0.125 * allPeakHeights[i] + 0.875 * *NPKI;
        
        if ((allPeakHeights[i] > maxHeight) && (allPeakHeights[i] > threshold2))
        {
          maxHeight = allPeakHeights[i];
          maxPosition = allPeakPositions[i];
        }
      }
      else
      {
        *SPKI = 0.125 * allPeakHeights[i] + 0.875 * *SPKI;
        signalPeakPositions[peakIndex] = allPeakPositions[i];
        signalPeakHeights[peakIndex] = allPeakHeights[i];
        lastPeakPosition = allPeakPositions[i];
        peakIndex++;
        maxPosition = 0;
        maxHeight = 0;
      }
    }
    else
    {
      *NPKI = 0.125 * allPeakHeights[i] + 0.875 * *NPKI;
      
      if ((allPeakHeights[i] > maxHeight) && (allPeakHeights[i] > threshold2))
      {
        maxHeight = allPeakHeights[i];
        maxPosition = allPeakPositions[i];
      }
    }
    
    if (((allPeakPositions[i]-lastPeakPosition) > RRMissedLimit) && (maxPosition != 0))
    {
      *SPKI = 0.25 * maxHeight + 0.75 * *SPKI;
      signalPeakPositions[peakIndex] = maxPosition;
      signalPeakHeights[peakIndex] = maxHeight;
      lastPeakPosition = maxPosition;
      peakIndex++;
      maxPosition = 0;
      maxHeight = 0;
    }
    
    
    if (*channelNum == 12)
    {
      *threshold = *NPKI+0.25*(*SPKI-*NPKI);
      threshold2 = *threshold/2;
    }
    
    else
    {
      if (*channelNum == 4 || *channelNum == 8 || *channelNum == 11)
      {
        *threshold = *NPKI+0.1*(*SPKI-*NPKI);
        threshold2 = *threshold/2;
      }
      
      else
      {
        if (*channelNum == 7 || *channelNum == 9 || *channelNum == 10)
        {
          *threshold = *NPKI+0.08*(*SPKI-*NPKI);
          threshold2 = *threshold/2;
        }
        else
        {
          if (*channelNum == 1 || *channelNum == 2 || *channelNum == 3 || *channelNum == 6)
          {
            *threshold = *NPKI+0.05*(*SPKI-*NPKI);
            threshold2 = *threshold/2;
          }
          else
          {
            if (*channelNum == 5)
            {
              *threshold = *NPKI+0.02*(*SPKI-*NPKI);
              threshold2 = *threshold/2;
            }
          }
        }
      }
    }
    
    if (((allPeakPositions[i]-lastPeakPosition) > RRLowLimit) && ((allPeakPositions[i]-lastPeakPosition) < RRHighLimit))
    {
      normalRecentRRBeats[avg2Index%RR_ARRAY_SIZE] = allPeakPositions[i];
      
      for (int j = 0 ; j < RR_ARRAY_SIZE; j++)
      {
        sum += normalRecentRRBeats[j];
      }
      
      RRAverage2 = 0.125*sum;
      avg2Index++;
    }
    else
    {
      *threshold /= 2;
      threshold2 /= 2;
    }
    
    recentRRBeats[avg1Index%RR_ARRAY_SIZE] = allPeakPositions[i];
    
    for (int j = 0 ; j < RR_ARRAY_SIZE; j++)
    {
      sum += recentRRBeats[j];
    }
    
    RRAverage1 = 0.125*sum;
    avg1Index++;
    RRLowLimit = 0.92*RRAverage2;
    RRHighLimit = 1.16*RRAverage2;
    RRMissedLimit = 1.66*RRAverage2;
    i++;
  }
}
#include <stdlib.h>
#include <math.h>

/*Detects peaks into an interval (maximum heights)*/
void peakDetection(double *signal, int *positions, double *heights, int *arraySize, int *intervalSize)
{
  int i = *intervalSize;
  int j = 0;
  int isPeak;
  
  while (i < (*arraySize-*intervalSize))
  { 
    isPeak = 1;
    
    for (int k = (i-*intervalSize) ; k < (i+*intervalSize); k++)
    {
      if (k == i)
      {
        continue;
      }
      
      if (signal[k] > signal[i])
      {
        isPeak = 0;
        break;
      }
    }
    
    if (isPeak)
    {			
      heights[j] = signal[i]; 
      positions[j] = i+1;
      j++;
    }
    
    i++;
  }
}

/*Given two detected peaks, one in the mwi signal and the other in the derivate signal, it decides if these positions are the same beat*/
void sameBeat(int *mwiPositions, double *mwiHeights, int *derivatePositions, double *derivateHeights,  int *mwiLength, int *derivateLength, int *resultPositions, double *resultHeights, int *windowSize)
{
  int errorFactor = 50;
  int signalPosition = 0;
  double signalHeight = 0.0;
  int rIndex = 0;
  int DELAY = *windowSize/2;
  
  if (*mwiLength > *derivateLength)
  {
    for (int i = 0; i <  *derivateLength; i++)
    {
      signalPosition = derivatePositions[i]+DELAY;
      
      for (int j = 0; (mwiPositions[j]-errorFactor) <  signalPosition ; j++)
      {
        if ((abs(signalPosition-mwiPositions[j]) < errorFactor))
        {
          resultPositions[rIndex] = derivatePositions[i];
          resultHeights[rIndex] = derivateHeights[i];
          rIndex++;
        }
      }
    }
  }
  else
  {
    for (int i = 0; i < *mwiLength ; i++)
    {
      signalPosition = mwiPositions[i];
      signalHeight = mwiHeights[i];
      
      for (int j = 0; ((derivatePositions[j]+DELAY)-errorFactor) < signalPosition; j++)
      {
        if ((abs(signalPosition-(derivatePositions[j]+DELAY)) < errorFactor))
        {
          resultPositions[rIndex] = derivatePositions[j];
          resultHeights[rIndex] = derivateHeights[j];
          rIndex++;
        }
      }
    }
  }
}
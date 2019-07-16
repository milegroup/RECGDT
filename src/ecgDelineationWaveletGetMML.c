#include "getPeaks.h"
#include <math.h>
#include <stdlib.h>

int cuiweiAlgorithm(double *signal, int peakPos1, int peakPos2, int referencePeak)
{
  double a1 = fabs(signal[peakPos1]);
  double l1 = fabs(referencePeak-peakPos1);
  double factor = 1.2;
  double a2 = fabs(signal[peakPos2]);
  double l2 = fabs(referencePeak-peakPos2);		
  
  if((a1/l1) > (factor*a2/l2))
  {				
    return peakPos1;
  }
  
  else
  {
    
    if((a2/l2) > (factor*a1/l1))
    {
      return peakPos2;
    }
    
    else
    {			
      if((peakPos1 < referencePeak && peakPos2 < referencePeak) || (peakPos1 > referencePeak && peakPos2 > referencePeak))
      {
        return (abs(referencePeak-peakPos1) < abs(referencePeak-peakPos2)) ? peakPos1 : peakPos2;
      }
      
      else
      {
        return (peakPos2 > referencePeak) ? peakPos1 : peakPos2;
      }
      
    }
    
  }
  
}

int removeRedundantPeak(double *signal, int peakPos1, int peakPos2, int referencePeak)
{
  double slope1 = fabs((signal[peakPos1]-signal[referencePeak])/abs(peakPos1-referencePeak));
  double slope2 = fabs((signal[peakPos2]-signal[referencePeak])/abs(peakPos2-referencePeak));
  
  if(signal[peakPos1]*signal[peakPos2] < 0)
  {
    if(signal[referencePeak] > 0)
    {
      if(signal[peakPos1] > 0)
      {
        if(slope1 > 1.2*slope2)
        {
          return peakPos1;
        }
        
        else
        {
          return peakPos2;
        }
        
      }
      
      else
      {
        if(slope2 > 1.2*slope1)
        {
          return peakPos2;
        }
        
        else
        {
          return peakPos1;
        }
        
      }
      
    }
    
    else
    {
      if(signal[peakPos1] < 0)
      {
        if(slope1 > 1.2*slope2)
        {
          return peakPos1;
        }
        
        else
        {
          return peakPos2;
        }
        
      }
      
      else
      {
        if(slope2 > 1.2*slope1)
        {
          return peakPos2;
        }
        
        else
        {
          return peakPos1;
        }
        
      }
      
    }
    
  }
  
  else
  {
    return cuiweiAlgorithm(signal, peakPos1, peakPos2, referencePeak);
  }
  
}

int selectCorrectAdjacentPeak(double *signal, int *possibleAdjacentPeaks, int referencePeak)
{
  int i;
  int numPeaks = possibleAdjacentPeaks[0];
  int possiblePeaks[numPeaks];
  int returnCorrectAdjacentPeak = 0;
  
  for(i = 0; i < numPeaks; i++)
  {
    possiblePeaks[i] = possibleAdjacentPeaks[i+1];
  }
  
  i = 0;
  returnCorrectAdjacentPeak = possiblePeaks[i++];
  
  while(i < numPeaks)
  {
    returnCorrectAdjacentPeak = removeRedundantPeak(signal, returnCorrectAdjacentPeak, possiblePeaks[i], referencePeak);
    i++;
  }	
  
  return(returnCorrectAdjacentPeak);
}

int * selectCorrectMML(double *signal, int maxPos1, int maxPos2, int minPos2, int minPos1)
{
  int *returnCorrectMML = malloc(sizeof(int)*2);
  
  if(minPos1 == minPos2 && maxPos1 == maxPos2)
  {
    returnCorrectMML[0] = maxPos1;
    returnCorrectMML[1] = minPos1;
  }
  
  if(minPos1 == minPos2 && maxPos1 != maxPos2)
  {
    returnCorrectMML[0] = removeRedundantPeak(signal, maxPos1, maxPos2, minPos1);
    returnCorrectMML[1] = minPos1;
  }
  
  if(minPos1 != minPos2 && maxPos1 == maxPos2)
  {
    returnCorrectMML[0] = maxPos1;
    returnCorrectMML[1] = removeRedundantPeak(signal, minPos1, minPos2, maxPos1);
  }
  
  if(minPos1 != minPos2 && maxPos1 != maxPos2)
  {		
    if(maxPos1 > maxPos2)
    {
      returnCorrectMML[0] = maxPos1;
      returnCorrectMML[1] = minPos1;
    }
    
    else
    {
      returnCorrectMML[0] = maxPos2;
      returnCorrectMML[1] = minPos2;
    }
    
  }
  
  return returnCorrectMML;
}

int * getMMLInterval(double *signal, int signalLength, int leftIndex, int rightIndex, int searchIntervalSize)
{
  int * arrayLocalMinimums = getAllLocalMinimumsInterval(signal, signalLength, leftIndex, rightIndex, searchIntervalSize);
  int * arrayLocalMaximums = getAllLocalMaximumsInterval(signal, signalLength, leftIndex, rightIndex, searchIntervalSize);
  int minPos = getArrayMinimum(signal, leftIndex, rightIndex);
  int maxPos = getArrayMaximum(signal, leftIndex, rightIndex);
  minPos = isMoreThanOnePeak(signal, leftIndex, rightIndex, minPos, maxPos);
  maxPos = isMoreThanOnePeak(signal, leftIndex, rightIndex, maxPos, minPos);
  int adjacentMin = selectCorrectAdjacentPeak(signal, arrayLocalMinimums, maxPos);
  int adjacentMax = selectCorrectAdjacentPeak(signal, arrayLocalMaximums, minPos);	
  int* returnMMLPositions = selectCorrectMML(signal, maxPos, adjacentMax, minPos, adjacentMin);
  free(arrayLocalMinimums);
  free(arrayLocalMaximums);
  return(returnMMLPositions);
}
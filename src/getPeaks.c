#include <stdlib.h>
#include <math.h>

double * getArrayAbs(double * array, int arrayLength)
{
  int i = 0;
  double * returnArrayAbs = (double *) malloc(sizeof(double)*arrayLength);
  
  while(i < arrayLength)
  {
    returnArrayAbs[i] = fabs(array[i]);
    i++;
  }
  
  return returnArrayAbs;
}

int getAbsMinimumInArray(double *array, int leftIndex, int rightIndex)
{
  double minValue = fabs(array[leftIndex]);
  int returnAbsMinimumArray = leftIndex;
  leftIndex++;
  
  while(leftIndex <= rightIndex)
  {	
    if(fabs(array[leftIndex]) < minValue)
    {
      minValue = fabs(array[leftIndex]);
      returnAbsMinimumArray = leftIndex;
    }
    
    leftIndex++;
  }
  
  return returnAbsMinimumArray;
}

int getArrayMinimum(double *array, int leftIndex, int rightIndex)
{
  double minValue = array[leftIndex];
  int returnArrayMinimum = leftIndex;
  
  while(leftIndex <= rightIndex)
  {
    if(array[leftIndex] < minValue)
    {
      minValue = array[leftIndex];
      returnArrayMinimum = leftIndex;
    }
    
    leftIndex++;
  }
  
  return returnArrayMinimum;
}

int getArrayMaximum(double *array, int leftIndex, int rightIndex)
{
  double maxValue = array[leftIndex];
  int returnArrayMaximum = leftIndex;
  
  while(leftIndex <= rightIndex)
  {
    if(array[leftIndex] > maxValue)
    {
      maxValue = array[leftIndex];
      returnArrayMaximum = leftIndex;
    }
    
    leftIndex++;
  }
  
  return returnArrayMaximum;
}

int getArrayMaximumInteger(int *array, int leftIndex, int rightIndex)
{
  int maxValue = array[leftIndex];
  int returnArrayMaximumInteger = leftIndex;
  
  while(leftIndex <= rightIndex)
  {
    if(array[leftIndex] > maxValue)
    {
      maxValue = array[leftIndex];
      returnArrayMaximumInteger = leftIndex;
    }
    
    leftIndex++;
  }
  
  return returnArrayMaximumInteger;
}

double getArrayAvg(int *array, int leftIndex, int rightIndex)
{
  int sum = 0;	
  int cont = 0;
  double returnArrayAvg = 0;
  
  while(leftIndex <= rightIndex)
  {
    sum += array[leftIndex];
    leftIndex++;
    cont++;
  }
  
  returnArrayAvg = sum/cont;
  return returnArrayAvg;
}

int * getAllLocalMaximumsInterval(double *signal, int signalLength, int leftIndex, int rightIndex, int searchIntervalSize)
{
  int i;
  int *maximumPeaksTemp = malloc(sizeof(int)*(rightIndex-leftIndex));
  int leftInside;
  int rightInside;
  int isPeak = 0;	
  int peakCont = 0;
  int initialLeft = leftIndex;
  
  for(i = 0; i < (rightIndex-leftIndex); i++)
  {
    maximumPeaksTemp[i] = 0;
  }
  
  while(leftIndex <= rightIndex)
  {
    leftInside = ((leftIndex-searchIntervalSize/2) < 0) ? 0 : (leftIndex-searchIntervalSize/2);
    rightInside = ((leftIndex+searchIntervalSize/2) > signalLength-1 ) ? signalLength-1: (leftIndex+searchIntervalSize/2);		
    isPeak = 1;
    
    while(leftInside <= rightInside)
    {
      if(leftInside == leftIndex)
      {
        leftInside++;
        continue;
      }
      
      if(signal[leftInside] > signal[leftIndex])
      {
        isPeak = 0;
        break;
      }
      
      leftInside++;
    }
    
    if(isPeak)
    {			
      maximumPeaksTemp[peakCont++] = leftIndex;
    }
    
    leftIndex++;
  }
  
  if(peakCont == 0)
  {
    maximumPeaksTemp[peakCont++] = getArrayMaximum(signal, initialLeft, rightIndex);		
  }
  
  int *returnMaximumPeaks = malloc(sizeof(int)*(peakCont+1));
  returnMaximumPeaks[0] = peakCont;
  
  for(i = 1; i < peakCont+1; i++)
  {
    returnMaximumPeaks[i] = maximumPeaksTemp[i-1];
  }
  
  free(maximumPeaksTemp);
  return(returnMaximumPeaks);
}

int * getAllLocalMinimumsInterval(double *signal, int signalLength, int leftIndex, int rightIndex, int searchIntervalSize)
{
  int i;
  int *minimumPeaksTemp = malloc(sizeof(int)*(rightIndex-leftIndex));
  int leftInside;
  int rightInside;
  int isPeak = 0;	
  int peakCont = 0;
  int initialLeft = leftIndex;
  
  for(i = 0; i < (rightIndex - leftIndex); i++)
  {
    minimumPeaksTemp[i] = 0;
  }
  
  while(leftIndex <= rightIndex)
  {
    leftInside = ((leftIndex-searchIntervalSize/2) < 0) ? 0 : (leftIndex-searchIntervalSize/2);
    rightInside = ((leftIndex+searchIntervalSize/2) > signalLength-1) ? signalLength-1: (leftIndex+searchIntervalSize/2);		
    isPeak = 1;
    
    while(leftInside <= rightInside)
    {
      if(leftInside == leftIndex)
      {
        leftInside++;
        continue;
      }
      
      if(signal[leftInside] < signal[leftIndex])
      {
        isPeak = 0;
        break;
      }
      
      leftInside++;
    }
    
    if(isPeak)
    {			
      minimumPeaksTemp[peakCont++] = leftIndex;
    }
    
    leftIndex++;
  }
  
  if(peakCont == 0)
  {
    minimumPeaksTemp[peakCont++] = getArrayMinimum(signal, initialLeft, rightIndex);		
  }
  
  int *returnMinimumPeaks = malloc(sizeof(int)*(peakCont+1));
  returnMinimumPeaks[0] = peakCont;
  
  for(i = 1; i < peakCont+1; i++)
  {
    returnMinimumPeaks[i] = minimumPeaksTemp[i-1];
  }
  
  free(minimumPeaksTemp);
  return(returnMinimumPeaks);
}

int isMoreThanOnePeak(double *array, int leftIndex, int rightIndex, int peakToEval, int referencePeak)
{
  while(leftIndex <= rightIndex)
  {
    if(array[leftIndex] == array[peakToEval] && leftIndex != peakToEval && (abs(leftIndex-referencePeak) < abs(peakToEval-referencePeak)))
    {				
      peakToEval = leftIndex;
    }
    
    leftIndex++;
  }
  
  return peakToEval;
}

int getArrayMiddlePosition(int sw, int initialPos, int searchForward)
{
  return (searchForward) ? initialPos+(sw/2): initialPos-(sw/2);
}

int getArrayBeginning(int sw, int middlePos, int initialPos, int searchForward)
{
  if(searchForward)
  {
    return ((middlePos-sw/2) <= initialPos) ? initialPos : (middlePos-sw/2);
  }
  
  else
  {
    return middlePos-sw/2;		
  }
  
}

int getArrayEnd(int sw, int middlePos, int initialPos, int searchForward)
{
  if(searchForward)
  {
    return middlePos+sw/2;
  }
  
  else
  {
    return ((middlePos+sw/2) >= initialPos) ? initialPos : (middlePos+sw/2);
  }
  
}

int isAPeak(double *signal, int arrayBeginning, int arrayEnd, int middlePos, int searchForMinimum)
{
  int i = 0;
  
  for(i = arrayBeginning; i <= arrayEnd; i++)
  {
    if(i == middlePos)
    {
      continue;
    }
    
    if(searchForMinimum && signal[i] < signal[middlePos])
    {
      return 0;
    }
    
    if(!searchForMinimum && signal[i] > signal[middlePos])
    {
      return 0;
    }	
    
  }
  
  return 1;
}

int isExceedLimit(int searchForward, int arrayBeginning, int arrayEnd, int lengthSignal)
{
  if(!searchForward && arrayBeginning <= 0)
  {		
    return 1;
  }
  
  if(searchForward && arrayEnd >= lengthSignal-1)
  {
    return 1;
  }
  
  return 0;
}

double * getLocalPeak(double *signal, int lengthSignal, int initialPos, int sw, int searchForward, int searchForMinimum)
{
  int arrayBeginning;
  sw = (sw%2 == 0) ? sw+1 : sw;
  int middlePos = getArrayMiddlePosition(sw, initialPos, searchForward);
  int arrayEnd;
  double * returnLocalPeak = (double *) malloc(2*sizeof(double));
  
  while(1)
  {		
    arrayBeginning = getArrayBeginning(sw, middlePos, initialPos, searchForward);
    arrayEnd = getArrayEnd(sw, middlePos, initialPos, searchForward);
    
    if(isAPeak(signal, arrayBeginning, arrayEnd, middlePos, searchForMinimum))
    { 
      returnLocalPeak[0] = middlePos;
      returnLocalPeak[1] = signal[middlePos];
      break;
    } 
    
    else
    { 
      middlePos = (searchForward) ? middlePos+1: middlePos-1;
    }
    
    if(isExceedLimit(searchForward, arrayBeginning, arrayEnd, lengthSignal))
    {
      returnLocalPeak[0] = 0;
      returnLocalPeak[1] = 0;
      break;
    }
    
  }
  
  return returnLocalPeak;
}
#include "getPeaks.h"
#include <math.h>
#include <stdlib.h>

int firstCriteria(double *signal, int signalLength, int initialPos, int searchForward, int thr)
{
  int initialSign = (signal[initialPos] >= 0.0) ? 1 : -1;
  int returnSearchPosition = initialPos;
  
  while(fabs(signal[returnSearchPosition]) > thr)
  {
    if((initialSign*signal[returnSearchPosition] < 0 ) || (returnSearchPosition == 0) || (returnSearchPosition >= signalLength-1))
    {
      break;
    }
    
    returnSearchPosition = (searchForward) ? returnSearchPosition+1 : returnSearchPosition-1;		
  }
  
  return returnSearchPosition;
}

int secondCriteria(double *signal, int signalLength, int initialPos, int searchForward, int sw)
{
  int returnSearchPosition = initialPos;
  double *returnedPeak;
  double *signalAbs = getArrayAbs(signal, signalLength);		
  int searchForMinimum; 
  
  if(returnSearchPosition != 0 && returnSearchPosition != (signalLength-1))
  {	
    returnedPeak = getLocalPeak(signalAbs, signalLength, returnSearchPosition, sw, searchForward, searchForMinimum = 1);		
    returnSearchPosition = returnedPeak[0];
    free(returnedPeak);
  }
  
  free(signalAbs);
  return returnSearchPosition;
}

int getOnset(double *signal, int signalLength, int peak, int pre, int sw, double thr)
{
  int searchForward;
  int onsetPosFirstCriteria = firstCriteria(signal, signalLength, pre, searchForward = 0, thr);
  int onsetPosSecondCriteria = secondCriteria(signal, signalLength, pre, searchForward = 0, sw);	
  int returnOnset =  abs(peak-onsetPosFirstCriteria) <= abs(peak-onsetPosSecondCriteria)  ? onsetPosFirstCriteria : onsetPosSecondCriteria;
  return returnOnset;
}

int getOffset(double *signal, int signalLength, int peak, int post, int sw, double thr)
{
  int searchForward;
  int offsetPosFirstCriteria = firstCriteria(signal, signalLength, post, searchForward = 1, thr);
  int offsetPosSecondCriteria = secondCriteria(signal, signalLength, post, searchForward = 1, sw);	
  int returnOffset = abs(peak-offsetPosFirstCriteria) <= abs(peak-offsetPosSecondCriteria) ? offsetPosFirstCriteria : offsetPosSecondCriteria;
  return returnOffset;
}

double * getQRSOnsetOffsetPositions(double *scale2, int lengthScale2, double *nFirstnLast, int *nQRSPositions, int numQRSPositions)
{
  
  int i = 0;
  int nFirstPos;
  int contPos = 0;
  double nFirstValue;
  int nLastPos; 
  double nLastValue;
  double thrQRSOnset;
  double thrQRSOffset;	
  int sw = 3;
  double *returnQRSOnsetOffset = (double *) malloc(sizeof(double) * 2*numQRSPositions);
  int returnPos = 0;
  
  while(i < numQRSPositions)
  {
    nFirstPos = nFirstnLast[contPos++];
    nFirstValue = scale2[nFirstPos];
    nLastPos = nFirstnLast[contPos++];
    nLastValue = scale2[nLastPos];
    thrQRSOnset = (nFirstValue > 0.0) ? 0.05*fabs(nFirstValue) : 0.07*fabs(nFirstValue);
    thrQRSOffset = (nLastValue > 0.0) ? 0.125*fabs(nLastValue) : 0.71*fabs(nLastValue);
    returnQRSOnsetOffset[returnPos++] = getOnset(scale2, lengthScale2, nQRSPositions[i], nFirstPos, sw, thrQRSOnset);
    returnQRSOnsetOffset[returnPos++] = getOffset(scale2, lengthScale2, nQRSPositions[i], nLastPos, sw, thrQRSOffset);
    i++;
  }
  
  return returnQRSOnsetOffset;
}

void getPOnsetOffsetInScale(double *scale, int lengthScale, int pPeak, int pPre, int pPost, int intervalSizeToSearch, int *onsetPos, int *offsetPos, int pPositionIndex, int scaleFactor)
{
  double thr = 0.5*fabs(scale[pPre]);
  onsetPos[pPositionIndex] = getOnset(scale, lengthScale, pPeak, pPre, intervalSizeToSearch, thr)*scaleFactor;
  thr = 0.9*fabs(scale[pPost]);
  offsetPos[pPositionIndex] = getOffset(scale, lengthScale, pPeak, pPost, intervalSizeToSearch, thr)*scaleFactor;
}

void getPOnsetOffset(double *scale4, int lengthScale4, double *scale5, int lengthScale5, int pPeak, int modulus1, int modulus2, int *onsetPos, int *offsetPos, int pPositionIndex, int scale)
{	
  int scaleFactor = pow(2, scale)-pow(2, scale-1);
  int pPeakScale = pPeak/scaleFactor;
  int pPre = (modulus1 < modulus2) ? modulus1 : modulus2;
  int pPreScale = pPre/scaleFactor;
  int pPost = (modulus1 > modulus2) ? modulus1 : modulus2;
  int pPostScale = pPost/scaleFactor;
  int intervalSizeToSearch = 3;	
  
  if(scale == 4)
  {
    getPOnsetOffsetInScale(scale4, lengthScale4, pPeakScale, pPreScale, pPostScale, intervalSizeToSearch, onsetPos, offsetPos, pPositionIndex, scaleFactor); 
  }
  
  if(scale == 5)
  {
    getPOnsetOffsetInScale(scale5, lengthScale5, pPeakScale, pPreScale, pPostScale, intervalSizeToSearch, onsetPos, offsetPos, pPositionIndex, scaleFactor);
  }
  
}

void getTOnsetOffsetInScale(double *scale, int lengthScale, int tPeak, int tPre, int tPost, int intervalSizeToSearch, int *onsetPos, int *offsetPos, int tPositionIndex, int scaleFactor)
{
  double thr = 0.5*fabs(scale[tPre]);
  onsetPos[tPositionIndex] = getOnset(scale, lengthScale, tPeak, tPre, intervalSizeToSearch, thr)*scaleFactor;
  thr = 0.9*fabs(scale[tPost]);
  offsetPos[tPositionIndex] = getOffset(scale, lengthScale, tPeak, tPost, intervalSizeToSearch, thr)*scaleFactor;
}

void getTOnsetOffset(double *scale4, int lengthScale4, double *scale5, int lengthScale5, int tPeak, int modulus1, int modulus2, int *onsetPos, int *offsetPos, int tPositionIndex, int scale)
{	
  int scaleFactor = pow(2, scale)-pow(2, scale-1);
  int tPeakScale = tPeak/scaleFactor;	
  int tPre = (modulus1 < modulus2) ? modulus1 : modulus2;
  int tPreScale = tPre/scaleFactor;
  int tPost = (modulus1 > modulus2) ? modulus1 : modulus2;
  int tPostScale = tPost/scaleFactor;
  int intervalSizeToSearch = 3;
  
  if(scale == 4)
  {
    getTOnsetOffsetInScale(scale4, lengthScale4, tPeakScale, tPreScale, tPostScale, intervalSizeToSearch, onsetPos, offsetPos, tPositionIndex, scaleFactor); 
  }
  
  if(scale == 5)
  {
    getTOnsetOffsetInScale(scale5, lengthScale5, tPeakScale, tPreScale, tPostScale, intervalSizeToSearch, onsetPos, offsetPos, tPositionIndex, scaleFactor);
  }
  
}
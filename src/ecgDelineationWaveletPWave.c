#include "ecgDelineationWaveletGetMML.h"
#include "getPeaks.h"
#include "ecgDelineationWaveletGetOnsetOffset.h"
#include <math.h>
#include <stdio.h>
#include <stdlib.h>

int * getPWaveFromInterval(double *signal, int lengthSignal, int leftIndex, int rightIndex, int qrsOnset, int scale)
{
  int scaleFactor = pow(2, scale)-pow(2, scale-1);
  int rightInd = (round(rightIndex/scaleFactor) > (lengthSignal-1) ? (lengthSignal-1) : round(rightIndex/scaleFactor));
  int qrsOns = round(qrsOnset/scaleFactor);
  int leftInd = (ceil(leftIndex/scaleFactor) < 0) ? 0 : ceil(leftIndex/scaleFactor);
  int searchWindow = 3;	
  double finalMaxAbsValue = 0.0;
  double thr = 0.0;
  int pPeakPos = 0;
  
  if(rightInd >= qrsOns)
  {
    rightInd = qrsOns-1;
  }
  
  int *mmlPositions = getMMLInterval(signal, lengthSignal, leftInd, rightInd, searchWindow);
  int maxPos = mmlPositions[0];
  int minPos = mmlPositions[1];
  free(mmlPositions);	
  double absMaxValue = fabs(signal[maxPos]);
  double absMinValue = fabs(signal[minPos]);
  
  if(minPos != 0 && maxPos != 0)
  { 
    finalMaxAbsValue = (absMaxValue >= absMinValue) ? absMaxValue : absMinValue;
    thr = 0.125*finalMaxAbsValue;
    
    if(absMaxValue > thr && absMinValue > thr)
    {
      
      if(minPos < maxPos)
      {
        pPeakPos = getAbsMinimumInArray(signal, minPos, maxPos);
      }
      
      else
      {
        pPeakPos = getAbsMinimumInArray(signal, maxPos, minPos);
      }
      
      pPeakPos = pPeakPos*scaleFactor;
      maxPos = maxPos*scaleFactor;
      minPos = minPos*scaleFactor;				
    }
    
  }
  
  int * returnPWave = malloc(sizeof(int)*3);
  returnPWave[0] = pPeakPos;
  returnPWave[1] = maxPos;
  returnPWave[2] = minPos;
  return returnPWave;
}

void placePAtScale(double *waveletScale, int lengthScale, int swBegin, int swEnd, int qrsOnsetPos, int * pPositionReturn, int *maxPosition, int *minPosition, int pPositionIndex, int scale, int *leftIndexP, int *rightIndexP)
{
  int *pWave = getPWaveFromInterval(waveletScale, lengthScale, swBegin, swEnd, qrsOnsetPos, scale = 3);
  
  if(pWave[0] != 0)
  {
    pPositionReturn[pPositionIndex] = pWave[0];
    maxPosition[pPositionIndex] = pWave[1];
    minPosition[pPositionIndex] = pWave[2];
    leftIndexP[pPositionIndex] = swBegin;
    rightIndexP[pPositionIndex] = swEnd;
  }
  
  free(pWave);
}

void delineatePWave(double *waveletScale3, double *waveletScale4, double *waveletScale5, int *lengthScale3, int *lengthScale4, int *lengthScale5, int *rPositions, int *lengthRPositions, int *qrsOnsetPositions, int *initialRRMean, int *rrSize, int *pOnsetPositionReturn, int *pOffsetPositionReturn, int *pPositionReturn, int *basalBeatLength)
{
  int min = 0;
  int initRRMean = *initialRRMean;
  int bBeatLength = *basalBeatLength;
  int iswBegin = 0;
  int rCont = 0;
  int iswEnd = 0;
  int *pWave;
  int scale = 0;
  int pPositionIndex = 0;
  int maxPosition[1];
  int minPosition[1];
  int leftIndexP[1];
  int rightIndexP[1];
  
  iswBegin = 0;
  iswEnd = round(0.35*bBeatLength);
  	
  pWave = getPWaveFromInterval(waveletScale4, *lengthScale4, iswBegin, iswEnd, qrsOnsetPositions[rCont], scale = 4);
  
  if( pWave[0] == 0)
  {
    free(pWave);
    pWave = getPWaveFromInterval(waveletScale5, *lengthScale5, iswBegin, iswEnd, qrsOnsetPositions[rCont], scale = 5);
  }
  
  if(pWave[0] != 0)
  {
    getPOnsetOffset(waveletScale4, *lengthScale4, waveletScale5, *lengthScale5, pWave[0], pWave[1], pWave[2], pOnsetPositionReturn, pOffsetPositionReturn, pPositionIndex, scale);
    pPositionReturn[pPositionIndex] = pWave[0];
    maxPosition[pPositionIndex] = pWave[1];
    minPosition[pPositionIndex] = pWave[2];
    leftIndexP[pPositionIndex] = iswBegin;
    rightIndexP[pPositionIndex] = iswEnd;
    free(pWave);
    placePAtScale(waveletScale3, *lengthScale3, iswBegin, iswEnd, qrsOnsetPositions[rCont], pPositionReturn, maxPosition, minPosition, pPositionIndex, scale = 3, leftIndexP, rightIndexP);
  }
  
  else
  {
    free(pWave);
  }
  
}

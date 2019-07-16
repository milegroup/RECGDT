#include "ecgDelineationWaveletGetMML.h"
#include "getPeaks.h"
#include "ecgDelineationWaveletGetOnsetOffset.h"
#include <math.h>
#include <stdio.h>
#include <stdlib.h>

int * getTWaveFromInterval(double *signal, int lengthSignal, int leftIndex, int rightIndex, int scale)
{
  int scaleFactor = pow(2, scale)-pow(2, scale-1);
  int leftInd = (ceil(leftIndex/scaleFactor) < 0) ? 0 : ceil(leftIndex/scaleFactor);
  int rightInd = (round(rightIndex/scaleFactor) > (lengthSignal-1) ? (lengthSignal-1) : round(rightIndex/scaleFactor));
  int searchWindow = 3;  
  int *mmlPositions = getMMLInterval(signal, lengthSignal, leftInd, rightInd, searchWindow);
  int maxPos = mmlPositions[0];
  int minPos = mmlPositions[1];
  free(mmlPositions);	
  double finalMaxAbsValue = 0.0;
  double absMaxValue = fabs(signal[maxPos]);
  double absMinValue = fabs(signal[minPos]);
  double thr = 0.0;
  int tPeakPos = 0;
  
  if(minPos != 0 && maxPos != 0)
  { 
    finalMaxAbsValue = (absMaxValue >= absMinValue) ? absMaxValue : absMinValue;
    thr = 0.125*finalMaxAbsValue;
    
    if(absMaxValue >= thr && absMinValue >= thr)
    {
      if(minPos < maxPos)
      {
        tPeakPos = getAbsMinimumInArray(signal, minPos, maxPos);				
      }
      
      else
      {
        tPeakPos = getAbsMinimumInArray(signal, maxPos, minPos);	
      }
      
      tPeakPos = tPeakPos*scaleFactor;
      maxPos = maxPos*scaleFactor;
      minPos = minPos*scaleFactor;		
    }
    
  }
  
  int * returnTWave = malloc(sizeof(int)*3);
  returnTWave[0] = tPeakPos;
  returnTWave[1] = maxPos;
  returnTWave[2] = minPos;
  return(returnTWave);
}

void placeTAtScale(double *waveletScale3, int lengthScale3, int swBegin, int swEnd, int * tPositionReturn, int *maxPosition, int *minPosition, int tPositionIndex, int scale, int *leftIndexT, int *rightIndexT)
{
  int *tWave = getTWaveFromInterval(waveletScale3, lengthScale3, swBegin, swEnd, scale = 3);
  
  if(tWave[0] != 0)
  {
    tPositionReturn[tPositionIndex] = tWave[0];
    maxPosition[tPositionIndex] = tWave[1];
    minPosition[tPositionIndex] = tWave[2];
    leftIndexT[tPositionIndex] = swBegin;
    rightIndexT[tPositionIndex] = swEnd;
  }
  
  free(tWave);
}

void delineateTWave(double *waveletScale3, double *waveletScale4, double *waveletScale5, int *lengthScale3, int *lengthScale4, int *lengthScale5, int *rPositions, int *lengthRPositions, int *qrsOnsetPositions, int *qrsOffsetPositions, int *initialRRMean, int *rrSize, int *tOnsetPositionReturn, int *tOffsetPositionReturn, int *tPositionReturn, int *basalBeatLength)
{
  int rPosition = 0;
  int rCont = 0;
  int arrayRRIndex = 0;
  int rrArraySize = *rrSize;
  int bBeatLength = *basalBeatLength;
  int prevRR = 0;
  int prevRPosition = 0;
  int swBegin = 0;
  int swEnd = 0;
  int *tWave;
  int scale = 0;
  int tPositionIndex = 0;
  int maxPosition[1];
  int minPosition[1];
  int leftIndexT[1];
  int rightIndexT[1];
  int * qtcArray = malloc(sizeof(int)*rrArraySize);

  rPosition = rPositions[rCont];
  arrayRRIndex = rCont%rrArraySize;
  prevRR = rPosition-prevRPosition;
  prevRPosition = rPosition;
  swBegin = round(0.50*bBeatLength);
  swEnd = round(0.98*bBeatLength);
  tWave = getTWaveFromInterval(waveletScale4, *lengthScale4, swBegin, swEnd, scale = 4);		
  
  if(tWave[0] == 0)
  {
    free(tWave);
    tWave = getTWaveFromInterval(waveletScale5, *lengthScale5, swBegin, swEnd, scale = 5);
  }
  
  if(tWave != 0)
  {
    getTOnsetOffset(waveletScale4, *lengthScale4, waveletScale5, *lengthScale5, tWave[0], tWave[1], tWave[2], tOnsetPositionReturn, tOffsetPositionReturn, tPositionIndex, scale);
    tPositionReturn[tPositionIndex] = tWave[0];
    maxPosition[tPositionIndex] = tWave[1];
    minPosition[tPositionIndex] = tWave[2];
    leftIndexT[tPositionIndex] = swBegin;
    rightIndexT[tPositionIndex] = swEnd;
    free(tWave);
    placeTAtScale(waveletScale3, *lengthScale3, swBegin, swEnd, tPositionReturn, maxPosition, minPosition, tPositionIndex, scale = 3, leftIndexT, rightIndexT);
    tPositionIndex++;
    qtcArray[arrayRRIndex] = (tPositionReturn[tPositionIndex-1]-qrsOnsetPositions[rCont-1])/sqrt(prevRR);
  }
  
  else
  {
    free(tWave);	
  }
  
  free(qtcArray);
}

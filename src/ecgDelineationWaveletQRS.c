#include "ecgDelineationWaveletGetMML.h"
#include "getPeaks.h"
#include "ecgDelineationWaveletGetOnsetOffset.h"
#include <stdlib.h>
#include <math.h>

int getLeftIndex(int qrsPosition, int qrsLength)
{
	return (qrsPosition-qrsLength < 0) ? 0 : qrsPosition-qrsLength;
}

int getRightIndex(int qrsPosition, int qrsLength, int signalLength)
{
	return (qrsPosition+qrsLength > signalLength) ? signalLength : qrsPosition+qrsLength;
}

double * getNpreNpostPositions(double *scale2, int lengthScale2, int *nQRSPositions, int numQRSPositions, int qrsLength)
{
	int i;
	int leftIndex;
	int rightIndex;
	int *mmlPositions;
	int searchWindow = 3;
	int maxPos;
	int minPos;
	int returnPos = 0;
	double * returnNpreNpostPositions = (double *) malloc(sizeof(double) * 2*numQRSPositions);
	
	for(i = 0; i < numQRSPositions; i++)
	{
		leftIndex = getLeftIndex(nQRSPositions[i]/2, qrsLength/2);
		rightIndex = getRightIndex(nQRSPositions[i]/2, qrsLength/2, lengthScale2);
		mmlPositions =  getMMLInterval(scale2, lengthScale2, leftIndex, rightIndex, searchWindow);
		maxPos = mmlPositions[0];
		minPos = mmlPositions[1];
		free(mmlPositions);
		returnNpreNpostPositions[returnPos++] = (minPos < maxPos) ? minPos : maxPos;
		returnNpreNpostPositions[returnPos++] = (minPos < maxPos) ? maxPos : minPos;
	}
	
	return returnNpreNpostPositions;	
}

double * getNFirstNlastPositions(double *scale2, int lengthScale2, double *nPrenPost, int intervalSize, int numQRSPositions)
{
	int rightIndex = intervalSize;
	int nPrenPostCont = 0;
	int nPrePos = (int) nPrenPost[nPrenPostCont++];
	double nPreValue = scale2[nPrePos];
	int nPostPos = (int) nPrenPost[nPrenPostCont++];
	double nPostValue = scale2[nPostPos];
	double * returnedPeak;
	int sw = 3;
	int searchForward;
	int searchForMinimum;
	double * returnNFirstNlastPositions = (double *) malloc(sizeof(double) * 2*numQRSPositions);
	int returnPos = 0;

	while(rightIndex < (lengthScale2 -1))
	{
		while(nPrePos < rightIndex && nPostPos < rightIndex && nPrePos != 0 && nPostPos != 0)
		{			
			if(nPreValue > 0)
			{
				returnedPeak = (double *) getLocalPeak(scale2, lengthScale2, nPrePos, sw, searchForward = 0, searchForMinimum = 1);
				returnNFirstNlastPositions[returnPos++] = returnedPeak[0];
			}
			
			else
			{
				returnedPeak = (double *) getLocalPeak(scale2, lengthScale2, nPrePos, sw, searchForward = 0, searchForMinimum = 0);
				returnNFirstNlastPositions[returnPos++] = returnedPeak[0];
			}
			
			free(returnedPeak);
	
			if(nPostValue > 0)
			{
				returnedPeak = (double *) getLocalPeak(scale2, lengthScale2, nPostPos, sw, searchForward = 1, searchForMinimum = 1);
				returnNFirstNlastPositions[returnPos++] = returnedPeak[0];
			}
			
			else
			{
				returnedPeak = (double *) getLocalPeak(scale2, lengthScale2, nPostPos, sw, searchForward = 1, searchForMinimum = 0);
				returnNFirstNlastPositions[returnPos++] = returnedPeak[0];
			}
			
			free(returnedPeak);						
			nPrePos = (int) nPrenPost[nPrenPostCont++];
			nPreValue = scale2[nPrePos];
			nPostPos = (int) nPrenPost[nPrenPostCont++];
			nPostValue = scale2[nPostPos];
		}		

		if(rightIndex == lengthScale2-2)
		{
			break;
		}
		
		rightIndex += intervalSize;		
		
		if(rightIndex > (lengthScale2-1))
		{
			rightIndex = lengthScale2-2;
		}
		
	}	
	
	return returnNFirstNlastPositions;
}

int relocatePeaksToAnotherScale(double *newScale, int newScaleLength, double *currentScale, int currentPos, int sw, int scaleFactor, int isBiggerScale)
{
	int newPos = (isBiggerScale) ? currentPos*scaleFactor : currentPos/scaleFactor;
	double currentValue = currentScale[currentPos];
	int returnPeaksToAnotherScale;
	int leftIndex = (newPos-sw < 0) ? 0 : newPos-sw;
	int rightIndex = (newPos+sw > newScaleLength-1) ? newScaleLength-1 : newPos+sw;
	
	if(currentValue >= 0)
	{
		returnPeaksToAnotherScale = getArrayMaximum(newScale, leftIndex, rightIndex);
	}
	
	else
	{
		returnPeaksToAnotherScale = getArrayMinimum(newScale, leftIndex, rightIndex);
	}

	return returnPeaksToAnotherScale;
}

double * getQPeakSPeakPositions(double *scale1, int lengthScale1, double *scale2, double *nPrenPost, double *nFirstnLast, int numBeats)
{
	int i;
	int nPrePos;
	int nPrenPostCont = 0;
	int nPostPos;
	int nFirstPos;	
	int nFirstnLastCont = 0;
	int nLastPos;
	int sw = 2;
	int scaleFactor;
	int isBiggerScale;
	double * returnQPeakSPeak = (double *) malloc(sizeof(double) * 2*numBeats);
	int returnPos = 0;
	
	for(i = 0; i < numBeats; i++)
	{
		nPrePos = nPrenPost[nPrenPostCont++];
		nPostPos = nPrenPost[nPrenPostCont++] ;
		nFirstPos = nFirstnLast[nFirstnLastCont++];
		nLastPos = nFirstnLast[nFirstnLastCont++];
		nPrePos = relocatePeaksToAnotherScale(scale1, lengthScale1, scale2, nPrePos, sw, scaleFactor = 2, isBiggerScale = 1);
		nPostPos = relocatePeaksToAnotherScale(scale1, lengthScale1, scale2, nPostPos, sw, scaleFactor = 2, isBiggerScale = 1);
		nFirstPos = relocatePeaksToAnotherScale(scale1, lengthScale1, scale2, nFirstPos, sw, scaleFactor = 2, isBiggerScale = 1);
		nLastPos = relocatePeaksToAnotherScale(scale1, lengthScale1, scale2, nLastPos, sw, scaleFactor = 2, isBiggerScale = 1);
		returnQPeakSPeak[returnPos++] = getAbsMinimumInArray(scale1, nFirstPos, nPrePos);
		returnQPeakSPeak[returnPos++] = getAbsMinimumInArray(scale1, nPostPos, nLastPos);
	}
	
	return returnQPeakSPeak;
}

void delineateQRS(double *scale1, double *scale2, int *lengthScale1, int *lengthScale2, int *nQRSPositions, int *numQRSPositions, int *qrsLength, int *intervalSize, int *returnOnsetPositions, int *returnOffsetPositions, int *returnQpeakPositions, int *returnSpeakPositions)
{	
	double * nPrenPost = (double *) getNpreNpostPositions(scale2, *lengthScale2, nQRSPositions, *numQRSPositions, *qrsLength);
	double * nFirstnLast = (double *) getNFirstNlastPositions(scale2, *lengthScale2, nPrenPost, *intervalSize, *numQRSPositions);		
	double * QPeakSPeak = (double *) getQPeakSPeakPositions(scale1, *lengthScale1, scale2, nPrenPost, nFirstnLast, *numQRSPositions);
	double * QRSOnsetOffset = (double *) getQRSOnsetOffsetPositions(scale2, *lengthScale2, nFirstnLast, nQRSPositions, *numQRSPositions);
	int i = 0;
	int cont = 0;	

	while(i < *numQRSPositions)
	{
		returnOnsetPositions[i] = ((int) QRSOnsetOffset[cont])*2; 
		returnQpeakPositions[i] = (int) QPeakSPeak[cont];
		i++;
		cont += 2;
	}
	
	i = 0;
	cont = 1;
	
	while(i < *numQRSPositions)
	{
		returnOffsetPositions[i] = ((int) QRSOnsetOffset[cont])*2;
		returnSpeakPositions[i] = (int) QPeakSPeak[cont];
		i++;
		cont += 2;
	}

	free(QRSOnsetOffset);
	free(QPeakSPeak);
	free(nPrenPost);
	free(nFirstnLast);
}
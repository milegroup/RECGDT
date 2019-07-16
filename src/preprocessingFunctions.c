#include <stdlib.h>

/*Low-pass filter to reduce signal noise*/
void lowPassFilter(double *lpfSignal, double *initialSamples, int *channelLength)
{
  int i = 12;
  
  while (i < *channelLength)
  {
    lpfSignal[i] = 2 * lpfSignal[i-1] - lpfSignal[i-2] + initialSamples[i] - 2 * initialSamples[i-6] + initialSamples[i-12];		
    i++;
  }
}

/*High-pass filter to attenuate P & T waves and baseline drift*/
void highPassFilter(double *hpfSignal, double *lpfSignal, int *channelLength)
{	
  int i = 32;
  
  while (i < *channelLength)
  {
    hpfSignal[i] = 32 * lpfSignal[i-16] - (1.0/32.0) * ((hpfSignal[i-1] + lpfSignal[i] - lpfSignal[i-32]));
    i++;
  }
}

/*Derivate of the result signal from the the previous filtering step*/
void derivative(double *derivativeSignal, double *hpfSignal, int *channelLength)
{	
  int i = 2;
  
  while (i < *channelLength)
  {
    derivativeSignal[i] = (0.125) * ((-1) * hpfSignal[i-2] - 2 * hpfSignal[i-1] + 2 * hpfSignal[i+1] + hpfSignal[i+2]) ;
    i++;
  }
}

/*Moving window integration function applied to the squared signal*/
void mwi(double *mwiSignal, double *squaredSignal, int *channelLength, double *windowSize)
{
  double mwiSum;
  double n = *windowSize;
  int i = *windowSize;
  int j = 0;
  
  while (i < (*channelLength-2))
  {
    mwiSum = 0;
    
    for (j = (i-n+1) ; j < i; j++ ) 
    {
      mwiSum += squaredSignal[j];
    }
    
    mwiSignal[i] = mwiSum;
    mwiSignal[i] = (1.0/n)*mwiSignal[i];
    i++;
  }
}
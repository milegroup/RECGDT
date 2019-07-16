void getQuadraticSplineWavelet250(double *signal, int *signalLength, double *W1, double *S1, double *W2, double *S2, double *W3, double *S3/*, double *W4, double *S4, double *W5, double *S5*/)
{
  //Scale 1
  int n = 1;
  
  while(n < (*signalLength-3))
  {
    S1[n] = (1/8.0) * (signal[n+2] + 3*signal[n+1] + 3*signal[n] + signal[n-1]);
    W1[n] = 2 * (signal[n+1] - signal[n]);
    n++;
  }
  
  //Scale 2
  n = 1;
  
  while(n < ((*signalLength>>1) - 5))
  {
    S2[n] = (1/8.0) * (signal[(n<<1) + 4] + 3*signal[(n<<1) + 2] + 3*signal[(n<<1)] + signal[(n<<1) - 2]);
    W2[n] = 2 * (signal[(n<<1) + 2] - signal[(n<<1)]);    
    n++;
  }
  
  //Scale 3
  n = 1;
  
  while(n < ((*signalLength>>2) - 9))
  {
    S3[n] = (1/8.0) * (signal[(n<<2) + 8] + 3*signal[(n<<2) + 4] + 3*signal[(n<<2)] + signal[(n<<2) - 4]);
    W3[n] = 2*(signal[(n<<2) + 4] - signal[(n<<2)]);
    n++;
  }
    
}
double * getArrayAbs(double * array, int arrayLength);

int getAbsMinimumInArray(double *array, int leftIndex, int rightIndex);

int getArrayMinimum(double *array, int leftIndex, int rightIndex);

int getArrayMaximum(double *array, int leftIndex, int rightIndex);

int getArrayMaximumInteger(int *array, int leftIndex, int rightIndex);

double getArrayAvg(int *array, int leftIndex, int rightIndex);

int * getAllLocalMaximumsInterval(double *signal, int signalLength, int leftIndex, int rightIndex, int searchIntervalSize);

int * getAllLocalMinimumsInterval(double *signal, int signalLength, int leftIndex, int rightIndex, int searchIntervalSize);

int isMoreThanOnePeak(double *array, int leftIndex, int rightIndex, int peakToEval, int referencePeak);

double * getLocalPeak(double *signal, int lengthSignal, int initialPos, int sw, int searchForward, int searchForMinimum);
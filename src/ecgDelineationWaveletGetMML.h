int cuiweiAlgorithm(double *signal, int peakPos1, int peakPos2, int referencePeak);

int removeRedundantPeak(double *signal, int peakPos1, int peakPos2, int referencePeak);

int selectCorrectAdjacentPeak(double *signal, int *possibleAdjacentPeaks, int currentPeak);

int * selectCorrectMML(double *signal, int maxPos1, int maxPos2, int minPos2, int minPos1);

int * getMMLInterval(double *signal, int signalLength, int leftIndex, int rightIndex, int searchIntervalSize);
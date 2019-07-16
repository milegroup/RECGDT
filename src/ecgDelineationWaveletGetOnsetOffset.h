int getOnset(double *signal, int signalLength, int peak, int pre, int sw, double thr);

int getOffset(double *signal, int signalLength, int peak, int post, int sw, double thr);

double * getQRSOnsetOffsetPositions(double *scale2, int lengthScale2, double *nFirstnLast, int *nQRSPositions, int numQRSPositions);

void getPOnsetOffset(double *scale4, int lengthScale4, double *scale5, int lengthScale5, int pPeak, int modulus1, int modulus2, int *onsetPos, int *offsetPos, int pPositionIndex, int scale);

void getTOnsetOffset(double *scale4, int lengthScale4, double *scale5, int lengthScale5, int tPeak, int modulus1, int modulus2, int *onsetPos, int *offsetPos, int tPositionIndex, int scale);

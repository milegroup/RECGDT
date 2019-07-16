#include <stdio.h>
#include <stdlib.h>

void getFileSize(char **fileName, long int *fileSize) 
{	
	FILE *file = NULL;
	int SAMPLES_SIZE = 2;
	long int comienzo, final;

	file = fopen(*fileName,"rb");
	
	if(! file)
	{
		fclose(file); 
		file = NULL;	
	}
	
	if((comienzo=ftell(file)) < 0)
	{  
		fclose(file); 
		file = NULL;
	}
	
	if(fseek(file, 0L, SEEK_END) != 0)
	{
		fclose(file); 
		file = NULL;
	}
	
	final = ftell(file);

	if(fseek(file, 0L, SEEK_SET) != 0)
	{
		fclose(file); 
		file = NULL;
	}

	*fileSize = (final - comienzo); 
	
	fclose(file); 
	file = NULL;
}

void readFile16(char **fileName, double *channel1, double *channel2, int *fileSize)
{		
	int i;	
	short int *tmpSamplesCh1, *tmpSamplesCh2;
	FILE *file = NULL;
	int readCh1 = 0;
	int readCh2 = 0;
	int numSamplesRead = 0; 
	int numBytesRead = 0;

	tmpSamplesCh1 = (short int *) calloc( *fileSize, sizeof(short int ) );
	tmpSamplesCh2 = (short int *) calloc( *fileSize, sizeof(short int ) );
	
	file=fopen(*fileName,"rb");

	fseek( file, 0L, SEEK_SET );
	
	for (i=0; i < *fileSize; i++)
	{
		readCh1 = fread(&tmpSamplesCh1[i],2,1,file);
		readCh2 = fread(&tmpSamplesCh2[i],2,1,file);
		
		if(readCh1 == 0 || readCh2 == 0)
			break;		

		if(readCh1 != 1 || readCh2 !=1)  
		{
			fclose(file); 	
			file = NULL;
			free(tmpSamplesCh1); 
			free(tmpSamplesCh2);
			tmpSamplesCh1 = NULL;
			tmpSamplesCh2 = NULL;
		}
		else
		{					
			numSamplesRead++; 
			numBytesRead = numBytesRead + 4; 
		}
		
	}

	if(numBytesRead != *fileSize)
	{ 
		fclose(file); 
		file = NULL;
	}
	else
	{
		for (i = 0; i < numSamplesRead ; i++)
		{
			channel1[i] =  (tmpSamplesCh1[i]);	
			channel2[i] = (tmpSamplesCh2[i]);
		}	
	}
	
	fclose(file); 	
	file = NULL;
	free(tmpSamplesCh1); 
	free(tmpSamplesCh2);
	tmpSamplesCh1 = NULL;
	tmpSamplesCh2 = NULL;	
}

void readFile212(char **fileName, double *channel1, double *channel2, int *fileSize)
{	
	int i;	
	short int *byte1, *byte2, *byte3;
	short int *byte2_1, *byte2_2;
	short int *samples1, *samples2;
	FILE *file = NULL;
	int b1 = 0;
	int b2 = 0;
	int b3 = 0;
	int numSamplesRead = 0;
	int numBytesRead = 0;

	byte1 = (short int *) calloc( *fileSize, sizeof(short int) );
	byte2 = (short int *) calloc( *fileSize, sizeof(short int) );
	byte3 = (short int *) calloc( *fileSize, sizeof(short int) );

	byte2_1 = (short int *) calloc( *fileSize, sizeof(short int));
	byte2_2 = (short int *) calloc( *fileSize, sizeof(short int));

	samples1 = (short int *) calloc( *fileSize, sizeof(short int));
	samples2 = (short int *) calloc( *fileSize, sizeof(short int));
	
	file = fopen(*fileName,"rb");


	fseek( file, 0L, SEEK_SET );
	
	for (i=0; i < *fileSize; i++)
	{
		b1 = fread(&byte1[i], 1, 1, file);  
		b2 = fread(&byte2[i], 1, 1, file); 
		b3 = fread(&byte3[i], 1, 1, file);

		byte2_1[i] = (byte2[i] & 0xF0) >> 4;
		byte2_2[i] = (byte2[i] & 0x0F);

		samples1[i] = (byte2_1[i] << 8) | byte1[i];
		samples2[i] = (byte2_2[i] << 8) | byte3[i];

		
		if(b1 == 0 || b2 == 0 || b3 == 0)
			break;		

		if(b1 != 1 || b2 != 1 || b3 != 1)  
		{
			fclose(file); 	
			file = NULL;
			free(byte1); 
			free(byte2);
			free(byte3);
			free(byte2_1);
			free(byte2_2);
			free(samples1);
			free(samples2);
			byte1 = NULL;
			byte2 = NULL;
			byte3 = NULL;
			byte2_1 = NULL;
			byte2_2 = NULL;
			samples1 = NULL;
			samples2 = NULL;
		}
		else
		{					
			numSamplesRead++; 
			numBytesRead = numBytesRead + 3;
		}

		
	}

	if(numBytesRead  != *fileSize)
	{
		fclose(file); 	
		file = NULL;
		free(byte1); 
		free(byte2);
		free(byte3);
		free(byte2_1);
		free(byte2_2);
		free(samples1);
		free(samples2);
		byte1 = NULL;
		byte2 = NULL;
		byte3 = NULL;
		byte2_1 = NULL;
		byte2_2 = NULL;
		samples1 = NULL;
		samples2 = NULL;
	}
	else
	{
		for (i = 0; i < numSamplesRead ; i++)
		{
			channel1[i] =  (samples1[i]);	
			channel2[i] = (samples2[i]);
		}	
	}
	
	fclose(file);  	
	file = NULL;
	free(byte1); 
	free(byte2);
	free(byte3);
	free(byte2_1);
	free(byte2_2);
	free(samples1);
	free(samples2);
	byte1 = NULL;
	byte2 = NULL;
	byte3 = NULL;
	byte2_1 = NULL;
	byte2_2 = NULL;
	samples1 = NULL;
	samples2 = NULL;
}

void convertToPhysicalUnits(int *dataSize, int *ADCGain, int *ADCZero, double *channelData)
{
	int i = 0;
	double gain = (1 /(double) *ADCGain);
	
	while(i < *dataSize)
	{
		channelData[i] = ((double) (channelData[i] - *ADCZero)) * gain;
		i++;
	}

}

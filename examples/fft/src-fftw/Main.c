
#include <sys/types.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "BMP.h"


extern void highpass2d_fftw(int width, int height, u_int8_t* image);
extern void highpass2d_jones(int width, int height, u_int8_t* image);

int main(int argc, char** argv)
{
	if(argc != 4) {
		printf("usage: highpass <algorithm> <input.bmp> <output.bmp>\n");
		printf("       algorithms: -fftw -jones\n");
		
		exit(1);
	}

	char* algName		= argv[1];
	char* fileNameIn	= argv[2];
	char* fileNameOut	= argv[3];

	// Decide what algorithm to use.
	void (*highpass2d)(int, int, u_int8_t*)	= 0;
	if      (strcmp(algName, "-fftw") == 0)
		highpass2d	= highpass2d_fftw;
	else if (strcmp(algName, "-jones") == 0)
		highpass2d	= highpass2d_jones;
	else {
		printf("unknown algorithm %s\n", algName);
		exit(1);
	}
	
	// Read the image.
	ImageRGB* image	= readBMP24(fileNameIn);

	// Transform it.
	highpass2d(image->width, image->height, image->red);
	highpass2d(image->width, image->height, image->green);
	highpass2d(image->width, image->height, image->blue);
	
	// Write it back to file.
	writeBMP24(fileNameOut, image);
}


#include <sys/types.h>
#include <stdlib.h>
#include <stdio.h>
#include "BMP.h"


// gettin' and puttin'
u_int32_t getWord32le(u_int8_t* buf)
{
	return	  ((u_int32_t)buf[3]) << 24
		| ((u_int32_t)buf[2]) << 16
		| ((u_int32_t)buf[1]) << 8
		|  (u_int32_t)buf[0];
}

u_int32_t getWord16le(u_int8_t* buf)
{
	return	  ((u_int16_t)buf[1]) << 8
		|  (u_int16_t)buf[0];
}

void putWord32le(u_int8_t* buf, u_int32_t x)
{
	buf[0]	= (u_int8_t)( x 	& (u_int32_t)0x0ff);
	buf[1]	= (u_int8_t)( (x >> 8) 	& (u_int32_t)0x0ff);
	buf[2]	= (u_int8_t)( (x >> 16) & (u_int32_t)0x0ff);
	buf[3]	= (u_int8_t)( (x >> 24) & (u_int32_t)0x0ff);
}

void putWord16le(u_int8_t* buf, u_int16_t x)
{
	buf[0]	= (u_int8_t)( x 	& (u_int16_t)0x0ff);
	buf[1]	= (u_int8_t)( (x >> 8) 	& (u_int16_t)0x0ff);
}


// Header sizes.
#define FILE_HEADER_SIZE  14
#define IMAGE_HEADER_SIZE 40


// Read an uncompressed 24bit BMP file.
ImageRGB* readBMP24(char* fileName)
{
	// Open the file.
	FILE* file	= fopen (fileName, "r");
	if(file == 0) {
		printf("loadBMP: can't open file\n");
		exit(1);
	}

	// Get the length of the file.
	fseek(file, 0L, SEEK_END);
	long filesize	= ftell(file);
	rewind(file);

	// Make sure it's got at least the headers
	if (filesize < FILE_HEADER_SIZE + IMAGE_HEADER_SIZE) {
		printf("loadBMP: short file\n");
		exit(1);
	}

	// Allocate a buffer to hold the contents.
	u_int8_t* buf	= (u_int8_t*)malloc(filesize);

	// Load the sucker in.
	fread (buf, filesize, 1, file);
	fclose(file);

	// Make sure it's really a 24bit uncompressed BMP file.
	if(buf[0] != 'B' || buf[1] != 'M') {
		printf("loadBMP: not a BMP file");
		exit(1);
	}

	u_int16_t bpp		= getWord16le(buf + FILE_HEADER_SIZE + 14);
	u_int32_t compression	= getWord32le(buf + FILE_HEADER_SIZE + 16);
	if(bpp != 24 || compression != 0) {
		printf("loadBMP: not an uncompressed 24bit file\n");
		printf("bpp         = %d\n", bpp);
		printf("compression = %d\n", compression);
		exit(1);
	}

	// Grab the width and height of the image.
	u_int32_t width		= getWord32le(buf + FILE_HEADER_SIZE + 4);
	u_int32_t height	= getWord32le(buf + FILE_HEADER_SIZE + 8);

	// Get the supposed size of the image data from the file
	u_int32_t imagesize	= getWord32le(buf + FILE_HEADER_SIZE + 20);

	if (FILE_HEADER_SIZE + IMAGE_HEADER_SIZE + imagesize != filesize) {
		printf("loadBMP: file sizes just don't add up\n");
		printf("imagesize = %d\n", imagesize);
		exit(1);
	}

	// Get a pointer to the image data
	u_int8_t* data		= buf + FILE_HEADER_SIZE + IMAGE_HEADER_SIZE;

	// Forget about handling lines with pad bytes.
	if ((width * 3) % 4 != 0) {
		printf("loadBMP: lines have pad bytes, write it yourself\n");
		exit(1);
	}

	// Allocate buffers for the RGB components.
	u_int8_t* red		= (u_int8_t*)malloc(width * height);
	u_int8_t* green		= (u_int8_t*)malloc(width * height);
	u_int8_t* blue		= (u_int8_t*)malloc(width * height);

	u_int8_t* iptr 		= data;
	u_int8_t* iptrmax	= data + imagesize;
	u_int8_t* rptr 		= red;
	u_int8_t* gptr		= green;
	u_int8_t* bptr 		= blue;

	// Copy out the component values;
	while(iptr < iptrmax) {
		*bptr++	= *iptr++;
		*gptr++	= *iptr++;
		*rptr++	= *iptr++;
	}

	ImageRGB *image	= (ImageRGB*)malloc(sizeof(ImageRGB));
	image->width	= width;
	image->height	= height;
	image->red	= red;
	image->green	= green;
	image->blue	= blue;

	free(buf);
	return (image);
}


// Write an image as an uncompressed 24bit RGB BMP file.
void	writeBMP24(char* fileName, ImageRGB* image)
{
	// Forget about handline lines with pad bytes
	if ((image->width * 3) % 4 != 0) {
		printf("writeBMP: lines have pad bytes, write it yourself\n");
		printf("    width  = %d\n", image->width);
		printf("    height = %d\n", image->height);
		exit(1);
	}

	int imagesize	= image->width * image->height * 3;
	int filesize	= FILE_HEADER_SIZE + IMAGE_HEADER_SIZE + imagesize;

	// Allocate a buffer for the image.
	u_int8_t* buf	= (u_int8_t*)malloc(filesize);

	// Make the file header
	buf[0]		= 'B';				// magic
	buf[1]		= 'M';
	putWord32le(buf+2, 	filesize);		// size of whole file
	putWord16le(buf+6, 	0);			// reserved
	putWord16le(buf+8,	0);			// reserved
	putWord32le(buf+10, 	FILE_HEADER_SIZE + IMAGE_HEADER_SIZE);
							// offset to start of pixel data

	// Make the image header
	u_int8_t* bufi	= buf + FILE_HEADER_SIZE;
	putWord32le(bufi, 	IMAGE_HEADER_SIZE);	// size of header
	putWord32le(bufi+4,	image->width);		// width of image
	putWord32le(bufi+8,	image->height);		// height of image
	putWord16le(bufi+12,	1);			// number of planes
	putWord16le(bufi+14,	24);			// bits per pixel
	putWord32le(bufi+16,	0);			// compression mode
	putWord32le(bufi+20,	imagesize);		// size of image data
	putWord32le(bufi+24,	2834);			// pels per meter (X)
	putWord32le(bufi+28,	2834);			// pels per meter (Y)
	putWord32le(bufi+32,	0);			// colors used
	putWord32le(bufi+36,	0);			// colors important

	// Make the image data
	u_int8_t* optr		= buf + FILE_HEADER_SIZE + IMAGE_HEADER_SIZE;
	u_int8_t* optrmax	= buf + filesize;
	u_int8_t* rptr 		= image->red;
	u_int8_t* gptr		= image->green;
	u_int8_t* bptr 		= image->blue;

	while(optr < optrmax) {
		*optr++		= *bptr++;
		*optr++		= *gptr++;
		*optr++		= *rptr++;
	}

	// Write the sucker out to file.
	FILE* file	= fopen(fileName, "w");
	fwrite(buf, filesize, 1, file);
	fclose(file);
}


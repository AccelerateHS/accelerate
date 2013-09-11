
#include <sys/types.h>
#include <math.h>
#include <fftw3.h>
#include "BMP.h"

#define PI 3.141592653589793238462643383

// Perform FFT on a 2d image.
void image_fftw(ImageRGB *image, double clipMag, u_int8_t *phase)
{
	// The size of the whole image.
	int width       = image->width;
	int height      = image->height;
	int size		= image->height * image->width;

	// Allocate input and output buffers
	fftw_complex* buf1	= (fftw_complex*) fftw_malloc(sizeof(fftw_complex) * size);
	fftw_complex* buf2	= (fftw_complex*) fftw_malloc(sizeof(fftw_complex) * size);

	// Copy in image data as real values for the transform.
	for(int i = 0; i < size; i++) {
		double r = 0.3  * (double)image->red[i];
		double g = 0.59 * (double)image->green[i];
		double b = 0.11 * (double)image->blue[i];

		buf1[i][0]	= (r + g + b)/255;
		buf1[i][1]	= 0;
	}

	// Transform to frequency space.
	fftw_plan pFwd = fftw_plan_dft_2d(width, height, buf1, buf2, FFTW_FORWARD, FFTW_ESTIMATE);
	fftw_execute(pFwd);
	fftw_destroy_plan(pFwd);

	// Clip the magnitude
	for (int i = 0; i < size; i++) {
		double re       = buf2[i][0];
		double im       = buf2[i][1];
		double mag      = sqrt (re*re + im*im);
		double clipped  = mag > clipMag ? 1.0 : mag / clipMag;
		int v           = round(clipped*255);
		image->red[i]   = v;
		image->green[i] = v;
		image->blue[i]  = v;

		double p        = atan(im/re);
		double scale    = (p + PI) / (2 * PI);
		phase[i]        = round(scale*255);
	}

	// Cleanup.
	fftw_free(buf1);
	fftw_free(buf2);
}

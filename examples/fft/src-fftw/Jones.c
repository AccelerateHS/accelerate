
#include <math.h>
#include <stdlib.h>
#include <stdio.h>

// Based on 1d FFT code (c) Douglas L. Jones
// University of Illinois at Urbana-Champaign
// January 19, 1992. Creative Commons "Attribution" license.
void fft 	
	( int n		// length of vector.
	, int m		// log2 of length of vector.
	, double x[]	// real values.
	, double y[]	// imaginary values.
	, double sign)	// sign of transform. +1 fwd, -1 reverse.
{
	int 	i, j, k, n1, n2;
	double 	c, s, e, a, t1, t2;
	  
	/* bit-reverse */
	j 	= 0; 
	n2 	= n/2;
	for (i=1; i < n - 1; i++)
	{
  		n1 = n2;
  		while ( j >= n1 )
   		{
    			j	= j - n1;
    			n1	= n1/2;
   		}
  		j = j + n1;
               
  		if (i < j)
   		{
    			t1 	= x[i];
    			x[i] 	= x[j];
    			x[j] 	= t1;
    			t1	= y[i];
    			y[i]	= y[j];
    			y[j]	= t1;
   		}
	}
	             
	/* FFT */          
	n1 	= 0; 
	n2 	= 1;                             
	for (i=0; i < m; i++)
	{
  		n1	= n2;
  		n2 	= n2 + n2;
  		e	= -6.283185307179586/n2;
  		a	= 0.0;
                                             
  		for (j=0; j < n1; j++)
   		{
    			c =        cos(a);
    			s = sign * sin(a);
    			a = a + e;
                                            
    			for (k=j; k < n; k = k + n2)
     			{
      				t1 	= c*x[k+n1] - s*y[k+n1];
      				t2 	= s*x[k+n1] + c*y[k+n1];
      				x[k+n1] = x[k] - t1;
      				y[k+n1] = y[k] - t2;
      				x[k] 	= x[k] + t1;
      				y[k] 	= y[k] + t2;
     			}
   		}
	}                                      
}                          

// ----------------------------------------------------------------------------
// Transpose a square matrix.
void	transpose (int width, int height, double* dest, double* src)
{
	for(int j = 0; j < height; j++)
	for(int i = 0; i < width; i++) {
		dest[j + i*height]	= src[i + j*width];
	}
}


// Do a 2D fft of a square matrix.
void	fft2d (int width, int height, int m, double* re, double* im, double sign)
{
	// TODO: Not finished for rectangular matrices.
	//       Also check that width and height are multiples of 2.
	if (width != height) {
		printf("jones_fft2d: matrix not square\n");
		exit(1);
	}

	// Allocate temp buffers.
	int size	= width * height;
	double* rebuf	= (double*)malloc(sizeof(double) * size);
	double* imbuf	= (double*)malloc(sizeof(double) * size);

	// Get the log of the width and height.
	double log2width  = log2(width);
	double log2height = log2(height);

	// Transform every row.
	for(int j = 0; j < height; j++)
		fft(width, log2width, re + j*width, im + j*width, sign);

	// Transpose into new buffers.
	transpose(width, height, rebuf, re);
	transpose(width, height, imbuf, im);
	
	// Transform transposed columns.
	for(int j = 0; j < height; j++)
		fft(width, log2height, rebuf + j*width, imbuf + j*width, sign);

	// Transpose back into the original buffers.
	transpose(width, height, re, rebuf);
	transpose(width, height, im, imbuf);		
	
	// Cleanup
	free(rebuf);
	free(imbuf);
	
}


// Do highpass filtering on a square image.
//	The DC value is set to zero, but the higher frequencies are kept.	
void highpass2d_jones(int width, int height, u_int8_t* image)
{
	// TODO: Not finished for rectangular matrices.
	//       Also check that width and height are multiples of 2.
	if (width != height) {
		printf("jones_highpass2d: matrix not square\n");
		exit(1);
	}

	double 	m	= (int)log2(width);
	
	// The size of the whole image.
	int size		= height * width;

	// Allocate buffers for real and imaginary components.
	double* re	= (double*)malloc(sizeof(double) * size);
	double* im	= (double*)malloc(sizeof(double) * size);

	// Copy in image data as real values for the transform.
	for(int i = 0; i < size; i++) {
		re[i]	= (double)image[i];
		im[i]	= 0;
	}

	// Transform to frequency space.
	fft2d (width, height, m, re, im, 1);
	
	// Zap the DC value.
	re[0]	= 0;
	im[0]	= 0;

	// Transform back to image space.
	fft2d (width, height, m, re, im, -1);
	
	// Have to scale the output values to get back to the original.
	for(int i = 0; i < size; i++) {
		re[i]	= re[i] / size;
		im[i]	= im[i] / size;
	}

	// Copy the magnitude of the result back into the image.
	for(int i = 0; i < size; i++) {
		double xre	= re[i];
		double xim	= im[i];
		double mag	= sqrt (xre*xre + xim*xim);
		image[i]	= (u_int8_t)mag;
	}

	// Cleanup.
	free(re);
	free(im);
}


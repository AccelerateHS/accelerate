
#include <sys/types.h>


typedef struct ImageRGB_ {
	int width;
	int height;
	u_int8_t*	red;
	u_int8_t*	green;
	u_int8_t*	blue;
} ImageRGB;

ImageRGB* readBMP24 (char* fileName);
void	  writeBMP24(char* fileName, ImageRGB* image);


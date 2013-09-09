FFTW_LIB=/usr/local/lib/libfftw3.a
FFTW_INCLUDE=/usr/local/include

HIGHPASS2D_SRCS = \
	Main.c \
	FFTW.c \
	Jones.c \
	ImageFFTW.c \
	../../../libc/BMP.c \
	../../../libc/Timing.c

highpass2d : $(HIGHPASS2D_SRCS)
	@echo "* Building Highpass2D"
	gcc --std=c99 -O3 $+ $(FFTW_LIB) -I../../../libc -I$(FFTW_INCLUDE) $(CFLAGS) -o $@
	@echo
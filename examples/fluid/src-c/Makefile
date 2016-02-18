.PHONY: clean

CC=gcc
CFLAGS=-O3 -Wall -Wextra -pedantic
GLFLAGS=-lglut -lGL -lGLU

demo: demo.c solver.o
	$(CC) $(CFLAGS) $(GLFLAGS) -o demo demo.c solver.o

solver.o: solver.c
	$(CC) $(CFLAGS) -c solver.c

clean:
	rm -f demo solver.o

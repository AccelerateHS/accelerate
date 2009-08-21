HCFLAGS = -O -package accelerate

all:
	ghc $(HCFLAGS) -c Time.hs
	ghc $(HCFLAGS) -c SAXPY.hs
	ghc $(HCFLAGS) -c DotP.hs
	ghc $(HCFLAGS) -c Filter.hs
	ghc $(HCFLAGS) -c Main.hs
	ghc $(HCFLAGS) -o test Main.o Time.o SAXPY.o DotP.o Filter.o

clean:
	rm -f *.o *.hi time
	
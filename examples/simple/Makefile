
GHC	 = ghc
HCFLAGS  = -O2 -package accelerate
SRCDIR   = src
BUILDDIR = dist
HSMAIN   = src/Main.hs
TARGET   = test


all:
	@mkdir -p $(BUILDDIR)
	$(GHC) --make $(HCFLAGS) -odir $(BUILDDIR) -hidir $(BUILDDIR) -i$(SRCDIR) $(HSMAIN) -o $(TARGET)

clean:
	$(RM) -r $(BUILDDIR)
	$(RM) $(TARGET)


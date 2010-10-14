
GHC	 = ghc
HCFLAGS  = -O2 -Wall -package accelerate
SRCDIR   = src
BUILDDIR = dist
HSMAIN   = src/Main.hs
TARGET   = test-smvm


all: dirs
	$(GHC) --make $(HCFLAGS) -odir $(BUILDDIR) -hidir $(BUILDDIR) -i$(SRCDIR) $(HSMAIN) -o $(TARGET)

$(BUILDDIR)/%.o: %.cu
	nvcc -c -O2 $< -o $@

dirs:
	@mkdir -p $(BUILDDIR)/$(SRCDIR)

core:
	ghc-core -- $(HCFLAGS) -odir $(BUILDDIR) -hidir $(BUILDDIR) -i$(SRCDIR) $(HSMAIN) -o $(TARGET)

clean:
	$(RM) -r $(BUILDDIR)
	$(RM) $(TARGET)


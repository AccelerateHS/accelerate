
GHC	 = ghc
HCFLAGS  = -O2 -Wall -package accelerate
SRCDIR   = src
BUILDDIR = dist
HSMAIN   = src/Main.hs
TARGET   = test-blackscholes
CUBITS   = src/sdk.cu

OBJS     = $(CUBITS:%.cu=$(BUILDDIR)/%.o)

all: dirs $(OBJS)
	$(GHC) --make $(HCFLAGS) -odir $(BUILDDIR) -hidir $(BUILDDIR) -i$(SRCDIR) $(HSMAIN) $(CBITS) $(OBJS) -lcublas -lstdc++ -o $(TARGET)

$(BUILDDIR)/%.o: %.cu
	nvcc -c -O2 $< -o $@

dirs:
	@mkdir -p $(BUILDDIR)/$(SRCDIR)

clean:
	$(RM) -r $(BUILDDIR)
	$(RM) $(TARGET)


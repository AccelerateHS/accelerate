# vim: nospell

FROM tmcdonell/accelerate-llvm
MAINTAINER Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>

RUN apt-get update \
 && apt-get install -y \
      freeglut3-dev \
      libfftw3-dev \
      libgmp-dev

# Copy over just the cabal and stack file and install dependencies
WORKDIR /opt/accelerate-examples
COPY ./stack-8.0.yaml /opt/accelerate-examples/stack.yaml
COPY ./accelerate-examples.cabal /opt/accelerate-examples/
RUN stack build accelerate-examples \
  --only-dependencies \
  --flag accelerate-examples:-gui

# Copy over the source files and build
COPY . /opt/accelerate-examples
RUN stack install --flag accelerate-examples:-gui

CMD ["bash"]


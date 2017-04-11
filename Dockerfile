# vim: nospell

FROM fpco/stack-build:lts-8.0
MAINTAINER Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>

# Install llvm-4.0
#
RUN wget -O - http://apt.llvm.org/llvm-snapshot.gpg.key | apt-key add - \
 && add-apt-repository "deb http://apt.llvm.org/xenial/ llvm-toolchain-xenial-4.0 main" \
 && apt-get update \
 && apt-get install -y llvm-4.0-dev

# Copy over just the cabal and stack file and install dependencies
#
WORKDIR /opt/accelerate-examples
COPY ./stack-8.0.yaml /opt/accelerate-examples/stack.yaml
COPY ./accelerate-examples.cabal /opt/accelerate-examples/
RUN stack --system-ghc build accelerate-examples \
  --only-dependencies \
  --flag accelerate-fft:-llvm-ptx \
  --flag accelerate-examples:-llvm-ptx \
  --flag accelerate-examples:-gui

# Copy over the source files and build
#
COPY . /opt/accelerate-examples
RUN stack --system-ghc install \
  --flag accelerate-fft:-llvm-ptx \
  --flag accelerate-examples:-llvm-ptx \
  --flag accelerate-examples:-gui


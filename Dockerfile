# vim: nospell
FROM fpco/stack-build:lts-8.0
MAINTAINER Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>

# Install llvm-4.0
RUN wget -O - http://apt.llvm.org/llvm-snapshot.gpg.key | apt-key add -
RUN add-apt-repository "deb http://apt.llvm.org/xenial/ llvm-toolchain-xenial-4.0 main"
RUN apt-get update
RUN apt-get install -y llvm-4.0-dev

# Install ghc-8.0.2
RUN mkdir -p /root/.stack/global/
RUN echo "resolver: lts-8.0" > /root/.stack/global/stack.yaml
RUN stack setup

# Copy over just the cabal and stack file and install dependencies
WORKDIR /opt/accelerate-examples
COPY ./stack-8.0.yaml /opt/accelerate-examples/stack.yaml
COPY ./accelerate-examples.cabal /opt/accelerate-examples/
RUN stack build accelerate-examples \
  --only-dependencies \
  --flag accelerate-fft:-llvm-ptx \
  --flag accelerate-examples:-llvm-ptx

# Copy over the source files and build
COPY . /opt/accelerate-examples
RUN stack install \
  --flag accelerate-fft:-llvm-ptx \
  --flag accelerate-examples:-llvm-ptx

# Make the programs available via 'docker run tmcdonell/accelerate-examples <foo>'
ENV PATH /root/.local/bin:${PATH}


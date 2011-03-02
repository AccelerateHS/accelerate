# Make the accelerate-examples.cabal file from its template.
# Build the example programs.
#

ACC_DEPENDS = \
	accelerate	== 0.9.*, 	 \
	array		== 0.3.*, 	 \
	base		== 4.*,   	 \
	criterion	== 0.5.*, 	 \
	deepseq		== 1.1.*, 	 \
	directory	>= 1.0 && < 1.2, \
	mtl		>= 1.1 && < 3.0, \
	mwc-random	== 0.8.*,	 \
	vector		== 0.7.*

ACC_OPTIONS = \
	-Wall -O2

.PHONY : all clean
all : accelerate-examples.cabal
	cabal configure --user
	cabal build

inplace : accelerate-examples.cabal
	cabal configure --user --package-db ../dist/package.conf.inplace
	cabal build

clean : accelerate-examples.cabal
	@cabal clean
	@rm accelerate-examples.cabal

accelerate-examples.cabal : accelerate-examples.template Makefile
	@echo "* Making accelerate-examples.cabal"
	@cpp	-P -undef \
		-DACC_DEPENDS="$(ACC_DEPENDS)" \
		-DACC_OPTIONS="$(ACC_OPTIONS)" \
		accelerate-examples.template accelerate-examples.cabal


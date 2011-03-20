
.PHONY : all inplace clean
all :
	cabal configure --user
	cabal build

inplace :
	cabal configure --user --package-db ../dist/package.conf.inplace
	cabal build

clean :
	@cabal clean


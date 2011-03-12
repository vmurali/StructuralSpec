.PHONY: all clean

all:
	ghc --make -outputdir build StructuralSpec.hs

clean:
	rm -rf *.o *.hi build StructuralSpec lib/*.bi lib/*.bo

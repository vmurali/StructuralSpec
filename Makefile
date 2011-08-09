.PHONY: all clean

all:
	ghc --make -outputdir build StructuralSpec.hs

clean:
	rm -rf build StructuralSpec

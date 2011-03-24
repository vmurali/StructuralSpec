.PHONY: all cleanall clean

all:
	ghc --make -outputdir build StructuralSpec.hs

cleanall:
	rm -rf *.o *.hi build StructuralSpec lib/*.bi lib/*.bo lib/mk*.v lib/a.out lib/Library.bsv lib/RegFile.bsv

clean:
	rm -rf *.o *.hi lib/*.bi lib/*.bo lib/mk*.v lib/a.out lib/Library.bsv lib/RegFile.bsv

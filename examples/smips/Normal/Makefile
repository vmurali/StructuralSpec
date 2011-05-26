.PHONY: all clean

ROOT = $(HOME)/StructuralSpec

all:
	mkdir -p bsv
	mkdir -p bsv/bdir
	cd bsv; cp -f ../Primitive.bsv .; cd ..
	cd bsv; cp -f $(ROOT)/lib/*.v .; cd ..
	$(ROOT)/StructuralSpec -o bsv -i $(ROOT)/lib:.. Processor.spec
	cd bsv; bsc -u -verilog -bdir bdir -keep-fires -p +:$(ROOT)/lib:../.. -aggressive-conditions -Werror -g mkProcessor Processor.bsv +RTS -K1000m; cd ..
	cd bsv; bsc -u -verilog -bdir bdir -keep-fires -p +:$(ROOT)/lib:../.. -aggressive-conditions -e mkProcessor *.v; cd ..
	mv bsv/a.out .

clean:
	rm -rf bsv a.out

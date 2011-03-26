.PHONY: all clean

ROOT = /home/vmurali/StructuralSpec

all:
	mkdir -p bsv
	mkdir -p bsv/bdir
	cd bsv; cp -f ../Primitive.bsv .; cd ..
	cd bsv; cp -f $(ROOT)/lib/*.v .; cd ..
	$(ROOT)/StructuralSpec -o bsv -i $(ROOT)/lib:.. Processor.spec
	cd bsv; bsc -u -verilog -bdir bdir -p +:$(ROOT)/lib:../.. -aggressive-conditions -g mkProcessor Processor.bsv; cd ..
	cd bsv; bsc -u -verilog -bdir bdir -p +:$(ROOT)/lib:../.. -aggressive-conditions -e mkProcessor *.v; cd ..

clean:
	rm -rf bsv

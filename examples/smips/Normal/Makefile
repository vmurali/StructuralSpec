.PHONY: all clean

ROOT = /home/vmurali/StructuralSpec

all:
	mkdir -p bsv
	mkdir -p bsv/bdir
	cd bsv; ln -sf ../tests/memory.vmh; cd ..
	cd bsv; ln -sf ../Primitive.bsv; cd ..
	cd bsv; ln -sf $(ROOT)/lib/*.v .; cd ..
	$(ROOT)/StructuralSpec -o bsv -i $(ROOT)/lib:.. Processor.spec
	cd bsv; bsc -u -verilog -p +:$(ROOT)/lib:../.. -aggressive-conditions -Werror -g mkProcessor Processor.bsv; cd ..
	cd bsv; bsc -u -verilog -p +:$(ROOT)/lib:../.. -aggressive-conditions -Werror -e mkProcessor *.v; cd ..

clean:
	rm -rf bsv

.PHONY: all clean

ROOT = /home/vmurali/StructuralSpec

all:
	mkdir -p bsv
	mkdir -p bsv/bdir
	cd bsv; cp -f ../Primitive.bsv .; cd ..
	cd bsv; cp -f $(ROOT)/lib/*.v .; cd ..
	$(ROOT)/StructuralSpec -o bsv -i $(ROOT)/lib:.. Processor.spec
	#cd bsv; bsc -u -verilog -bdir bdir -keep-fires -p +:$(ROOT)/lib:../.. -aggressive-conditions -no-O -no-opt-ATS -no-opt-AndOr -no-opt-aggressive-inline -no-opt-bit-const -no-opt-bool -no-opt-final-pass -no-opt-if-mux -no-opt-join-defs -no-opt-mux -no-opt-mux-const -no-opt-mux-expand -no-opt-sched -no-opt-undetermined-vals -Werror -g mkProcessor Processor.bsv +RTS -K1000m; cd ..
	cd bsv; bsc -u -verilog -bdir bdir -keep-fires -p +:$(ROOT)/lib:../.. -aggressive-conditions -Werror -g mkProcessor Processor.bsv +RTS -K1000m; cd ..
	cd bsv; bsc -u -verilog -bdir bdir -keep-fires -p +:$(ROOT)/lib:../.. -aggressive-conditions -e mkProcessor *.v; cd ..
	mv bsv/a.out .

clean:
	rm -rf bsv a.out

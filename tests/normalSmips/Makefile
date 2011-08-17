%.:
	mkdir -p bsv
	mkdir -p bsv/bdir
	StructuralSpec -o bsv -i ${STRUCTURALSPEC_HOME}/lib:${STRUCTURALSPEC_HOME}/lib/multi $*.spec
	cd bsv && bsc -u -unsafe-always-ready -verilog -bdir bdir -p +:${STRUCTURALSPEC_HOME}/lib/multi:${STRUCTURALSPEC_HOME}/lib -aggressive-conditions -v95 -steps-warn-interval 100000000 $*.bsv +RTS -K1000m 2>&1 | ignoreBsc.pl

%: %.
	ln -sf -t bsv ${STRUCTURALSPEC_HOME}/lib/multi/*.v
	cd bsv && bsc -e mk$* *.v

clean:
	rm -rf bsv memory.vmh

.PHONY: clean

.DEFAULT_GOAL := Processor

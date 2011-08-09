%.:
	mkdir -p bsv
	mkdir -p bsv/bdir
	StructuralSpec -o bsv -i ${STRUCTURALSPEC_HOME}/lib $*.spec
	cd bsv && bsc -u -unsafe-always-ready -verilog -bdir bdir -p +:${STRUCTURALSPEC_HOME}/lib/single:${STRUCTURALSPEC_HOME}/lib -aggressive-conditions -v95 -steps-warn-interval 100000000 $*.bsv +RTS -K1000m 2>&1 | ignoreBsc.pl

%: %.
	ln -sf -t bsv ${STRUCTURALSPEC_HOME}/lib/single/*.v
	cd bsv && bsc -u -verilog -e mk$* *.v

clean:
	rm -rf bsv a.out

.PHONY: clean

.DEFAULT_GOAL := Processor

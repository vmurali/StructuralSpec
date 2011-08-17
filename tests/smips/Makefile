%.:
	mkdir -p bsv
	mkdir -p bsv/bdir
	mkdir -p bsv/single
	StructuralSpec -o bsv -i ${STRUCTURALSPEC_HOME}/lib:${STRUCTURALSPEC_HOME}/lib/multi $*.spec
	cd bsv && bsc -u -unsafe-always-ready -verilog -vdir single -bdir bdir -p +:${STRUCTURALSPEC_HOME}/lib/single:${STRUCTURALSPEC_HOME}/lib -aggressive-conditions -v95 -steps-warn-interval 100000000 $*.bsv +RTS -K1000m 2>&1 | ignoreBsc.pl

%: %.
	mkdir -p bsv/multi
	cd bsv/single && Convert.sh
	ln -sf -t bsv/multi ${STRUCTURALSPEC_HOME}/lib/multi/*.v
	cd bsv/multi && bsc -e mk$* *.v
	ln -sf -t bsv/single ${STRUCTURALSPEC_HOME}/lib/single/*.v
	cd bsv/single && bsc -e mk$* *.v

clean:
	rm -rf bsv memory.vmh

.PHONY: clean

.DEFAULT_GOAL := Processor

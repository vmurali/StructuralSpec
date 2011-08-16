%:
	mkdir -p bsv
	mkdir -p bsv/bdir
	mkdir -p bsv/multi
	StructuralSpec -o bsv -i ${STRUCTURALSPEC_HOME}/lib:${STRUCTURALSPEC_HOME}/lib/multi $*.spec
	cd bsv && bsc -u -unsafe-always-ready -verilog -vdir multi -bdir bdir -p +:${STRUCTURALSPEC_HOME}/lib/multi:${STRUCTURALSPEC_HOME}/lib -aggressive-conditions -v95 -steps-warn-interval 100000000 $*.bsv +RTS -K1000m 2>&1 | ignoreBsc.pl

clean:
	rm -rf bsv

.PHONY: clean

.DEFAULT_GOAL :=

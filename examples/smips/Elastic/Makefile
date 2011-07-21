%.:
	mkdir -p bsv
	mkdir -p bsv/bdir
	ln -f -t bsv Primitive.bsv
	ln -f -t bsv ${STRUCTURALSPEC_HOME}/lib/*.v
	StructuralSpec -o bsv -i ${STRUCTURALSPEC_HOME}/lib:.. $*.spec
	cd bsv && bsc -u -verilog -bdir bdir -p +:${STRUCTURALSPEC_HOME}/lib -aggressive-conditions -Werror -v95 -steps-warn-interval 100000000 $*.bsv +RTS -K1000m

%: %.
	cd bsv && bsc -u -verilog -bdir bdir -p +:${STRUCTURALSPEC_HOME}/lib -aggressive-conditions -Werror -v95 -e mk$* *.v

clean:
	rm -rf bsv a.out

.PHONY: clean

.DEFAULT_GOAL := Processor

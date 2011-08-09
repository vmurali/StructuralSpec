include Library;

include RegFile;

port MultiFifoFill#(numeric type fillsNum, type t);
  Input#(NumElems#(fillsNum)) numFreeSlots;
  ConditionalOutput#(t)[fillsNum] data;
endport

port MultiFifoRemove#(numeric type removesNum, type t);
  Input#(NumElems#(removesNum)) numFilledSlots;
  ConditionalInput#(t)[removesNum] data;
  Output#(NumElems#(removesNum)) numDeqs;
endport

port MultiFifo#(numeric type n, numeric type fillsNum, numeric type removesNum, type t);
  Reverse MultiFifoFill#(fillsNum, t) fill;
  Reverse MultiFifoRemove#(removesNum, t) remove;
endport

partition MultiFifo#(n, fillsNum, removesNum, t) mkMultiFifo provisos(Bits#(t, tSz));
  RegFile#(removesNum, fillsNum, n, t) rf <- mkRegFileU;
  Reg#(Index#(n))                    head <- mkReg(0);
  Reg#(Index#(n))                    tail <- mkReg(0);
  Reg#(NumElems#(n))             numElems <- mkReg(0);

  function Index#(n) moduloPlus(NumElems#(n) incr, Index#(n) orig) = truncate(zeroExtend(orig) + incr <= (fromInteger(valueOf(n) - 1))? zeroExtend(orig) + incr: (incr - (fromInteger(valueOf(n) - 1) - (zeroExtend(orig) - 1))));
  function Index#(n) moduloPlus1(Index#(n) orig) = moduloPlus(1, orig);

  atomic a;
    fill.numFreeSlots := truncate(fromInteger(valueOf(n)) - numElems);

    remove.numFilledSlots := truncate(numElems);

    NumElems#(n) numEnqs = 0;
    for(Integer i = 0; i < valueOf(fillsNum); i = i + 1)
      if(fill.data[i].en)
      begin
        rf.write[i] := tuple2(moduloPlus(numEnqs, head), fill.data[i]);
        numEnqs = numEnqs + 1;
      end
    head <= moduloPlus(numEnqs, head);

    for(Integer i = 0; i < valueOf(removesNum); i = i + 1)
      if(fromInteger(i) < numElems)
      begin
        rf.read[i].req := moduloPlus(fromInteger(i), tail);
        remove.data[i] := rf.read[i].resp;
      end
    tail <= moduloPlus(zeroExtend(remove.numDeqs), tail);

    numElems <= numElems + numEnqs - zeroExtend(remove.numDeqs);
  endatomic
endpartition

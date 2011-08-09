include LibraryNormal;

include RegFileNormal;

port MultiFifoFillNormal#(numeric type fillsNum, type t);
  InputNormal#(NumElems#(fillsNum)) numFreeSlots;
  ConditionalOutputNormal#(t)[fillsNum] data;
endport

port MultiFifoRemoveNormal#(numeric type removesNum, type t);
  InputNormal#(NumElems#(removesNum)) numFilledSlots;
  ConditionalInputNormal#(t)[removesNum] data;
  OutputNormal#(NumElems#(removesNum)) numDeqs;
endport

port MultiFifoNormal#(numeric type n, numeric type fillsNum, numeric type removesNum, type t);
  Reverse MultiFifoFillNormal#(fillsNum, t) fill;
  Reverse MultiFifoRemoveNormal#(removesNum, t) remove;
endport

partition MultiFifoNormal#(n, fillsNum, removesNum, t) mkMultiFifoNormal provisos(Bits#(t, tSz));
  RegFileNormal#(removesNum, fillsNum, n, t) rf <- mkRegFileUNormal;
  RegNormal#(Index#(n))                    head <- mkRegNormal(0);
  RegNormal#(Index#(n))                    tail <- mkRegNormal(0);
  RegNormal#(NumElems#(n))             numElems <- mkRegNormal(0);

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

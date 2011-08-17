include Library;

include RegFile;

port MultiFifoEnq#(numeric type n, numeric type enqsNum, type t);
  Input#(NumElems#(n)) numFreeSlots;
  ConditionalOutput#(t)[enqsNum] data;
endport

port MultiFifoDeq#(numeric type n, numeric type deqsNum, type t);
  Input#(NumElems#(n)) numFilledSlots;
  ConditionalInput#(t)[deqsNum] data;
  Output#(NumElems#(n)) numDeqs;
endport

port MultiFifo#(numeric type n, numeric type enqsNum, numeric type deqsNum, type t);
  Reverse MultiFifoEnq#(n, enqsNum, t) enq;
  Reverse MultiFifoDeq#(n, deqsNum, t) deq;
endport

partition MultiFifo#(n, enqsNum, deqsNum, t) mkMultiFifo provisos(Bits#(t, tSz));
  RegFile#(deqsNum, enqsNum, n, t) rf <- mkRegFileU;
  Reg#(Index#(n))                head <- mkReg(0);
  Reg#(Index#(n))                tail <- mkReg(0);
  Reg#(NumElems#(n))         numElems <- mkReg(0);

  atomic a;
    NumElems#(n) numEnqs = 0;
    for(Integer i = 0; i < valueOf(enqsNum); i = i + 1)
      if(enq.data[i].en)
      begin
        rf.write[i] := Pair{fst: Index#(n)'(moduloPlus(valueOf(n), numEnqs, head)), snd: enq.data[i]};
        numEnqs = numEnqs + 1;
      end
    head <= moduloPlus(valueOf(n), numEnqs, head);

    enq.numFreeSlots := fromInteger(valueOf(n)) - numElems;

    NumElems#(n) numFilledSlots = numElems;
    deq.numFilledSlots := numFilledSlots;

    for(Integer i = 0; i < valueOf(deqsNum); i = i + 1)
      if(fromInteger(i) < numFilledSlots)
      begin
        rf.read[i].req := Index#(n)'(moduloPlus(valueOf(n), fromInteger(i), tail));
        deq.data[i] := rf.read[i].resp;
      end
    tail <= moduloPlus(valueOf(n), deq.numDeqs, tail);

    numElems <= numElems + (numEnqs - deq.numDeqs);
  endatomic
endpartition

partition MultiFifo#(n, enqsNum, deqsNum, t) mkMultiLFifo provisos(Bits#(t, tSz));
  RegFile#(deqsNum, enqsNum, n, t) rf <- mkRegFileU;
  Reg#(Index#(n))                head <- mkReg(0);
  Reg#(Index#(n))                tail <- mkReg(0);
  Reg#(NumElems#(n))         numElems <- mkReg(0);

  atomic a;
    NumElems#(n) numEnqs = 0;
    for(Integer i = 0; i < valueOf(enqsNum); i = i + 1)
      if(enq.data[i].en)
      begin
        rf.write[i] := Pair{fst: Index#(n)'(moduloPlus(valueOf(n), numEnqs, head)), snd: enq.data[i]};
        numEnqs = numEnqs + 1;
      end
    head <= moduloPlus(valueOf(n), numEnqs, head);

    enq.numFreeSlots := fromInteger(valueOf(n)) - numElems + deq.numDeqs;

    NumElems#(n) numFilledSlots = numElems;
    deq.numFilledSlots := numFilledSlots;

    for(Integer i = 0; i < valueOf(deqsNum); i = i + 1)
      if(fromInteger(i) < numFilledSlots)
      begin
        rf.read[i].req := Index#(n)'(moduloPlus(valueOf(n), fromInteger(i), tail));
        deq.data[i] := rf.read[i].resp;
      end
    tail <= moduloPlus(valueOf(n), deq.numDeqs, tail);

    numElems <= numElems + (numEnqs - deq.numDeqs);
  endatomic
endpartition

partition MultiFifo#(n, enqsNum, deqsNum, t) mkMultiBypassFifo provisos(Bits#(t, tSz));
  RegFile#(deqsNum, enqsNum, n, t) rf <- mkRegFileU;
  Reg#(Index#(n))                head <- mkReg(0);
  Reg#(Index#(n))                tail <- mkReg(0);
  Reg#(NumElems#(n))         numElems <- mkReg(0);

  atomic a;
    NumElems#(n) numEnqs = 0;
    for(Integer i = 0; i < valueOf(enqsNum); i = i + 1)
      if(enq.data[i].en)
      begin
        rf.write[i] := Pair{fst: Index#(n)'(moduloPlus(valueOf(n), numEnqs, head)), snd: enq.data[i]};
        numEnqs = numEnqs + 1;
      end
    head <= moduloPlus(valueOf(n), numEnqs, head);

    enq.numFreeSlots := fromInteger(valueOf(n)) - numElems;

    NumElems#(n) numFilledSlots = numElems + numEnqs;
    deq.numFilledSlots := numFilledSlots;

    for(Integer i = 0; i < valueOf(deqsNum); i = i + 1)
      if(fromInteger(i) < numFilledSlots)
      begin
        if(fromInteger(i) < numElems)
        begin
          rf.read[i].req := Index#(n)'(moduloPlus(valueOf(n), fromInteger(i), tail));
          deq.data[i] := rf.read[i].resp;
        end
        else
          deq.data[i] := enq.data[fromInteger(i) - numElems];
      end
    tail <= moduloPlus(valueOf(n), deq.numDeqs, tail);

    numElems <= numElems + (numEnqs - deq.numDeqs);
  endatomic
endpartition

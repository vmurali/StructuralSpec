include Library;

include RegFileNormal;

port MultiFifoEnqNormal#(numeric type n, numeric type enqsNum, type t);
  InputNormal#(NumElems#(n)) numFreeSlots;
  ConditionalOutputNormal#(t)[enqsNum] data;
endport

port MultiFifoDeqNormal#(numeric type n, numeric type deqsNum, type t);
  InputNormal#(NumElems#(n)) numFilledSlots;
  ConditionalInputNormal#(t)[deqsNum] data;
  OutputNormal#(NumElems#(n)) numDeqs;
endport

port MultiFifoNormal#(numeric type n, numeric type enqsNum, numeric type deqsNum, type t);
  Reverse MultiFifoEnqNormal#(n, enqsNum, t) enq;
  Reverse MultiFifoDeqNormal#(n, deqsNum, t) deq;
endport

partition MultiFifoNormal#(n, enqsNum, deqsNum, t) mkMultiFifoNormal provisos(Bits#(t, tSz));
  RegFileNormal#(deqsNum, enqsNum, n, t) rf <- mkRegFileUNormal;
  RegNormal#(Index#(n))                head <- mkRegNormal(0);
  RegNormal#(Index#(n))                tail <- mkRegNormal(0);
  RegNormal#(NumElems#(n))         numElems <- mkRegNormal(0);

  atomic a;
    NumElems#(n) numEnqs = 0;
    for(Integer i = 0; i < valueOf(enqsNum); i = i + 1)
      if(enq.data[i].en)
      begin
        rf.write[i] := RegFileWriteNormal{index: moduloPlus(valueOf(n), numEnqs, head), data: enq.data[i]};
        numEnqs = numEnqs + 1;
      end
    head <= moduloPlus(valueOf(n), numEnqs, head);

    enq.numFreeSlots := fromInteger(valueOf(n)) - numElems;

    NumElems#(n) numFilledSlots = numElems;
    deq.numFilledSlots := numFilledSlots;

    for(Integer i = 0; i < valueOf(deqsNum); i = i + 1)
      if(fromInteger(i) < numFilledSlots)
      begin
        rf.read[i].req := moduloPlus(valueOf(n), fromInteger(i), tail);
        deq.data[i] := rf.read[i].resp;
      end
    tail <= moduloPlus(valueOf(n), deq.numDeqs, tail);

    numElems <= numElems + (numEnqs - deq.numDeqs);
  endatomic
endpartition

partition MultiFifoNormal#(n, enqsNum, deqsNum, t) mkMultiLFifoNormal provisos(Bits#(t, tSz));
  RegFileNormal#(deqsNum, enqsNum, n, t) rf <- mkRegFileUNormal;
  RegNormal#(Index#(n))                head <- mkRegNormal(0);
  RegNormal#(Index#(n))                tail <- mkRegNormal(0);
  RegNormal#(NumElems#(n))         numElems <- mkRegNormal(0);

  atomic a;
    NumElems#(n) numEnqs = 0;
    for(Integer i = 0; i < valueOf(enqsNum); i = i + 1)
      if(enq.data[i].en)
      begin
        rf.write[i] := RegFileWriteNormal{index: moduloPlus(valueOf(n), numEnqs, head), data: enq.data[i]};
        numEnqs = numEnqs + 1;
      end
    head <= moduloPlus(valueOf(n), numEnqs, head);

    enq.numFreeSlots := fromInteger(valueOf(n)) - numElems + deq.numDeqs;

    NumElems#(n) numFilledSlots = numElems;
    deq.numFilledSlots := numFilledSlots;

    for(Integer i = 0; i < valueOf(deqsNum); i = i + 1)
      if(fromInteger(i) < numFilledSlots)
      begin
        rf.read[i].req := moduloPlus(valueOf(n), fromInteger(i), tail);
        deq.data[i] := rf.read[i].resp;
      end
    tail <= moduloPlus(valueOf(n), deq.numDeqs, tail);

    numElems <= numElems + (numEnqs - deq.numDeqs);
  endatomic
endpartition

partition MultiFifoNormal#(n, enqsNum, deqsNum, t) mkMultiBypassFifoNormal provisos(Bits#(t, tSz));
  RegFileNormal#(deqsNum, enqsNum, n, t) rf <- mkRegFileUNormal;
  RegNormal#(Index#(n))                head <- mkRegNormal(0);
  RegNormal#(Index#(n))                tail <- mkRegNormal(0);
  RegNormal#(NumElems#(n))         numElems <- mkRegNormal(0);

  atomic a;
    NumElems#(n) numEnqs = 0;
    for(Integer i = 0; i < valueOf(enqsNum); i = i + 1)
      if(enq.data[i].en)
      begin
        rf.write[i] := RegFileWriteNormal{index: moduloPlus(valueOf(n), numEnqs, head), data: enq.data[i]};
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
          rf.read[i].req := moduloPlus(valueOf(n), fromInteger(i), tail);
          deq.data[i] := rf.read[i].resp;
        end
        else
          deq.data[i] := enq.data[fromInteger(i) - numElems];
      end
    tail <= moduloPlus(valueOf(n), deq.numDeqs, tail);

    numElems <= numElems + (numEnqs - deq.numDeqs);
  endatomic
endpartition

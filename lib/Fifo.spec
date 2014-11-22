include MultiFifo;

port FifoEnq#(type t);
  Input#(Bool) notFull;
  ConditionalOutput#(t) enq WriteGuard(notFull);
endport

port FifoDeq#(type t);
  Input#(Bool) notEmpty;
  Input#(t) first ReadGuard(notEmpty);
  OutputPulse deq WriteGuard(notEmpty);
endport

port Fifo#(numeric type n, type t);
  Reverse FifoEnq#(t) enq;
  Reverse FifoDeq#(t) deq;
  Output#(NumElems#(n)) numFilled;
endport

partition Fifo#(n, t) mkGenericFifo#(function _m__#(MultiFifo#(n, 1, 1, t)) mkF) provisos(Bits#(t, tSz));
  MultiFifo#(n, 1, 1, t) f <- mkF;

  atomic a;
    enq.notFull := f.enq.numFreeSlots > 0;
    deq.notEmpty := f.deq.numFilledSlots > 0;
    f.deq.numDeqs := deq.deq? 1: 0;
    deq.first := f.deq.data[0];
    numFilled := f.deq.numFilledSlots;
  endatomic

  mkConnection(f.enq.data[0], enq.enq);
endpartition

partinst Fifo#(n, t) mkLFifo provisos(Bits#(t, tSz)) = mkGenericFifo(mkMultiLFifo);

partinst Fifo#(n, t) mkFifo provisos(Bits#(t, tSz)) = mkGenericFifo(mkMultiFifo);

partinst Fifo#(n, t) mkBypassFifo provisos(Bits#(t, tSz)) = mkGenericFifo(mkMultiBypassFifo);

port UgFifoEnq#(type t);
  Input#(Bool) notFull;
  ConditionalOutput#(t) enq;
endport

port UgFifoDeq#(type t);
  Input#(Bool) notEmpty;
  Input#(t) first;
  OutputPulse deq;
endport

port UgFifo#(numeric type n, type t);
  Reverse UgFifoEnq#(t) enq;
  Reverse UgFifoDeq#(t) deq;
  Output#(NumElems#(n)) numFilled;
endport

partition UgFifo#(n, t) mkGenericUgFifo#(function _m__#(MultiFifo#(n, 1, 1, t)) mkF) provisos(Bits#(t, tSz));
  MultiFifo#(n, 1, 1, t) f <- mkF;

  atomic a;
    enq.notFull := f.enq.numFreeSlots > 0;
    deq.notEmpty := f.deq.numFilledSlots > 0;
    f.deq.numDeqs := deq.deq? 1: 0;
    deq.first := f.deq.data[0];
    numFilled := f.deq.numFilledSlots;
  endatomic

  mkConnection(f.enq.data[0], enq.enq);
endpartition

partinst UgFifo#(n, t) mkLUgFifo provisos(Bits#(t, tSz)) = mkGenericUgFifo(mkMultiLFifo);

partinst UgFifo#(n, t) mkUgFifo provisos(Bits#(t, tSz)) = mkGenericUgFifo(mkMultiFifo);

partinst UgFifo#(n, t) mkBypassUgFifo provisos(Bits#(t, tSz)) = mkGenericUgFifo(mkMultiBypassFifo);

include Bram;

partition Fifo#(n, t) mkBramFifo provisos(Bits#(t, tSz), Add#(1, sth, n));
  Bram#(1, 1, n, t)        rf <- mkBramU;
  Reg#(Index#(n))        head <- mkReg(0);
  Reg#(Index#(n))        tail <- mkReg(moduloPlus(valueOf(n), 1, 0));
  Reg#(NumElems#(n)) numElems <- mkReg(0);
  Reg#(Bool)          reqSent <- mkReg(False);
  Reg#(t)             tailVal <- mkRegU;

  atomic a;
    NumElems#(n) numEnqs = zeroExtend(pack(enq.enq.en));
    NumElems#(n) numDeqs = zeroExtend(pack(deq.deq));

    enq.notFull := fromInteger(valueOf(n)) > numElems;
    deq.notEmpty := numElems > 0;

    rf.write[0] := Pair{fst: Index#(n)'(head), snd: enq.enq};

    head <= moduloPlus(valueOf(n), numEnqs, head);
    tail <= moduloPlus(valueOf(n), numDeqs, tail);
    
    numElems <= numElems + (numEnqs - numDeqs);
    reqSent <= deq.deq;

    if(deq.deq)
      rf.read[0].req := Index#(n)'(tail);

    t deqVal =
      reqSent ?
        rf.read[0].resp :
        tailVal;

    t newTailVal =
      enq.enq.en && numElems == 0 ?
        enq.enq :
        deqVal;

    deq.first := deqVal;
    tailVal <= newTailVal;

    numFilled := numElems;
  endatomic
endpartition

partition Fifo#(n, t) mkBramLFifo provisos(Bits#(t, tSz), Add#(1, sth, n));
  Bram#(1, 1, n, t)        rf <- mkBramU;
  Reg#(Index#(n))        head <- mkReg(0);
  Reg#(Index#(n))        tail <- mkReg(moduloPlus(valueOf(n), 1, 0));
  Reg#(NumElems#(n)) numElems <- mkReg(0);
  Reg#(Bool)          reqSent <- mkReg(False);
  Reg#(t)             tailVal <- mkRegU;

  atomic a;
    NumElems#(n) numEnqs = zeroExtend(pack(enq.enq.en));
    NumElems#(n) numDeqs = zeroExtend(pack(deq.deq));

    enq.notFull := fromInteger(valueOf(n)) + numDeqs > numElems;
    deq.notEmpty := numElems > 0;

    //if(enq.enq.en && numElems != 0)
    rf.write[0] := Pair{fst: Index#(n)'(head), snd: enq.enq};

    //NumElems#(n) numEnqsL = zeroExtend(pack(enq.enq.en && numElems != 0));
    head <= moduloPlus(valueOf(n), numEnqs, head);
    tail <= moduloPlus(valueOf(n), numDeqs, tail);
    
    numElems <= numElems + (numEnqs - numDeqs);
    reqSent <= deq.deq;

    if(deq.deq)
      rf.read[0].req := Index#(n)'(tail);

    t deqVal =
      reqSent ?
        rf.read[0].resp :
        tailVal;

    t newTailVal =
      enq.enq.en && numElems == 0 ?
        enq.enq :
        deqVal;

    deq.first := deqVal;
    tailVal <= newTailVal;

    numFilled := numElems;
  endatomic
endpartition

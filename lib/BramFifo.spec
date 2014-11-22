include Bram;
include Fifo;

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
//    Bit#(TAdd#(1, tSz)) val = zeroExtend(pack(tailVal));
//    $display("%d %m vals: %d %d %d %d %x %d %d %d %d %d", $time, head, tail, numElems, reqSent, val >> 1, val[0], enq.enq.en, deq.deq, deqVal, newTailVal);
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

//    Bit#(TAdd#(1, tSz)) val = zeroExtend(pack(tailVal));
//    $display("%d vals: %d %d %d %d %x %d %d %d", $time, head, tail, numElems, reqSent, val >> 1, val[0], enq.enq.en, deq.deq);
  endatomic
endpartition

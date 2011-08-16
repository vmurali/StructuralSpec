include Library;

include RegFileNormal;

port FifoEnqNormal#(type t);
  InputNormal#(Bool) notFull;
  ConditionalOutputNormal#(t) enq WriteGuard(notFull);
endport

port FifoDeqNormal#(type t);
  InputNormal#(Bool) notEmpty;
  InputNormal#(t) first ReadGuard(notEmpty);
  OutputPulseNormal deq WriteGuard(notEmpty);
endport

port FifoNormal#(numeric type n, type t);
  Reverse FifoEnqNormal#(t) enq;
  Reverse FifoDeqNormal#(t) deq;
endport

partition FifoNormal#(n, t) mkLFifoNormal provisos(Bits#(t, tSz));
  RegFileNormal#(1, 1, n, t) regs <- mkRegFileUNormal;

  RegNormal#(Index#(n)) head <- mkRegNormal(0);
  RegNormal#(Index#(n)) tail <- mkRegNormal(0);
  RegNormal#(NumElems#(n)) numElems <- mkRegNormal(0);

  atomic a;
    enq.notFull := numElems != fromInteger(valueOf(n)) || deq.deq;
    deq.notEmpty := numElems != 0;

    regs.read[0].req := tail;
    deq.first := regs.read[0].resp;

    if(deq.deq)
      tail <= moduloIncr(tail);

    if(enq.enq.en)
    begin
      regs.write[0] := RegFileWriteNormal{index: head, data: enq.enq};
      head <= moduloIncr(head);
    end

    Bit#(2) diff = zeroExtend(pack(enq.enq.en)) - zeroExtend(pack(deq.deq));
    numElems <= numElems + signExtend(diff);
  endatomic
endpartition

partition FifoNormal#(n, t) mkFifoNormal provisos(Bits#(t, tSz));
  RegFileNormal#(1, 1, n, t) regs <- mkRegFileUNormal;

  RegNormal#(Index#(n)) head <- mkRegNormal(0);
  RegNormal#(Index#(n)) tail <- mkRegNormal(0);
  RegNormal#(NumElems#(n)) numElems <- mkRegNormal(0);

  atomic a;
    enq.notFull := numElems != fromInteger(valueOf(n));
    deq.notEmpty := numElems != 0;

    regs.read[0].req := tail;
    deq.first := regs.read[0].resp;

    if(deq.deq)
      tail <= moduloIncr(tail);

    if(enq.enq.en)
    begin
      regs.write[0] := RegFileWriteNormal{index: head, data: enq.enq};
      head <= moduloIncr(head);
    end

    Bit#(2) diff = zeroExtend(pack(enq.enq.en)) - zeroExtend(pack(deq.deq));
    numElems <= numElems + signExtend(diff);
  endatomic
endpartition

partition FifoNormal#(n, t) mkBypassFifoNormal provisos(Bits#(t, tSz));
  RegFileNormal#(1, 1, n, t) regs <- mkRegFileUNormal;

  RegNormal#(Index#(n)) head <- mkRegNormal(0);
  RegNormal#(Index#(n)) tail <- mkRegNormal(0);
  RegNormal#(NumElems#(n)) numElems <- mkRegNormal(0);

  atomic a;
    enq.notFull := numElems != fromInteger(valueOf(n));
    deq.notEmpty := numElems != 0 || enq.enq.en;

    regs.read[0].req := tail;
    deq.first := (numElems != 0)? regs.read[0].resp: enq.enq;

    if(deq.deq)
      tail <= moduloIncr(tail);

    if(enq.enq.en)
    begin
      regs.write[0] := RegFileWriteNormal{index: head, data: enq.enq};
      head <= moduloIncr(head);
    end

    Bit#(2) diff = zeroExtend(pack(enq.enq.en)) - zeroExtend(pack(deq.deq));
    numElems <= numElems + signExtend(diff);
  endatomic
endpartition

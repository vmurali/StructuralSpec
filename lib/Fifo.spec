include Library;

include RegFile;

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
endport

partition Fifo#(n, t) mkLFifo provisos(Bits#(t, tSz));
  RegFile#(1, 1, n, t) regs <- mkRegFileU;

  Reg#(Index#(n)) head <- mkReg(0);
  Reg#(Index#(n)) tail <- mkReg(0);
  Reg#(NumElems#(n)) numElems <- mkReg(0);

  atomic a;
    enq.notFull := numElems != fromInteger(valueOf(n)) || deq.deq;
    deq.notEmpty := numElems != 0;

    regs.read[0].req := tail;
    deq.first := regs.read[0].resp;

    if(deq.deq)
      tail <= moduloIncr(tail);

    if(enq.enq.en)
    begin
      regs.write[0] := RegFileWrite{index: head, data: enq.enq};
      head <= moduloIncr(head);
    end

    Bit#(2) diff = zeroExtend(pack(enq.enq.en)) - zeroExtend(pack(deq.deq));
    numElems <= numElems + signExtend(diff);
  endatomic
endpartition

partition Fifo#(n, t) mkFifo provisos(Bits#(t, tSz));
  RegFile#(1, 1, n, t) regs <- mkRegFileU;

  Reg#(Index#(n)) head <- mkReg(0);
  Reg#(Index#(n)) tail <- mkReg(0);
  Reg#(NumElems#(n)) numElems <- mkReg(0);

  atomic a;
    enq.notFull := numElems != fromInteger(valueOf(n));
    deq.notEmpty := numElems != 0;

    regs.read[0].req := tail;
    deq.first := regs.read[0].resp;

    if(deq.deq)
      tail <= moduloIncr(tail);

    if(enq.enq.en)
    begin
      regs.write[0] := RegFileWrite{index: head, data: enq.enq};
      head <= moduloIncr(head);
    end

    Bit#(2) diff = zeroExtend(pack(enq.enq.en)) - zeroExtend(pack(deq.deq));
    numElems <= numElems + signExtend(diff);
  endatomic
endpartition

partition Fifo#(n, t) mkBypassFifo provisos(Bits#(t, tSz));
  RegFile#(1, 1, n, t) regs <- mkRegFileU;

  Reg#(Index#(n)) head <- mkReg(0);
  Reg#(Index#(n)) tail <- mkReg(0);
  Reg#(NumElems#(n)) numElems <- mkReg(0);

  atomic a;
    enq.notFull := numElems != fromInteger(valueOf(n));
    deq.notEmpty := numElems != 0 || enq.enq.en;

    regs.read[0].req := tail;
    deq.first := (numElems != 0)? regs.read[0].resp: enq.enq;

    if(deq.deq)
      tail <= moduloIncr(tail);

    if(enq.enq.en)
    begin
      regs.write[0] := RegFileWrite{index: head, data: enq.enq};
      head <= moduloIncr(head);
    end

    Bit#(2) diff = zeroExtend(pack(enq.enq.en)) - zeroExtend(pack(deq.deq));
    numElems <= numElems + signExtend(diff);
  endatomic
endpartition

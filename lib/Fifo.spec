include Library;

port FifoEnq#(type t);
  Input#(Bool) rdy;
  OutputPulse en Guard(rdy);
  Output#(t) data En(en) Guard(rdy);
endport

port FifoDeq#(type t);
  Input#(Bool) rdy;
  Default Input#(t) first Guard(rdy);
  OutputPulse deq Guard(rdy);
endport

port Fifo#(numeric type n, type t);
  Reverse GuardedAction#(t) enq;
  Reverse FifoDeq#(t) deq;
endport

partition mkLFifo implements Fifo#(n, t) provisos(Bits#(t, tSz));
  Reg#(Vector#(n, t)) regs <- mkReg(newVector);
  Reg#(Bit#(TAdd#(TLog#(n), 1))) head <- mkReg(0);
  Reg#(Bit#(TAdd#(TLog#(n), 1))) tail <- mkReg(0);

  let actualHead = head >= fromInteger(valueOf(n))? head - fromInteger(valueOf(n)): head;
  let actualTail = tail >= fromInteger(valueOf(n))? tail - fromInteger(valueOf(n)): tail;

  rule r1;
    enq.rdy := head != tail + fromInteger(valueOf(n)) || deq.deq;
  endrule

  rule r2;
    deq.rdy := head != tail;
  endrule

  rule r3;
    deq.first := regs[actualTail];
  endrule

  rule r4;
    if(deq.deq)
      tail <= tail + 1;

    if(enq.en)
    begin
      let tempRegs = regs;
      tempRegs[actualHead] = enq.data;
      regs <= tempRegs;

      head <= head + 1;
    end
  endrule

  rule r5;
    specCycleDone;
  endrule
endpartition

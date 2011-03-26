include Library;

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

  rule r1;
    enq.rdy := head != tail + fromInteger(valueOf(n)) || deq.deq;
  endrule

  rule r2;
    deq.rdy := head != tail;
  endrule

  rule r3;
    deq.first := regs[tail];
  endrule

  rule r4;
    if(deq.deq)
      tail <= tail + 1;

    if(enq.en)
    begin
      let tempRegs = regs;
      tempRegs[head] = (tpl_1(asIfc(mod_))).enq;
      regs <= tempRegs;

      head <= head + 1;
    end
  endrule
endpartition

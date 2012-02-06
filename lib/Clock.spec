import Clocks::*;
import FIFOLevel::*;

include Fifo;

interface ClockDiv;
  interface Clock clk;
  interface Reset rst;
endinterface

module mkClockDiv#(Integer div) (ClockDiv);
  ClockDividerIfc clkDiv <- mkClockDivider(div);
  Reset destRst <- mkAsyncResetFromCR(div-1, clkDiv.slowClock);
  return (interface ClockDiv;
            interface clk = clkDiv.slowClock;
            interface rst = destRst;
          endinterface);
endmodule

interface SyncFifo#(numeric type n);
endinterface

module mkSyncFifo#(Clock sClk, Reset sRst, Clock dClk, Reset dRst, FifoEnq#(t) prod, FifoDeq#(t) cons) (SyncFifo#(n)) provisos(Bits#(t, tSz));
  SyncFIFOCountIfc#(t, n) sf <- mkSyncFIFOCount(sClk, sRst, dClk);

  rule dcNotFull;
    prod.notFull.write(sf.sNotFull);
  endrule

  rule dcEnq;
    if(prod.enq.en)
      sf.enq(prod.enq);
  endrule

  rule dcNotEmpty;
    cons.notEmpty.write(sf.dNotEmpty);
  endrule

  rule dcDeq;
    if(cons.deq)
      sf.deq;
  endrule

  rule dcFirst;
    cons.first.write(sf.first);
  endrule
endmodule

module mkSyncFifoFromCurr#(Clock dClk, Reset dRst, FifoEnq#(t) prod, FifoDeq#(t) cons) (SyncFifo#(n)) provisos(Bits#(t, tSz));
  Clock currClk <- exposeCurrentClock;
  Reset currRst <- exposeCurrentReset;
  SyncFifo#(n) f <- mkSyncFifo(currClk, currRst, dClk, dRst, prod, cons);
  return f;
endmodule

module mkSyncFifoToCurr#(Clock sClk, Reset sRst, FifoEnq#(t) prod, FifoDeq#(t) cons) (SyncFifo#(n)) provisos(Bits#(t, tSz));
  Clock currClk <- exposeCurrentClock;
  Reset currRst <- exposeCurrentReset;
  SyncFifo#(n) f <- mkSyncFifo(sClk, sRst, currClk, currRst, prod, cons);
  return f;
endmodule


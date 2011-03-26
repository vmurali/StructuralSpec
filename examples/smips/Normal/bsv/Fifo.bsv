import Vector::*;
import HaskellLib::*;
import Connectable::*;
import Base::*;
import Primitive::*;

import Library::*;
interface FifoDeq_#(type t);
  interface Output#(Bool) rdy;
  interface Output#(t) first;
  method (t) _read();
  interface OutputPulse_ deq;
  method Action specCycleInputDone();
  method Action specCycleOutputDone();
  method Bool isSupplied();
endinterface

interface FifoDeq#(type t);
  interface Output_#(Bool) rdy;
  interface Output_#(t) first;
  method Action _write((t) x);
  interface OutputPulse deq;
  method Action specCycleInputDone();
  method Action specCycleOutputDone();
  method Bool isSupplied();
endinterface

module _FifoDeq(Tuple2#(FifoDeq_#(t), FifoDeq#(t))) provisos(Bits#(t, _sZt));
  Tuple2#(Output_#(Bool), Output#(Bool)) rdy__ <- _Output(False, ?, True, True);
  Tuple2#(Output#(Bool), Output_#(Bool)) rdy_ = tuple2(tpl_2(asIfc(rdy__)), tpl_1(asIfc(rdy__)));
  Tuple2#(Output_#(t), Output#(t)) first__ <- _Output(False, ?, True, (tpl_1(asIfc(rdy_)))._read);
  Tuple2#(Output#(t), Output_#(t)) first_ = tuple2(tpl_2(asIfc(first__)), tpl_1(asIfc(first__)));
  Tuple2#(OutputPulse_, OutputPulse) deq_ <- _OutputPulse(False, ?, (tpl_1(asIfc(rdy_)))._read, True);
  return tuple2(
    interface FifoDeq_;
      interface rdy = tpl_1(asIfc(rdy_));
      interface first = tpl_1(asIfc(first_));
      method _read = (tpl_1(asIfc(first_)))._read;
      interface deq = tpl_1(asIfc(deq_));
      method Action specCycleInputDone();
        _specCycleInputDone(tpl_1(asIfc(rdy_)));
        _specCycleInputDone(tpl_1(asIfc(first_)));
        _specCycleInputDone(tpl_1(asIfc(deq_)));
      endmethod
      method Action specCycleOutputDone();
        _specCycleOutputDone(tpl_1(asIfc(rdy_)));
        _specCycleOutputDone(tpl_1(asIfc(first_)));
        _specCycleOutputDone(tpl_1(asIfc(deq_)));
      endmethod
      method Bool isSupplied = True  && _isSupplied(tpl_1(asIfc(rdy_))) && _isSupplied(tpl_1(asIfc(first_))) && _isSupplied(tpl_1(asIfc(deq_)));
    endinterface,
    interface FifoDeq;
      interface rdy = tpl_2(asIfc(rdy_));
      interface first = tpl_2(asIfc(first_));
      method _write = (tpl_2(asIfc(first_)))._write;
      interface deq = tpl_2(asIfc(deq_));
      method Action specCycleInputDone();
        _specCycleInputDone(tpl_2(asIfc(rdy_)));
        _specCycleInputDone(tpl_2(asIfc(first_)));
        _specCycleInputDone(tpl_2(asIfc(deq_)));
      endmethod
      method Action specCycleOutputDone();
        _specCycleOutputDone(tpl_2(asIfc(rdy_)));
        _specCycleOutputDone(tpl_2(asIfc(first_)));
        _specCycleOutputDone(tpl_2(asIfc(deq_)));
      endmethod
      method Bool isSupplied = True  && _isSupplied(tpl_2(asIfc(rdy_))) && _isSupplied(tpl_2(asIfc(first_))) && _isSupplied(tpl_2(asIfc(deq_)));
    endinterface);
endmodule

instance Connectable#(FifoDeq#(t), FifoDeq_#(t)) provisos(Bits#(t, _sZt));
  module mkConnection#(FifoDeq#(t) a, FifoDeq_#(t) b)();
    mkConnection(asIfc(a.rdy), asIfc(b.rdy));
    mkConnection(asIfc(a.first), asIfc(b.first));
    mkConnection(asIfc(a.deq), asIfc(b.deq));
  endmodule
endinstance

instance Connectable#(FifoDeq_#(t), FifoDeq#(t)) provisos(Bits#(t, _sZt));
  module mkConnection#(FifoDeq_#(t) a, FifoDeq#(t) b)();
    mkConnection(asIfc(a.rdy), asIfc(b.rdy));
    mkConnection(asIfc(a.first), asIfc(b.first));
    mkConnection(asIfc(a.deq), asIfc(b.deq));
  endmodule
endinstance

instance Sync_#(FifoDeq#(t));
  function Action _specCycleInputDone(FifoDeq#(t) x) = x.specCycleInputDone;
  function Action _specCycleOutputDone(FifoDeq#(t) x) = x.specCycleOutputDone;
  function Bool _isSupplied(FifoDeq#(t) x) = x.isSupplied;
endinstance

instance Sync_#(FifoDeq_#(t));
  function Action _specCycleInputDone(FifoDeq_#(t) x) = x.specCycleInputDone;
  function Action _specCycleOutputDone(FifoDeq_#(t) x) = x.specCycleOutputDone;
  function Bool _isSupplied(FifoDeq_#(t) x) = x.isSupplied;
endinstance

interface Fifo_#(numeric type n, type t);
  interface GuardedAction#(t) enq;
  interface FifoDeq#(t) deq;
  method Action specCycleInputDone();
  method Action specCycleOutputDone();
  method Bool isSupplied();
endinterface

interface Fifo#(numeric type n, type t);
  interface GuardedAction_#(t) enq;
  interface FifoDeq_#(t) deq;
  method Action specCycleInputDone();
  method Action specCycleOutputDone();
  method Bool isSupplied();
endinterface

module _Fifo(Tuple2#(Fifo_#(n, t), Fifo#(n, t))) provisos(Bits#(t, _sZt));
  Tuple2#(GuardedAction_#(t), GuardedAction#(t)) enq__ <- _GuardedAction;
  Tuple2#(GuardedAction#(t), GuardedAction_#(t)) enq_ = tuple2(tpl_2(asIfc(enq__)), tpl_1(asIfc(enq__)));
  Tuple2#(FifoDeq_#(t), FifoDeq#(t)) deq__ <- _FifoDeq;
  Tuple2#(FifoDeq#(t), FifoDeq_#(t)) deq_ = tuple2(tpl_2(asIfc(deq__)), tpl_1(asIfc(deq__)));
  return tuple2(
    interface Fifo_;
      interface enq = tpl_1(asIfc(enq_));
      interface deq = tpl_1(asIfc(deq_));
      method Action specCycleInputDone();
        _specCycleInputDone(tpl_1(asIfc(enq_)));
        _specCycleInputDone(tpl_1(asIfc(deq_)));
      endmethod
      method Action specCycleOutputDone();
        _specCycleOutputDone(tpl_1(asIfc(enq_)));
        _specCycleOutputDone(tpl_1(asIfc(deq_)));
      endmethod
      method Bool isSupplied = True  && _isSupplied(tpl_1(asIfc(enq_))) && _isSupplied(tpl_1(asIfc(deq_)));
    endinterface,
    interface Fifo;
      interface enq = tpl_2(asIfc(enq_));
      interface deq = tpl_2(asIfc(deq_));
      method Action specCycleInputDone();
        _specCycleInputDone(tpl_2(asIfc(enq_)));
        _specCycleInputDone(tpl_2(asIfc(deq_)));
      endmethod
      method Action specCycleOutputDone();
        _specCycleOutputDone(tpl_2(asIfc(enq_)));
        _specCycleOutputDone(tpl_2(asIfc(deq_)));
      endmethod
      method Bool isSupplied = True  && _isSupplied(tpl_2(asIfc(enq_))) && _isSupplied(tpl_2(asIfc(deq_)));
    endinterface);
endmodule

instance Connectable#(Fifo#(n, t), Fifo_#(n, t)) provisos(Bits#(t, _sZt));
  module mkConnection#(Fifo#(n, t) a, Fifo_#(n, t) b)();
    mkConnection(asIfc(a.enq), asIfc(b.enq));
    mkConnection(asIfc(a.deq), asIfc(b.deq));
  endmodule
endinstance

instance Connectable#(Fifo_#(n, t), Fifo#(n, t)) provisos(Bits#(t, _sZt));
  module mkConnection#(Fifo_#(n, t) a, Fifo#(n, t) b)();
    mkConnection(asIfc(a.enq), asIfc(b.enq));
    mkConnection(asIfc(a.deq), asIfc(b.deq));
  endmodule
endinstance

instance Sync_#(Fifo#(n, t));
  function Action _specCycleInputDone(Fifo#(n, t) x) = x.specCycleInputDone;
  function Action _specCycleOutputDone(Fifo#(n, t) x) = x.specCycleOutputDone;
  function Bool _isSupplied(Fifo#(n, t) x) = x.isSupplied;
endinstance

instance Sync_#(Fifo_#(n, t));
  function Action _specCycleInputDone(Fifo_#(n, t) x) = x.specCycleInputDone;
  function Action _specCycleOutputDone(Fifo_#(n, t) x) = x.specCycleOutputDone;
  function Bool _isSupplied(Fifo_#(n, t) x) = x.isSupplied;
endinstance

module mkLFifo(Fifo#(n, t)) provisos(Bits#(t, tSz));
  Tuple2#(Fifo_#(n, t), Fifo#(n, t)) mod_ <- _Fifo;

  Reg#(Vector#(n, t)) regs <- mkReg(newVector);
  Reg#(Bit#(TAdd#(TLog#(n), 1))) head <- mkReg(0);
  Reg#(Bit#(TAdd#(TLog#(n), 1))) tail <- mkReg(0);

  rule r1;
    (tpl_1(asIfc(mod_))).enq.rdy._write( head != tail + fromInteger(valueOf(n)) || (tpl_1(asIfc(mod_))).deq.deq);
  endrule

  rule r2;
    (tpl_1(asIfc(mod_))).deq.rdy._write( head != tail);
  endrule

  rule r3;
    (tpl_1(asIfc(mod_))).deq.first._write( regs[tail]);
  endrule

  rule r4;
    if((tpl_1(asIfc(mod_))).deq.deq)
      tail <= tail + 1;

    if((tpl_1(asIfc(mod_))).enq.en)
    begin
      let tempRegs = regs;
      tempRegs[head] = (tpl_1(asIfc(mod_))).enq;
      regs <= tempRegs;

      head <= head + 1;
    end
  endrule

  rule r5;
    (tpl_1(asIfc(mod_))).specCycleInputDone;
    (tpl_1(asIfc(mod_))).specCycleOutputDone;
    _specCycleInputDone(asIfc(regs));
    _specCycleOutputDone(asIfc(regs));
    _specCycleInputDone(asIfc(head));
    _specCycleOutputDone(asIfc(head));
    _specCycleInputDone(asIfc(tail));
    _specCycleOutputDone(asIfc(tail));
 endrule

  return tpl_2(asIfc(mod_));
endmodule


import Vector::*;
import HaskellLib::*;
import Connectable::*;
import Base::*;
import Primitive::*;
export Fifo::*;

import MultiFifo::*;
export MultiFifo::*;

interface FifoEnq_#(type t);
  interface Output#(Bool) notFull;
  interface ConditionalOutput_#(t) enq;
endinterface

interface FifoEnq#(type t);
  interface Output_#(Bool) notFull;
  interface ConditionalOutput#(t) enq;
endinterface

module _FifoEnq(Tuple2#(FifoEnq_#(t), FifoEnq#(t))) provisos(Bits#(t, _sZt));
  Tuple2#(Output_#(Bool), Output#(Bool)) notFull_ <- _Output(True, True);
  Tuple2#(ConditionalOutput_#(t), ConditionalOutput#(t)) enq_ <- _ConditionalOutput((tpl_2(asIfc(notFull_)))._read, True);
  return tuple2(
    interface FifoEnq_;
      interface notFull = tpl_2(asIfc(notFull_));
      interface enq = tpl_1(asIfc(enq_));
    endinterface,
    interface FifoEnq;
      interface notFull = tpl_1(asIfc(notFull_));
      interface enq = tpl_2(asIfc(enq_));
    endinterface);
endmodule

instance Connectable#(FifoEnq#(t), FifoEnq_#(t)) provisos(Bits#(t, _sZt));
  module mkConnection#(FifoEnq#(t) a, FifoEnq_#(t) b)();
    mkConnection(asIfc(a.notFull), asIfc(b.notFull));
    mkConnection(asIfc(a.enq), asIfc(b.enq));
  endmodule
endinstance

instance Connectable#(FifoEnq_#(t), FifoEnq#(t)) provisos(Bits#(t, _sZt));
  module mkConnection#(FifoEnq_#(t) a, FifoEnq#(t) b)();
    mkConnection(asIfc(b), asIfc(a));
  endmodule
endinstance

interface FifoDeq_#(type t);
  interface Output#(Bool) notEmpty;
  interface Output#(t) first;
  interface OutputPulse_ deq;
endinterface

interface FifoDeq#(type t);
  interface Output_#(Bool) notEmpty;
  interface Output_#(t) first;
  interface OutputPulse deq;
endinterface

module _FifoDeq(Tuple2#(FifoDeq_#(t), FifoDeq#(t))) provisos(Bits#(t, _sZt));
  Tuple2#(Output_#(Bool), Output#(Bool)) notEmpty_ <- _Output(True, True);
  Tuple2#(Output_#(t), Output#(t)) first_ <- _Output(True, (tpl_2(asIfc(notEmpty_)))._read);
  Tuple2#(OutputPulse_, OutputPulse) deq_ <- _OutputPulse((tpl_2(asIfc(notEmpty_)))._read, True);
  return tuple2(
    interface FifoDeq_;
      interface notEmpty = tpl_2(asIfc(notEmpty_));
      interface first = tpl_2(asIfc(first_));
      interface deq = tpl_1(asIfc(deq_));
    endinterface,
    interface FifoDeq;
      interface notEmpty = tpl_1(asIfc(notEmpty_));
      interface first = tpl_1(asIfc(first_));
      interface deq = tpl_2(asIfc(deq_));
    endinterface);
endmodule

instance Connectable#(FifoDeq#(t), FifoDeq_#(t)) provisos(Bits#(t, _sZt));
  module mkConnection#(FifoDeq#(t) a, FifoDeq_#(t) b)();
    mkConnection(asIfc(a.notEmpty), asIfc(b.notEmpty));
    mkConnection(asIfc(a.first), asIfc(b.first));
    mkConnection(asIfc(a.deq), asIfc(b.deq));
  endmodule
endinstance

instance Connectable#(FifoDeq_#(t), FifoDeq#(t)) provisos(Bits#(t, _sZt));
  module mkConnection#(FifoDeq_#(t) a, FifoDeq#(t) b)();
    mkConnection(asIfc(b), asIfc(a));
  endmodule
endinstance

interface Fifo_#(numeric type n, type t);
  interface FifoEnq#(t) enq;
  interface FifoDeq#(t) deq;
endinterface

interface Fifo#(numeric type n, type t);
  interface FifoEnq_#(t) enq;
  interface FifoDeq_#(t) deq;
endinterface

module _Fifo(Tuple2#(Fifo_#(n, t), Fifo#(n, t))) provisos(Bits#(t, _sZt));
  Tuple2#(FifoEnq_#(t), FifoEnq#(t)) enq_ <- _FifoEnq;
  Tuple2#(FifoDeq_#(t), FifoDeq#(t)) deq_ <- _FifoDeq;
  return tuple2(
    interface Fifo_;
      interface enq = tpl_2(asIfc(enq_));
      interface deq = tpl_2(asIfc(deq_));
    endinterface,
    interface Fifo;
      interface enq = tpl_1(asIfc(enq_));
      interface deq = tpl_1(asIfc(deq_));
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
    mkConnection(asIfc(b), asIfc(a));
  endmodule
endinstance

module mkGenericFifo#(function _m__#(MultiFifo#(n, 1, 1, t)) mkF)(Fifo#(n, t)) provisos(Bits#(t, tSz));
  Tuple2#(Fifo_#(n, t), Fifo#(n, t)) mod_ <- _Fifo;

  MultiFifo#(n, 1, 1, t) f <- mkF;

  (* fire_when_enabled *) rule a;
    (tpl_1(asIfc(mod_))).enq.notFull.write( f.enq.numFreeSlots > 0);
    (tpl_1(asIfc(mod_))).deq.notEmpty.write( f.deq.numFilledSlots > 0);
    f.deq.numDeqs.write( (tpl_1(asIfc(mod_))).deq.deq? 1: 0);
    (tpl_1(asIfc(mod_))).deq.first.write( f.deq.data[0]);
  endrule

  mkConnection(asIfc(f.enq.data[0]), asIfc( (tpl_1(asIfc(mod_))).enq.enq));

  return tpl_2(asIfc(mod_));
endmodule


module mkLFifo(Fifo#(n, t))provisos(Bits#(t, tSz));
  Fifo#(n, t) mod_ <- mkGenericFifo(mkMultiLFifo);
  return mod_;
endmodule


module mkFifo(Fifo#(n, t))provisos(Bits#(t, tSz));
  Fifo#(n, t) mod_ <- mkGenericFifo(mkMultiFifo);
  return mod_;
endmodule


module mkBypassFifo(Fifo#(n, t))provisos(Bits#(t, tSz));
  Fifo#(n, t) mod_ <- mkGenericFifo(mkMultiBypassFifo);
  return mod_;
endmodule

interface UgFifoEnq_#(type t);
  interface Output#(Bool) notFull;
  interface ConditionalOutput_#(t) enq;
endinterface

interface UgFifoEnq#(type t);
  interface Output_#(Bool) notFull;
  interface ConditionalOutput#(t) enq;
endinterface

module _UgFifoEnq(Tuple2#(UgFifoEnq_#(t), UgFifoEnq#(t))) provisos(Bits#(t, _sZt));
  Tuple2#(Output_#(Bool), Output#(Bool)) notFull_ <- _Output(True, True);
  Tuple2#(ConditionalOutput_#(t), ConditionalOutput#(t)) enq_ <- _ConditionalOutput(True, True);
  return tuple2(
    interface UgFifoEnq_;
      interface notFull = tpl_2(asIfc(notFull_));
      interface enq = tpl_1(asIfc(enq_));
    endinterface,
    interface UgFifoEnq;
      interface notFull = tpl_1(asIfc(notFull_));
      interface enq = tpl_2(asIfc(enq_));
    endinterface);
endmodule

instance Connectable#(UgFifoEnq#(t), UgFifoEnq_#(t)) provisos(Bits#(t, _sZt));
  module mkConnection#(UgFifoEnq#(t) a, UgFifoEnq_#(t) b)();
    mkConnection(asIfc(a.notFull), asIfc(b.notFull));
    mkConnection(asIfc(a.enq), asIfc(b.enq));
  endmodule
endinstance

instance Connectable#(UgFifoEnq_#(t), UgFifoEnq#(t)) provisos(Bits#(t, _sZt));
  module mkConnection#(UgFifoEnq_#(t) a, UgFifoEnq#(t) b)();
    mkConnection(asIfc(b), asIfc(a));
  endmodule
endinstance

interface UgFifoDeq_#(type t);
  interface Output#(Bool) notEmpty;
  interface Output#(t) first;
  interface OutputPulse_ deq;
endinterface

interface UgFifoDeq#(type t);
  interface Output_#(Bool) notEmpty;
  interface Output_#(t) first;
  interface OutputPulse deq;
endinterface

module _UgFifoDeq(Tuple2#(UgFifoDeq_#(t), UgFifoDeq#(t))) provisos(Bits#(t, _sZt));
  Tuple2#(Output_#(Bool), Output#(Bool)) notEmpty_ <- _Output(True, True);
  Tuple2#(Output_#(t), Output#(t)) first_ <- _Output(True, True);
  Tuple2#(OutputPulse_, OutputPulse) deq_ <- _OutputPulse(True, True);
  return tuple2(
    interface UgFifoDeq_;
      interface notEmpty = tpl_2(asIfc(notEmpty_));
      interface first = tpl_2(asIfc(first_));
      interface deq = tpl_1(asIfc(deq_));
    endinterface,
    interface UgFifoDeq;
      interface notEmpty = tpl_1(asIfc(notEmpty_));
      interface first = tpl_1(asIfc(first_));
      interface deq = tpl_2(asIfc(deq_));
    endinterface);
endmodule

instance Connectable#(UgFifoDeq#(t), UgFifoDeq_#(t)) provisos(Bits#(t, _sZt));
  module mkConnection#(UgFifoDeq#(t) a, UgFifoDeq_#(t) b)();
    mkConnection(asIfc(a.notEmpty), asIfc(b.notEmpty));
    mkConnection(asIfc(a.first), asIfc(b.first));
    mkConnection(asIfc(a.deq), asIfc(b.deq));
  endmodule
endinstance

instance Connectable#(UgFifoDeq_#(t), UgFifoDeq#(t)) provisos(Bits#(t, _sZt));
  module mkConnection#(UgFifoDeq_#(t) a, UgFifoDeq#(t) b)();
    mkConnection(asIfc(b), asIfc(a));
  endmodule
endinstance

interface UgFifo_#(numeric type n, type t);
  interface UgFifoEnq#(t) enq;
  interface UgFifoDeq#(t) deq;
endinterface

interface UgFifo#(numeric type n, type t);
  interface UgFifoEnq_#(t) enq;
  interface UgFifoDeq_#(t) deq;
endinterface

module _UgFifo(Tuple2#(UgFifo_#(n, t), UgFifo#(n, t))) provisos(Bits#(t, _sZt));
  Tuple2#(UgFifoEnq_#(t), UgFifoEnq#(t)) enq_ <- _UgFifoEnq;
  Tuple2#(UgFifoDeq_#(t), UgFifoDeq#(t)) deq_ <- _UgFifoDeq;
  return tuple2(
    interface UgFifo_;
      interface enq = tpl_2(asIfc(enq_));
      interface deq = tpl_2(asIfc(deq_));
    endinterface,
    interface UgFifo;
      interface enq = tpl_1(asIfc(enq_));
      interface deq = tpl_1(asIfc(deq_));
    endinterface);
endmodule

instance Connectable#(UgFifo#(n, t), UgFifo_#(n, t)) provisos(Bits#(t, _sZt));
  module mkConnection#(UgFifo#(n, t) a, UgFifo_#(n, t) b)();
    mkConnection(asIfc(a.enq), asIfc(b.enq));
    mkConnection(asIfc(a.deq), asIfc(b.deq));
  endmodule
endinstance

instance Connectable#(UgFifo_#(n, t), UgFifo#(n, t)) provisos(Bits#(t, _sZt));
  module mkConnection#(UgFifo_#(n, t) a, UgFifo#(n, t) b)();
    mkConnection(asIfc(b), asIfc(a));
  endmodule
endinstance

module mkGenericUgFifo#(function _m__#(MultiFifo#(n, 1, 1, t)) mkF)(UgFifo#(n, t)) provisos(Bits#(t, tSz));
  Tuple2#(UgFifo_#(n, t), UgFifo#(n, t)) mod_ <- _UgFifo;

  MultiFifo#(n, 1, 1, t) f <- mkF;

  (* fire_when_enabled *) rule a;
    (tpl_1(asIfc(mod_))).enq.notFull.write( f.enq.numFreeSlots > 0);
    (tpl_1(asIfc(mod_))).deq.notEmpty.write( f.deq.numFilledSlots > 0);
    f.deq.numDeqs.write( (tpl_1(asIfc(mod_))).deq.deq? 1: 0);
    (tpl_1(asIfc(mod_))).deq.first.write( f.deq.data[0]);
  endrule

  mkConnection(asIfc(f.enq.data[0]), asIfc( (tpl_1(asIfc(mod_))).enq.enq));

  return tpl_2(asIfc(mod_));
endmodule


module mkLUgFifo(UgFifo#(n, t))provisos(Bits#(t, tSz));
  UgFifo#(n, t) mod_ <- mkGenericUgFifo(mkMultiLFifo);
  return mod_;
endmodule


module mkUgFifo(UgFifo#(n, t))provisos(Bits#(t, tSz));
  UgFifo#(n, t) mod_ <- mkGenericUgFifo(mkMultiFifo);
  return mod_;
endmodule


module mkBypassUgFifo(UgFifo#(n, t))provisos(Bits#(t, tSz));
  UgFifo#(n, t) mod_ <- mkGenericUgFifo(mkMultiBypassFifo);
  return mod_;
endmodule


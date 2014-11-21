import Vector::*;
import HaskellLib::*;
import Connectable::*;
import Base::*;
import Primitive::*;
export Writeback::*;

import Library::*;
export Library::*;

import Types::*;
export Types::*;

import Fifo::*;
export Fifo::*;

interface Writeback_;
  interface FifoEnq#(Pair#(RegIndex, Maybe#(Data))) wb;
  interface ConditionalOutput_#(RegIndex) wbIndex;
  interface FifoDeq_#(Data) dataQ;
  interface ConditionalOutput_#(Pair#(RegIndex, Data)) regWrite;
endinterface

interface Writeback;
  interface FifoEnq_#(Pair#(RegIndex, Maybe#(Data))) wb;
  interface ConditionalOutput#(RegIndex) wbIndex;
  interface FifoDeq#(Data) dataQ;
  interface ConditionalOutput#(Pair#(RegIndex, Data)) regWrite;
endinterface

module _Writeback(Tuple2#(Writeback_, Writeback)) ;
  Tuple2#(FifoEnq_#(Pair#(RegIndex, Maybe#(Data))), FifoEnq#(Pair#(RegIndex, Maybe#(Data)))) wb_ <- _FifoEnq;
  Tuple2#(ConditionalOutput_#(RegIndex), ConditionalOutput#(RegIndex)) wbIndex_ <- _ConditionalOutput(True, True);
  Tuple2#(FifoDeq_#(Data), FifoDeq#(Data)) dataQ_ <- _FifoDeq;
  Tuple2#(ConditionalOutput_#(Pair#(RegIndex, Data)), ConditionalOutput#(Pair#(RegIndex, Data))) regWrite_ <- _ConditionalOutput(True, True);
  return tuple2(
    interface Writeback_;
      interface wb = tpl_2(asIfc(wb_));
      interface wbIndex = tpl_1(asIfc(wbIndex_));
      interface dataQ = tpl_1(asIfc(dataQ_));
      interface regWrite = tpl_1(asIfc(regWrite_));
    endinterface,
    interface Writeback;
      interface wb = tpl_1(asIfc(wb_));
      interface wbIndex = tpl_2(asIfc(wbIndex_));
      interface dataQ = tpl_2(asIfc(dataQ_));
      interface regWrite = tpl_2(asIfc(regWrite_));
    endinterface);
endmodule

instance Connectable#(Writeback, Writeback_) ;
  module mkConnection#(Writeback a, Writeback_ b)();
    mkConnection(asIfc(a.wb), asIfc(b.wb));
    mkConnection(asIfc(a.wbIndex), asIfc(b.wbIndex));
    mkConnection(asIfc(a.dataQ), asIfc(b.dataQ));
    mkConnection(asIfc(a.regWrite), asIfc(b.regWrite));
  endmodule
endinstance

instance Connectable#(Writeback_, Writeback) ;
  module mkConnection#(Writeback_ a, Writeback b)();
    mkConnection(asIfc(b), asIfc(a));
  endmodule
endinstance

(* synthesize *)
module mkWriteback(Writeback) ;
  Tuple2#(Writeback_, Writeback) mod_ <- _Writeback;

  Fifo#(1, Pair#(RegIndex, Maybe#(Data))) wbQ <- mkLFifo;

  mkConnection(asIfc((tpl_1(asIfc(mod_))).wb), asIfc( wbQ.enq));

  (* fire_when_enabled *) rule r1;
    (tpl_1(asIfc(mod_))).wbIndex.write( wbQ.deq.first.fst);
  endrule

  (* fire_when_enabled *) rule r2;
    if(wbQ.deq.first.snd matches tagged Valid .d)
    begin
      (tpl_1(asIfc(mod_))).regWrite.write( Pair{fst: wbQ.deq.first.fst, snd: d});
      wbQ.deq.deq;
    end
    else
    begin
      (tpl_1(asIfc(mod_))).regWrite.write( Pair{fst: wbQ.deq.first.fst, snd: (tpl_1(asIfc(mod_))).dataQ.first});
      wbQ.deq.deq;
      (tpl_1(asIfc(mod_))).dataQ.deq;
    end
  endrule

  return tpl_2(asIfc(mod_));
endmodule


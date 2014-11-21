import Vector::*;
import HaskellLib::*;
import Connectable::*;
import Base::*;
import Primitive::*;
export Mem::*;

import Library::*;
export Library::*;

import Types::*;
export Types::*;

import RegsFile::*;
export RegsFile::*;

import Fifo::*;
export Fifo::*;

interface Memory_;
  interface FifoDeq_#(VAddr) instReqQ;
  interface FifoEnq_#(Inst) instQ;
  interface FifoDeq_#(Mem) dataReqQ;
  interface FifoEnq_#(Inst) dataQ;
endinterface

interface Memory;
  interface FifoDeq#(VAddr) instReqQ;
  interface FifoEnq#(Inst) instQ;
  interface FifoDeq#(Mem) dataReqQ;
  interface FifoEnq#(Inst) dataQ;
endinterface

module _Memory(Tuple2#(Memory_, Memory)) ;
  Tuple2#(FifoDeq_#(VAddr), FifoDeq#(VAddr)) instReqQ_ <- _FifoDeq;
  Tuple2#(FifoEnq_#(Inst), FifoEnq#(Inst)) instQ_ <- _FifoEnq;
  Tuple2#(FifoDeq_#(Mem), FifoDeq#(Mem)) dataReqQ_ <- _FifoDeq;
  Tuple2#(FifoEnq_#(Inst), FifoEnq#(Inst)) dataQ_ <- _FifoEnq;
  return tuple2(
    interface Memory_;
      interface instReqQ = tpl_1(asIfc(instReqQ_));
      interface instQ = tpl_1(asIfc(instQ_));
      interface dataReqQ = tpl_1(asIfc(dataReqQ_));
      interface dataQ = tpl_1(asIfc(dataQ_));
    endinterface,
    interface Memory;
      interface instReqQ = tpl_2(asIfc(instReqQ_));
      interface instQ = tpl_2(asIfc(instQ_));
      interface dataReqQ = tpl_2(asIfc(dataReqQ_));
      interface dataQ = tpl_2(asIfc(dataQ_));
    endinterface);
endmodule

instance Connectable#(Memory, Memory_) ;
  module mkConnection#(Memory a, Memory_ b)();
    mkConnection(asIfc(a.instReqQ), asIfc(b.instReqQ));
    mkConnection(asIfc(a.instQ), asIfc(b.instQ));
    mkConnection(asIfc(a.dataReqQ), asIfc(b.dataReqQ));
    mkConnection(asIfc(a.dataQ), asIfc(b.dataQ));
  endmodule
endinstance

instance Connectable#(Memory_, Memory) ;
  module mkConnection#(Memory_ a, Memory b)();
    mkConnection(asIfc(b), asIfc(a));
  endmodule
endinstance

(* synthesize *)
module mkMemory(Memory) ;
  Tuple2#(Memory_, Memory) mod_ <- _Memory;

  RegFile#(2, 1, TExp#(20), Data) regs <- mkRegFileVmh("../memory.vmh");

  (* fire_when_enabled *) rule r1;
    regs.read[0].req.write( truncate((tpl_1(asIfc(mod_))).instReqQ.first>>2));
    (tpl_1(asIfc(mod_))).instReqQ.deq;
    (tpl_1(asIfc(mod_))).instQ.enq.write( regs.read[0].resp);
  endrule

  (* fire_when_enabled *) rule r2;
    (tpl_1(asIfc(mod_))).dataReqQ.deq;
    case ((tpl_1(asIfc(mod_))).dataReqQ.first) matches
      tagged Store (tagged Pair{fst: .addr, snd:.data}):
        regs.write[0].write( Pair{fst: truncate(addr>>2), snd: data});
      tagged Load .addr:
        begin
          regs.read[1].req.write( truncate(addr>>2));
          (tpl_1(asIfc(mod_))).dataQ.enq.write( regs.read[1].resp);
        end
    endcase
  endrule

  return tpl_2(asIfc(mod_));
endmodule


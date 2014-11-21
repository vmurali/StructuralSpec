import Vector::*;
import HaskellLib::*;
import Connectable::*;
import Base::*;
import Primitive::*;
export Core::*;

import Fifo::*;
export Fifo::*;

import Types::*;
export Types::*;

import Fetch::*;
export Fetch::*;

import Execute::*;
export Execute::*;

import Writeback::*;
export Writeback::*;

import Registers::*;
export Registers::*;

import Mem::*;
export Mem::*;

import Cop::*;
export Cop::*;

interface Core_;
  interface Memory mem;
endinterface

interface Core;
  interface Memory_ mem;
endinterface

module _Core(Tuple2#(Core_, Core)) ;
  Tuple2#(Memory_, Memory) mem_ <- _Memory;
  return tuple2(
    interface Core_;
      interface mem = tpl_2(asIfc(mem_));
    endinterface,
    interface Core;
      interface mem = tpl_1(asIfc(mem_));
    endinterface);
endmodule

instance Connectable#(Core, Core_) ;
  module mkConnection#(Core a, Core_ b)();
    mkConnection(asIfc(a.mem), asIfc(b.mem));
  endmodule
endinstance

instance Connectable#(Core_, Core) ;
  module mkConnection#(Core_ a, Core b)();
    mkConnection(asIfc(b), asIfc(a));
  endmodule
endinstance

(* synthesize *)

module mkPcQ(Fifo#(3, Pair#(VAddr, Bool)));
  Fifo#(3, Pair#(VAddr, Bool)) mod_ <- mkLFifo;
  return mod_;
endmodule

(* synthesize *)

module mkInstReqQ(Fifo#(6, VAddr));
  Fifo#(6, VAddr) mod_ <- mkLFifo;
  return mod_;
endmodule

(* synthesize *)

module mkInstQ(Fifo#(2, Inst));
  Fifo#(2, Inst) mod_ <- mkLFifo;
  return mod_;
endmodule

(* synthesize *)

module mkDataReqQ(Fifo#(1, Mem));
  Fifo#(1, Mem) mod_ <- mkLFifo;
  return mod_;
endmodule

(* synthesize *)

module mkDataQ(Fifo#(4, Data));
  Fifo#(4, Data) mod_ <- mkLFifo;
  return mod_;
endmodule

(* synthesize *)
module mkCore(Core) ;
  Tuple2#(Core_, Core) mod_ <- _Core;

  let    fetch <- mkFetch;
  let  execute <- mkExecute;
  let       wb <- mkWriteback;
  let      cop <- mkCop;
  let     regs <- mkRegisters;

  let      pcQ <- mkPcQ;
  let instReqQ <- mkInstReqQ;
  let    instQ <- mkInstQ;
  let dataReqQ <- mkDataReqQ;
  let    dataQ <- mkDataQ;

  mkConnection(asIfc(fetch.currEpoch), asIfc( execute.currEpoch));
  mkConnection(asIfc(fetch.pcQ), asIfc( pcQ.enq));
  mkConnection(asIfc(fetch.instReqQ), asIfc( instReqQ.enq));
  mkConnection(asIfc(fetch.branchPc), asIfc( execute.branchPc));

  mkConnection(asIfc(execute.pcQ), asIfc( pcQ.deq));
  mkConnection(asIfc(execute.instQ), asIfc( instQ.deq));
  mkConnection(asIfc(execute.dataReqQ), asIfc( dataReqQ.enq));
  mkConnection(asIfc(execute.regRead), asIfc( regs.read));
  mkConnection(asIfc(execute.wbQ), asIfc( wb.wb));
  mkConnection(asIfc(execute.wbIndex), asIfc( wb.wbIndex));
  mkConnection(asIfc(execute.cop), asIfc( cop));

  mkConnection(asIfc(wb.dataQ), asIfc( dataQ.deq));
  mkConnection(asIfc(wb.regWrite), asIfc( regs.write));

  mkConnection(asIfc((tpl_1(asIfc(mod_))).mem.instReqQ), asIfc( instReqQ.deq));
  mkConnection(asIfc((tpl_1(asIfc(mod_))).mem.instQ), asIfc( instQ.enq));
  mkConnection(asIfc((tpl_1(asIfc(mod_))).mem.dataReqQ), asIfc( dataReqQ.deq));
  mkConnection(asIfc((tpl_1(asIfc(mod_))).mem.dataQ), asIfc( dataQ.enq));

  return tpl_2(asIfc(mod_));
endmodule


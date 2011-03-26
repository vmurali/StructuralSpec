import Vector::*;
import HaskellLib::*;
import Connectable::*;
import Base::*;
import Primitive::*;

import Fifo::*;
import Types::*;
import FetchPort::*;
import Fetch::*;
import ExecutePort::*;
import Execute::*;
import WritebackPort::*;
import Writeback::*;
import RegistersPort::*;
import Registers::*;
import MemoryPort::*;
import Memory::*;
interface Processor_;
  method Action specCycleInputDone();
  method Action specCycleOutputDone();
  method Bool isSupplied();
endinterface

interface Processor;
  method Action specCycleInputDone();
  method Action specCycleOutputDone();
  method Bool isSupplied();
endinterface

module _Processor(Tuple2#(Processor_, Processor)) ;
  return tuple2(
    interface Processor_;
      method Action specCycleInputDone();
      endmethod
      method Action specCycleOutputDone();
      endmethod
      method Bool isSupplied = True ;
    endinterface,
    interface Processor;
      method Action specCycleInputDone();
      endmethod
      method Action specCycleOutputDone();
      endmethod
      method Bool isSupplied = True ;
    endinterface);
endmodule

instance Connectable#(Processor, Processor_) ;
  module mkConnection#(Processor a, Processor_ b)();
  endmodule
endinstance

instance Connectable#(Processor_, Processor) ;
  module mkConnection#(Processor_ a, Processor b)();
  endmodule
endinstance

instance Sync_#(Processor);
  function Action _specCycleInputDone(Processor x) = x.specCycleInputDone;
  function Action _specCycleOutputDone(Processor x) = x.specCycleOutputDone;
  function Bool _isSupplied(Processor x) = x.isSupplied;
endinstance

instance Sync_#(Processor_);
  function Action _specCycleInputDone(Processor_ x) = x.specCycleInputDone;
  function Action _specCycleOutputDone(Processor_ x) = x.specCycleOutputDone;
  function Bool _isSupplied(Processor_ x) = x.isSupplied;
endinstance

(* synthesize *)
module mkProcessor(Processor) ;
  Tuple2#(Processor_, Processor) mod_ <- _Processor;

  Fetch                        fetch <- mkFetch;
  Execute                    execute <- mkExecute;
  Writeback                       wb <- mkWriteback;

  Registers                     regs <- mkRegisters;
  Memory                         mem <- mkMemory;

  Fifo#(1, Tuple2#(VAddr, Bool)) pcQ <- mkLFifo;

  Fifo#(1, VAddr)           instReqQ <- mkLFifo;
  Fifo#(1, Inst)               instQ <- mkLFifo;

  Fifo#(1, Mem)             dataReqQ <- mkLFifo;
  Fifo#(1, Data)               dataQ <- mkLFifo;

  mkConnection(fetch.currEpoch, execute.currEpoch);
  mkConnection(fetch.pcQ, pcQ.enq);
  mkConnection(fetch.instReqQ, instReqQ.enq);
  mkConnection(fetch.branchPc, execute.branchPc);

  mkConnection(execute.pcQ, pcQ.deq);
  mkConnection(execute.instQ, instQ.deq);
  mkConnection(execute.dataReqQ, dataReqQ.enq);
  mkConnection(execute.regRead, regs.read);
  mkConnection(execute.wbQ, wb.wb);
  mkConnection(execute.wbIndex, wb.wbIndex);

  mkConnection(wb.dataQ, dataQ.deq);
  mkConnection(wb.regWrite, regs.write);

  mkConnection(mem.instReqQ, instReqQ.deq);
  mkConnection(mem.instQ, instQ.enq);
  mkConnection(mem.dataReqQ, dataReqQ.deq);
  mkConnection(mem.dataQ, dataQ.enq);

  return tpl_2(asIfc(mod_));
endmodule


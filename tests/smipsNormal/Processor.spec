include LibraryNormal;

include FifoNormal;
include Types;
include Fetch;
include Execute;
include Writeback;
include Registers;
include Memory;
include Cop;

port Core;
  Reverse Memory mem;
endport

(* synthesize *)
partinst FifoNormal#(3, PcQ) mkPcQ = mkLFifoNormal;
(* synthesize *)
partinst FifoNormal#(6, VAddr) mkInstReqQ = mkLFifoNormal;
(* synthesize *)
partinst FifoNormal#(2, Inst) mkInstQ = mkLFifoNormal;
(* synthesize *)
partinst FifoNormal#(1, Mem) mkDataReqQ = mkLFifoNormal;
(* synthesize *)
partinst FifoNormal#(4, Data) mkDataQ = mkLFifoNormal;

(* synthesize *)
partition Core mkCore;
  Fetch                        fetch <- mkFetch;
  Execute                    execute <- mkExecute;
  Writeback                       wb <- mkWriteback;

  Cop                            cop <- mkCop;
  Registers                     regs <- mkRegisters;

  FifoNormal#(3, PcQ)                  pcQ <- mkPcQ;

  FifoNormal#(6, VAddr)           instReqQ <- mkInstReqQ;
  FifoNormal#(2, Inst)               instQ <- mkInstQ;

  FifoNormal#(1, Mem)             dataReqQ <- mkDataReqQ;
  FifoNormal#(4, Data)               dataQ <- mkDataQ;

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
  mkConnection(execute.cop, cop);

  mkConnection(wb.dataQ, dataQ.deq);
  mkConnection(wb.regWrite, regs.write);

  mkConnection(mem.instReqQ, instReqQ.deq);
  mkConnection(mem.instQ, instQ.enq);
  mkConnection(mem.dataReqQ, dataReqQ.deq);
  mkConnection(mem.dataQ, dataQ.enq);
endpartition

(* synthesize *)
partition EmptyNormal mkProcessor;
  Core   core <- mkCore;
  Memory  mem <- mkMemory;

  mkConnection(core.mem, mem);
endpartition

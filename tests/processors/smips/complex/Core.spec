include Fifo;
include Types;
include Fetch;
include Execute;
include Writeback;
include Registers;
include Mem;
include Cop;

port Core;
  Reverse Memory mem;
endport

(* synthesize *)
partinst Fifo#(3, Pair#(VAddr, Bool)) mkPcQ = mkLFifo;
(* synthesize *)
partinst Fifo#(6, VAddr) mkInstReqQ = mkLFifo;
(* synthesize *)
partinst Fifo#(2, Inst) mkInstQ = mkLFifo;
(* synthesize *)
partinst Fifo#(1, Mem) mkDataReqQ = mkLFifo;
(* synthesize *)
partinst Fifo#(4, Data) mkDataQ = mkLFifo;

(* synthesize *)
partition Core mkCore;
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

include Fifo;
include Types;
include FetchPort;
include Fetch;
include ExecutePort;
include Execute;
include WritebackPort;
include Writeback;
include RegistersPort;
include Registers;
include MemoryPort;
include Memory;

port Processor;
endport

(* synthesize *)
partition mkProcessor implements Processor;
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
endpartition

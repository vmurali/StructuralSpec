include Library;
include Types;
include RegFileNormal;
include FifoNormal;

port Memory;
  FifoDeqNormal#(VAddr) instReqQ;
  FifoEnqNormal#(Inst) instQ;
  FifoDeqNormal#(Mem) dataReqQ;
  FifoEnqNormal#(Inst) dataQ;
endport

(* synthesize *)
partition Memory mkMemory;
  RegFileNormal#(2, 1, TExp#(20), Data) regs <- mkRegFileVmhNormal("../memory.vmh");

  atomic r1;
    regs.read[0].req := truncate(instReqQ.first>>2);
    instReqQ.deq;
    instQ.enq := regs.read[0].resp;
  endatomic

  atomic r2;
    dataReqQ.deq;
    case (dataReqQ.first) matches
      tagged Store (tagged MemWrite{index: .addr, data:.data}):
        regs.write[0] := RegWrite{index: truncate(addr>>2), data: data};
      tagged Load .addr:
        begin
          regs.read[1].req := truncate(addr>>2);
          dataQ.enq := regs.read[1].resp;
        end
    endcase
  endatomic
endpartition
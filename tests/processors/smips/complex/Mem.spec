include Library;
include Types;
include RegsFile;
include Fifo;

port Memory;
  FifoDeq#(VAddr) instReqQ;
  FifoEnq#(Inst) instQ;
  FifoDeq#(Mem) dataReqQ;
  FifoEnq#(Inst) dataQ;
endport

(* synthesize *)
partition Memory mkMemory;
  RegFile#(2, 1, TExp#(20), Data) regs <- mkRegFileVmh("../memory.vmh");

  atomic r1;
    regs.read[0].req := truncate(instReqQ.first>>2);
    instReqQ.deq;
    instQ.enq := regs.read[0].resp;
  endatomic

  atomic r2;
    dataReqQ.deq;
    case (dataReqQ.first) matches
      tagged Store (tagged Pair{fst: .addr, snd:.data}):
        regs.write[0] := Pair{fst: truncate(addr>>2), snd: data};
      tagged Load .addr:
        begin
          regs.read[1].req := truncate(addr>>2);
          dataQ.enq := regs.read[1].resp;
        end
    endcase
  endatomic
endpartition

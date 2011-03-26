include Library;
include Types;
include RegFile;
include MemoryPort;

(* synthesize *)
partition mkMemory implements Memory;
  RegFile#(2, 1, 20, Data) regs <- mkRegFileLoad("memory.vmh", False);

  rule r1;
    regs.read[0].req := truncate(instReqQ.first);
    instReqQ.deq;
    instQ.enq := regs.read[0].resp;
  endrule

  rule r2;
    case (dataReqQ.first.data) matches
      tagged Valid .d:
        regs.write.data := tuple2(dataReqQ.first.index, d);
      tagged Invalid:
        begin
          regs.read[1].req := dataReqQ.first.index;
          dataQ.enq := regs.read[1].resp;
        end
    endcase
  endrule
endpartition

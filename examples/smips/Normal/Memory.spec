include Library;
include Types;
include RegFile;
include MemoryPort;
include Fifo;

(* synthesize *)
partition mkMemory implements Memory;
  RegFile#(2, 1, 20, Data) regs <- mkRegFileLoad("memory.vmh", False);

  rule r1;
    regs.read[0].req := truncate(instReqQ.first>>2);
    instReqQ.deq;
    instQ.data := regs.read[0].resp;
  endrule

  rule r2;
    dataReqQ.deq;
    case (dataReqQ.first) matches
      tagged Store {.addr, .data}:
        regs.write[0].data := tuple2(truncate(addr>>2), data);
      tagged Load .addr:
        begin
          regs.read[1].req := truncate(addr>>2);
          dataQ.data := regs.read[1].resp;
        end
    endcase
  endrule
endpartition

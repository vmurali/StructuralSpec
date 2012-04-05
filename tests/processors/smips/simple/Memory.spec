include Types;
include RegFile;

port Memory;
  Input#(VAddr) iReq;
  Output#(Inst) iResp;
  ConditionalInput#(Mem) dReq;
  Output#(Data) dResp;
endport

(* synthesize *)
partition Memory mkMemory;
  RegFile#(2, 1, TExp#(20), Data) regs <- mkRegFileVmh("../memory.vmh");

  atomic r1;
    regs.read[0].req := truncate(iReq>>2);
    iResp := regs.read[0].resp;

    if(dReq.en)
    begin
      case (dReq) matches
        tagged Store {.addr, .data}:
          regs.write[0] := Pair{fst: truncate(addr>>2), snd: data};
        tagged Load .addr:
          begin
            regs.read[1].req := truncate(addr>>2);
            dResp := regs.read[1].resp;
          end
      endcase
    end
  endatomic
endpartition

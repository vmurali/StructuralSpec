include Library;
include Types;
include RegFile;
include MemoryPort;
include Fifo;

(* synthesize *)
partition mkMemory implements Memory;
  RegFile#(2, 1, 20, Data) regs <- mkRegFileLoad("../memory.vmh", False);

  rule r1;
    if(instReqQ.rdy && instQ.rdy)
      regs.read[0].req := truncate(instReqQ.first>>2);
    else
      regs.read[0].req.justFinish;
  endrule

  rule r2;
    if(instReqQ.rdy && instQ.rdy)
      instReqQ.deq;
    else
      instReqQ.deq.justFinish;
  endrule

  rule r3;
    if(instReqQ.rdy && instQ.rdy)
      instQ.data := regs.read[0].resp;
    else
      instQ.data.justFinish;
  endrule

  let syncFiresR2 = dataReqQ.rdy &&
                    (dataReqQ.first matches tagged Store .*?
                       True:
                    (dataReqQ.first matches tagged Load .*?
                       dataQ.rdy:
                    True));

  rule r4;
    if(syncFiresR2)
      dataReqQ.deq;
    else
      dataReqQ.deq.justFinish;
  endrule

  rule r5;
    if(syncFiresR2)
    begin
      case (dataReqQ.first) matches
        tagged Store {.addr, .data}:
          regs.write[0].data := tuple2(truncate(addr>>2), data);
        tagged Load .addr:
          regs.write[0].data.justFinish;
        default:
          regs.write[0].data.justFinish;
      endcase
    end
    else
      regs.write[0].data.justFinish;
  endrule

  rule r6;
    if(syncFiresR2)
    begin
      case (dataReqQ.first) matches
        tagged Store {.addr, .data}:
          regs.read[1].req.justFinish;
        tagged Load .addr:
          regs.read[1].req := truncate(addr>>2);
        default:
          regs.read[1].req.justFinish;
      endcase
    end
    else
      regs.read[1].req.justFinish;
  endrule

  rule r7;
    if(syncFiresR2)
    begin
      case (dataReqQ.first) matches
        tagged Store {.addr, .data}:
          dataQ.data.justFinish;
        tagged Load .addr:
          dataQ.data := regs.read[1].resp;
        default:
          dataQ.data.justFinish;
      endcase
    end
    else
      dataQ.data.justFinish;
  endrule

  rule r8;
    specCycleDone;
  endrule
endpartition

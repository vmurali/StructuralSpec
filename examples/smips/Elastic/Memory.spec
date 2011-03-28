include Library;
include Types;
include RegFile;
include MemoryPort;
include Fifo;

(* synthesize *)
partition mkMemory implements Memory;
  RegFile#(2, 1, 20, Data) regs <- mkRegFileLoad("memory.vmh", False);

  rule r1;
    if(instReqQ.rdy && instQ.rdy)
      regs.read[0].req := truncate(instReqQ.first);
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

  rule r4;
    if(dataReqQ.first matches tagged Store .*)
      dataReqQ.deq;
    else if(dataReqQ.first matches tagged Load .* &&& dataQ.rdy)
      dataReqQ.deq;
    else
      dataReqQ.deq.justFinish;
  endrule

  rule r5;
    if(dataReqQ.first matches tagged Store {.addr, .data})
      regs.write[0].data := tuple2(truncate(addr>>2), data);
    else
      regs.write[0].data.justFinish;
  endrule

  rule r6;
    if(dataReqQ.first matches tagged Load .addr &&& dataQ.rdy)
      regs.read[1].req := truncate(addr>>2);
    else
      regs.read[1].req.justFinish;
  endrule

  rule r7;
    if(dataReqQ.first matches tagged Load .addr &&& dataQ.rdy)
      dataQ.data := regs.read[1].resp;
    else
      dataQ.data.justFinish;
  endrule

  rule r8;
    specCycleDone;
  endrule
endpartition

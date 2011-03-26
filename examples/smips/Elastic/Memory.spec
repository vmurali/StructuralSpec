include Library;
include Types;
include RegFile;
include MemoryPort;

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
      instQ.enq := regs.read[0].resp;
    else
      instQ.enq.justFinish;
  endrule

  rule r4;
    if(dataReqQ.first.data matches tagged Valid .d)
      regs.write.data := tuple2(dataReqQ.first.index, d);
    else
      regs.write.data.justFinish;
  endrule

  rule r5;
    if(!isValid(dataReqQ.first.data matches tagged Invalid) && dataQ.rdy)
      regs.read[1].req := dataReqQ.first.index;
    else
      regs.read[1].req.justFinish;
  endrule

  rule r6;
    if(!isValid(dataReqQ.first.data matches tagged Invalid) && dataQ.rdy)
      dataQ.enq := regs.read[1].resp;
    else
      dataQ.enq.justFinish;
  endrule

  rule r7;
    specCycleDone;
  endrule
endpartition

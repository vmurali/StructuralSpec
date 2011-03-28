include Library;
include Types;
include Fifo;
include RegFile;
include Cop;
include ExecutePort;

(* synthesize *)
partition mkExecute implements Execute;
  let inst = instQ.first;
  let reg1 = inst[25:21];
  let reg2 = inst[20:16];

  rule r1;
    if(instQ.rdy)
      regRead[0].req := reg1;
    else
      regRead[0].req := ?;
  endrule

  rule r2;
    if(instQ.rdy)
      regRead[1].req := reg2;
    else
      regRead[1].req := ?;
  endrule

  match {.pcPlus4, .epoch} = pcQ.first;

  let stall = (isSrcValid(inst)[0] && wbIndex.en && wbIndex.data == reg1 || isSrcValid(inst)[1] && wbIndex.en && wbIndex.data == reg2);

  rule r3;
    if(pcQ.rdy && instQ.rdy && (epoch != currEpoch || !stall))
      pcQ.deq;
    else
      pcQ.deq.justFinish;
  endrule

  rule r4;
    if(pcQ.rdy && instQ.rdy && (epoch != currEpoch || !stall))
      instQ.deq;
    else
      instQ.deq.justFinish;
  endrule

  let dest = getDest(inst);
  match {.taken, .branchTarget, .isLinked} = branch(inst, regRead[0].resp, regRead[1].resp, pcPlus4);

  rule r5;
    if(pcQ.rdy && instQ.rdy && (epoch == currEpoch) && !stall && isBranch(inst) && taken)
      branchPc.data := branchTarget;
    else
      branchPc.data.justFinish;
  endrule

  let res = aluDataAddr(inst, regRead[0].resp, regRead[1].resp);

  rule r6;
    if(pcQ.rdy && instQ.rdy && (epoch == currEpoch) && !stall)
    begin
      if(isBranch(inst))
      begin
        if(isLinked)
          wbQ.data := Wb{index: dest, data: tagged Valid pcPlus4};
        else
          wbQ.data.justFinish;
      end
      else if(isLoad(inst))
        wbQ.data := Wb{index: dest, data: tagged Invalid};
      else if(copRead(inst))
        wbQ.data := Wb{index: dest, data: tagged Valid (cop.read)};
      else if(!(isStore(inst) || copWrite(inst)))
        wbQ.data := Wb{index: dest, data: tagged Valid res};
      else
        wbQ.data.justFinish;
    end
    else
      wbQ.data.justFinish;
  endrule

  rule r7;
    if(pcQ.rdy && instQ.rdy && (epoch == currEpoch) && !stall)
    begin
      if(isLoad(inst))
        dataReqQ.data := tagged Load aluDataAddr(inst, regRead[0].resp, regRead[1].resp);
      else if(isStore(inst))
        dataReqQ.data := tagged Store tuple2(res, regRead[1].resp);
      else
        dataReqQ.data.justFinish;
    end
    else
      dataReqQ.data.justFinish;
  endrule

  rule r8;
    if(pcQ.rdy && instQ.rdy && (epoch == currEpoch) && !stall && copWrite(inst))
      cop.write.data := tuple2(copReg(inst), regRead[1].resp);
    else
      cop.write.data.justFinish;
  endrule

  rule r9;
    specCycleDone;
  endrule
endpartition

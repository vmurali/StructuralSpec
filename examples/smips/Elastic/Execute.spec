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
  match {.pcPlus4, .epoch} = pcQ.first;
  let stall = (isSrcValid(inst)[0] && wbIndex.en && wbIndex.data == reg1 || isSrcValid(inst)[1] && wbIndex.en && wbIndex.data == reg2);
  let dest = getDest(inst);
  match {.taken, .branchTarget, .isLinked} = branch(inst, regRead[0].resp, regRead[1].resp, pcPlus4);
  let res = aluDataAddr(inst, regRead[0].resp, regRead[1].resp);

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

  let syncFiresR2 = pcQ.rdy &&
                    epoch != currEpoch?
                      pcQ.rdy && instQ.rdy:
                    instQ.rdy &&
                    (!stall?
                       instQ.rdy && pcQ.rdy &&
                       (isBranch(inst)?
                          (isLinked?
                             wbQ.rdy:
                             True):
                       (isLoad(inst)?
                          wbQ.rdy && dataReqQ.rdy:
                       (isStore(inst)?
                          dataReqQ.rdy:
                       (copRead(inst)?
                          wbQ.rdy:
                       (copWrite(inst)?
                          True:
                       wbQ.rdy))))):
                    True);

  rule r3;
    if(syncFiresR2)
    begin
      if(epoch != currEpoch)
        pcQ.deq;
      else if(!stall)
        pcQ.deq;
      else
        pcQ.deq.justFinish;
    end
    else
      pcQ.deq.justFinish;
  endrule

  rule r4;
    if(syncFiresR2)
    begin
      if(epoch != currEpoch)
        instQ.deq;
      else if(!stall)
        instQ.deq;
      else
        instQ.deq.justFinish;
    end
    else
      instQ.deq.justFinish;
  endrule

  rule r5;
    if(syncFiresR2)
    begin
      if(epoch != currEpoch)
        branchPc.data.justFinish;
      else if(!stall)
      begin
        if(isBranch(inst))
        begin
          if(taken)
            branchPc.data := branchTarget;
          else
            branchPc.data.justFinish;
        end
        else
          branchPc.data.justFinish;
      end
      else
        branchPc.data.justFinish;
    end
    else
      branchPc.data.justFinish;
  endrule

  rule r6;
    if(syncFiresR2)
    begin
      if(epoch != currEpoch)
        wbQ.data.justFinish;
      else if(!stall)
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
        else if(isStore(inst))
          wbQ.data.justFinish;
        else if(copRead(inst))
          wbQ.data := Wb{index: dest, data: tagged Valid (cop.read)};
        else if(copWrite(inst))
          wbQ.data.justFinish;
        else
          wbQ.data := Wb{index: dest, data: tagged Valid res};
      end
      else
        wbQ.data.justFinish;
    end
    else
      wbQ.data.justFinish;
  endrule

  rule r7;
    if(syncFiresR2)
    begin
      if(epoch != currEpoch)
        dataReqQ.data.justFinish;
      else if(!stall)
      begin
        if(isBranch(inst))
          dataReqQ.data.justFinish;
        else if(isLoad(inst))
          dataReqQ.data := tagged Load aluDataAddr(inst, regRead[0].resp, regRead[1].resp);
        else if(isStore(inst))
          dataReqQ.data := tagged Store tuple2(res, regRead[1].resp);
        else if(copRead(inst))
          dataReqQ.data.justFinish;
        else if(copWrite(inst))
          dataReqQ.data.justFinish;
        else
          dataReqQ.data.justFinish;
      end
      else
        dataReqQ.data.justFinish;
    end
    else
      dataReqQ.data.justFinish;
  endrule

  rule r8;
    if(syncFiresR2)
    begin
      if(epoch != currEpoch)
        cop.write.data.justFinish;
      else if(!stall)
      begin
        if(isBranch(inst))
          cop.write.data.justFinish;
        else if(isLoad(inst))
          cop.write.data.justFinish;
        else if(isStore(inst))
          cop.write.data.justFinish;
        else if(copRead(inst))
          cop.write.data.justFinish;
        else if(copWrite(inst))
          cop.write.data := tuple2(copReg(inst), regRead[1].resp);
        else
          cop.write.data.justFinish;
      end
      else
        cop.write.data.justFinish;
    end
    else
      cop.write.data.justFinish;
  endrule

  rule r9;
    specCycleDone;
  endrule
endpartition

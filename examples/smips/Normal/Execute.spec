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

  rule r1;
    regRead[0].req := reg1;
    regRead[1].req := reg2;
  endrule

  rule r2;
    if(epoch != currEpoch)
    begin
      pcQ.deq;
      instQ.deq;
    end
    else
    begin
      let stall = (isSrcValid(inst)[0] && wbIndex.en && wbIndex.data == reg1 || isSrcValid(inst)[1] && wbIndex.en && wbIndex.data == reg2);
      if(!stall)
      begin
        pcQ.deq;
        instQ.deq;
        let dest = getDest(inst);
        if(isBranch(inst))
        begin
          match {.taken, .branchTarget, .isLinked} = branch(inst, regRead[0].resp, regRead[1].resp, pcPlus4);
          if(taken)
            branchPc.data := branchTarget;
          if(isLinked)
            wbQ.data := Wb{index: dest, data: tagged Valid pcPlus4};
        end
        else
        begin
          let res = aluDataAddr(inst, regRead[0].resp, regRead[1].resp);
          if(isLoad(inst))
          begin
            wbQ.data := Wb{index: dest, data: tagged Invalid};
            dataReqQ.data := tagged Load aluDataAddr(inst, regRead[0].resp, regRead[1].resp);
          end
          else if(isStore(inst))
            dataReqQ.data := tagged Store tuple2(res, regRead[1].resp);
          else if(copRead(inst))
            wbQ.data := Wb{index: dest, data: tagged Valid (cop.read)};
          else if(copWrite(inst))
            cop.write := regRead[1].resp;
          else
            wbQ.data := Wb{index: dest, data: tagged Valid res};
        end
      end
    end
  endrule
endpartition

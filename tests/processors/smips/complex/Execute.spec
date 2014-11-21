include Library;
include Types;
include RegsFile;
include Cop;
include Fifo;
include ExecUtils;

port Execute;
  FifoDeq#(Pair#(VAddr, Bool)) pcQ;
  FifoDeq#(Inst) instQ;
  FifoEnq#(Mem) dataReqQ;
  RegFileRead#(NumRegs, Data)[2] regRead;
  FifoEnq#(Pair#(RegIndex, Maybe#(Data))) wbQ;
  ConditionalInput#(RegIndex) wbIndex;
  Input#(Bool) currEpoch;
  ConditionalOutput#(VAddr) branchPc;
  Reverse Cop cop;
endport

(* synthesize *)
partition Execute mkExecute;
  let inst = instQ.first;
  let reg1 = inst[25:21];
  let reg2 = inst[20:16];
  match tagged Pair{snd:.epoch, fst:.pcPlus4} = pcQ.first;

  atomic r1;
    regRead[0].req := reg1;
    regRead[1].req := reg2;
  endatomic

  atomic r2;
    if(epoch != currEpoch)
    begin
      pcQ.deq;
      instQ.deq;
    end
    else
    begin
      let stall = (isSrcValid(inst)[0] && wbIndex.en && wbIndex == reg1 || isSrcValid(inst)[1] && wbIndex.en && wbIndex == reg2);
      if(!stall)
      begin
        pcQ.deq;
        instQ.deq;
        let dest = getDest(inst);
        if(isBranch(inst))
        begin
          match {.taken, .branchTarget, .isLinked} = branch(inst, regRead[0].resp, regRead[1].resp, pcPlus4);
          if(taken)
            branchPc := branchTarget;
          if(isLinked)
            wbQ.enq := Pair{fst: dest, snd: tagged Valid pcPlus4};
        end
        else
        begin
          let res = aluDataAddr(inst, regRead[0].resp, regRead[1].resp);
          if(isLoad(inst))
          begin
            wbQ.enq := Pair{fst: dest, snd: tagged Invalid};
            dataReqQ.enq := tagged Load aluDataAddr(inst, regRead[0].resp, regRead[1].resp);
          end
          else if(isStore(inst))
            dataReqQ.enq := tagged Store (Pair{fst: res, snd: regRead[1].resp});
          else if(copRead(inst))
            wbQ.enq := Pair{fst: dest, snd: tagged Valid (cop.read)};
          else if(copWrite(inst))
            cop.write := Pair{fst: copReg(inst), snd: regRead[1].resp};
          else
            wbQ.enq := Pair{fst: dest, snd: tagged Valid res};
        end
      end
    end
  endatomic
endpartition

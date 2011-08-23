include Library;
include Types;
include RegFile;
include Fifo;
include ExecUtils;

port Execute;
  FifoDeq#(Pair#(Bool, VAddr)) pcQ;
  FifoDeq#(Inst) instQ;
  FifoEnq#(Mem) dataReqQ;
  RegFileRead#(NumRegs, Data)[2] regRead;
  FifoEnq#(Pair#(RegIndex, Maybe#(Data))) wbQ;
  ConditionalInput#(RegIndex) wbIndex;
  Input#(Bool) currEpoch;
  ConditionalOutput#(VAddr) branchPc;
endport

(* synthesize *)
partition Execute mkExecute;
  Reg#(VAddr)    lr <- mkRegU;
  Reg#(Bit#(4)) cr0 <- mkReg(0);

  atomic a;
    let epoch = pcQ.first.fst;
    if(epoch != currEpoch)
    begin
      pcQ.deq;
      instQ.deq;
    end
    else
    begin
      let inst = instQ.first;
      let srcsValid = isSrcsValid(inst);
      let srcs = getSrcsInst(inst);
      function Bool isStall;
        Bool ret = False;
        for(Integer i = 0; i < 2; i = i + 1)
          ret = ret || (srcsValid[i] && wbIndex.en && wbIndex == srcs[i]);
        return ret;
      endfunction

      if(!isStall)
      begin
        let pc = pcQ.first.snd;
        pcQ.deq;
        instQ.deq;
        let dest = getDest(inst);
        Vector#(2, Data) regVals = newVector;
        for(Integer i = 0; i < 2; i = i + 1)
        begin
          regRead[i].req := srcs[i];
          regVals[i] = regRead[i].resp;
        end

        if(isWriteLr(inst))
          lr <= pc + 4;

        if(isWriteCr0(inst))
          cr0 <= condWrite(inst, regVals[0]);
        else if(isBranch(inst))
        begin
          match {.taken, .branchTarget} = branch(inst, regVals[0], lr, cr0, pc);
          if(taken)
            branchPc := branchTarget;
        end
        else
        begin
          let res = aluResultMemAddr(inst, regVals, lr);
          if(isLoad(inst))
          begin
            wbQ.enq := Pair{fst: dest, snd: tagged Invalid};
            dataReqQ.enq := tagged Load res;
          end
          else if(isStore(inst))
            dataReqQ.enq := tagged Store (Pair{fst: res, snd: regVals[1]});
          else
            wbQ.enq := Pair{fst: dest, snd: tagged Valid res};
        end
      end
    end
  endatomic
endpartition

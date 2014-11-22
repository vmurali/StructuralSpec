include Fifo;
include BramFifo;
include Library;
include Types;
include Registers;
include Mem;
include ExecUtils;

(* synthesize *)
partition Empty mkProcessor;
  Reg#(VAddr) pc <- mkReg('h1000);
  Registers rf <- mkRegisters;
  Memory mem <- mkMemory;

  Fifo#(1, Tuple2#(VAddr, Inst)) ir <- mkBramLFifo;

  Wire#(VAddr) newPc <- mkWire;
  PulseWire brTaken <- mkPulseWire;

  atomic doFetch;
    if(brTaken)
      pc <= newPc;
    else if(ir.enq.notFull)
    begin
      mem.iReq := pc;
      let inst = mem.iResp;
      ir.enq.enq := tuple2(pc, inst);
      pc <= pc + 4;
    end
  endatomic

  atomic doExec;
    if(ir.deq.notEmpty)
    begin
      match {.pc, .inst} = ir.deq.first;

      rf.read[0].req := inst[25:21];
      rf.read[1].req := inst[20:16];
      let r0 = rf.read[0].resp;
      let r1 = rf.read[1].resp;
      let dest = getDest(inst);

      if(isBranch(inst))
      begin
        match {.taken, .branchTarget, .isLinked} = branch(inst, r0, r1, pc+4);
        if(taken)
        begin
          brTaken.send;
          newPc := branchTarget;
        end
        if(isLinked)
          rf.write := Pair{fst: dest, snd: pc+4};
      end
      else
      begin
        let res = aluDataAddr(inst, r0, r1);
        if(isLoad(inst))
        begin
          mem.dReq := tagged Load res;
          let data = mem.dResp;
          rf.write := Pair{fst: dest, snd: data};
        end
        else if(isStore(inst))
          mem.dReq := tagged Store (tuple2(res, r1));
        else if(copWrite(inst) && copReg(inst) == 21)
        begin
          if(r1 == 1)
            $display("Passed");
          else
            $display("Failed");
          $finish(0);
        end
        else
          rf.write := Pair{fst: dest, snd: res};
      end
      ir.deq.deq;
    end
  endatomic

endpartition

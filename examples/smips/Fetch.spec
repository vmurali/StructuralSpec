include Library;
include Types;

port Fetch;
  Output#(Bool) currEpoch;
  GuardedAction#(Tuple2#(VAddr, Bool)) pcQ;
  GuardedAction#(VAddr) instReqQ;
  Reverse OutputEn#(VAddr) branchPc;
endport

partition mkFetch implements Fetch;
  Reg#(VAddr)   pc <- mkReg(0);
  Reg#(Bool) epoch <- mkRegU;

  rule r1;
    if(instReqQ.rdy && pcQ.rdy)
      instReqQ.data := pc;
    else
      instReqQ.data.justFinish;
  endrule

  rule r2;
    if(instReqQ.rdy && pcQ.rdy)
      pcQ.data := tuple2(pc + 4, epoch);
    else
      pcQ.data.justFinish;
  endrule

  rule r3;
    currEpoch := epoch;
  endrule

  rule r4;
    if(branchPc.en)
    begin
      pc <= branchPc;
      epoch <= !epoch;
    end
    else
      pc <= pc + 4;
  endrule

  rule r5;
    specCycleDone;
  endrule
endpartition

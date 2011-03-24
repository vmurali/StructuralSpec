include Library;
include Types;

port Fetch;
  Output#(Bool) currEpoch;
  ActionCall#(Tuple2#(VAddr, Bool)) pcQ;
  ActionCall#(VAddr) instReqQ;
  Reverse OutputEn#(VAddr) branchPc;
endport

partition mkFetch implements Fetch;
  Reg#(VAddr)   pc <- mkReg(0);
  Reg#(Bool) epoch <- mkRegU;

  rule r1;
    instReqQ := pc;
    pcQ := tuple2(pc + 4, epoch);
  endrule

  rule r2;
    if(branchPc.en)
    begin
      pc <= branchPc;
      epoch <= !epoch;
    end
    else
      pc <= pc + 4;
    currEpoch := epoch;

    specCycleDone;
  endrule
endpartition

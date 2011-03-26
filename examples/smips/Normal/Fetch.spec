include Library;
include Types;
include FetchPort;

(* synthesize *)
partition mkFetch implements Fetch;
  Reg#(VAddr)   pc <- mkReg(0);
  Reg#(Bool) epoch <- mkRegU;

  rule r1;
    instReqQ.data := pc;
    pcQ.data := tuple2(pc + 4, epoch);
  endrule

  rule r2;
    currEpoch := epoch;

    if(branchPc.en)
    begin
      pc <= branchPc;
      epoch <= !epoch;
    end
    else
      pc <= pc + 4;
  endrule
endpartition

include Library;
include Types;
include FetchPort;

(* synthesize *)
partition mkFetch implements Fetch;
  Reg#(VAddr)   pc <- mkReg('h1000);
  Reg#(Bool) epoch <- mkRegU;

  Pulse      fired <- mkPulse;

  rule r1;
    instReqQ.data := pc;
    pcQ.data := tuple2(pc + 4, epoch);

    fired.send;
  endrule

  rule r2;
    currEpoch := epoch;

    if(branchPc.en)
    begin
      pc <= branchPc;
      epoch <= !epoch;
    end
    else if(fired)
      pc <= pc + 4;
  endrule
endpartition

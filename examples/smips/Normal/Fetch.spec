include Library;
include Types;
include FetchPort;

(* synthesize *)
partition mkFetch implements Fetch;
  Reg#(VAddr)   pc <- mkReg('h1000);
  Reg#(Bool) epoch <- mkRegU;

  Pulse      fired <- mkPulse;

  atomic r1;
    instReqQ.data := pc;
    pcQ.data := tuple2(pc + 4, epoch);

    fired.send;
  endatomic

  atomic r2;
    currEpoch := epoch;

    if(branchPc.en)
    begin
      pc <= branchPc;
      epoch <= !epoch;
    end
    else if(fired)
      pc <= pc + 4;
  endatomic
endpartition

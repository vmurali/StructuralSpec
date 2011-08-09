include Library;
include Types;

port Fetch;
  Output#(Bool) currEpoch;
  FifoEnq#(PcQ) pcQ;
  FifoEnq#(VAddr) instReqQ;
  ConditionalInput#(VAddr) branchPc;
endport

(* synthesize *)
partition Fetch mkFetch;
  Reg#(VAddr)   pc <- mkReg('h1000);
  Reg#(Bool) epoch <- mkRegU;

  Pulse      fired <- mkPulse;

  atomic r1;
    instReqQ.enq := pc;
    pcQ.enq := PcQ{pc: pc+4, epoch: epoch};

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
    begin
      pc <= pc + 4;
    end
  endatomic
endpartition

include Library;
include Types;
include Fifo;

port Fetch;
  Output#(Bool) currEpoch;
  FifoEnq#(Pair#(VAddr, Bool)) pcQ;
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
    pcQ.enq := Pair{fst: pc+4, snd: epoch};

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

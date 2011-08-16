include Library;
include Types;
include FifoNormal;

port Fetch;
  OutputNormal#(Bool) currEpoch;
  FifoEnqNormal#(PcQ) pcQ;
  FifoEnqNormal#(VAddr) instReqQ;
  ConditionalInputNormal#(VAddr) branchPc;
endport

(* synthesize *)
partition Fetch mkFetch;
  RegNormal#(VAddr)   pc <- mkRegNormal('h1000);
  RegNormal#(Bool) epoch <- mkRegUNormal;

  PulseNormal      fired <- mkPulseNormal;

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

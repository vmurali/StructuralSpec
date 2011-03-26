include Library;
include Types;

port Fetch;
  Output#(Bool) currEpoch;
  GuardedAction#(Tuple2#(VAddr, Bool)) pcQ;
  GuardedAction#(VAddr) instReqQ;
  Reverse OutputEn#(VAddr) branchPc;
endport

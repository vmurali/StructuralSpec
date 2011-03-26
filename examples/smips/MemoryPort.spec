include Library;
include Fifo;
include Types;

port Memory;
  FifoDeq#(VAddr) instReqQ;
  GuardedAction#(Inst) instQ;
  FifoDeq#(Tuple2#(VAddr, Data)) dataReqQ;
  GuardedAction#(Inst) dataQ;
endport

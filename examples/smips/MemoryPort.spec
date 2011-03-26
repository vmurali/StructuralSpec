include Library;
include Fifo;
include Types;

port Memory;
  FifoDeq#(VAddr) instReqQ;
  GuardedAction#(Inst) instQ;
  FifoDeq#(Mem) dataReqQ;
  GuardedAction#(Inst) dataQ;
endport

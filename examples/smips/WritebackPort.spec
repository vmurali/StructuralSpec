include Library;
include Types;
include Fifo;
include RegFile;

port Writeback;
  Reverse GuardedAction#(Wb) wb;
  OutputEn#(RegIndex) wbIndex;
  FifoDeq#(Data) dataQ;
  OutputEn#(Tuple2#(RegIndex, Data)) regWrite;
endport

include Library;
include Types;
include RegFile;

port Registers;
  Reverse RegRead#(RegIndexSz, Data)[2] read;
  Reverse OutputEn#(Tuple2#(RegIndex, Data)) write;
endport

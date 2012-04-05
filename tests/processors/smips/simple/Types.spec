typedef Bit#(32) VAddr;
typedef Bit#(32) Inst;
typedef Bit#(32) Data;
typedef 32 NumRegs;
typedef TLog#(NumRegs) RegIndexSz;
typedef Bit#(RegIndexSz) RegIndex;
typedef Int#(32) SData;

typedef union tagged {
  VAddr Load;
  Tuple2#(VAddr, Data) Store;
} Mem deriving (Bits, Eq);

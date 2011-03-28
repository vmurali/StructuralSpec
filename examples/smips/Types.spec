typedef Bit#(32) VAddr;
typedef Bit#(32) Inst;
typedef Bit#(32) Data;
typedef 5 RegIndexSz;
typedef Bit#(RegIndexSz) RegIndex;
typedef Int#(32) SData;

typedef struct {
  RegIndex index;
  Maybe#(Data) data;
} Wb deriving (Bits, Eq);

typedef union tagged {
  VAddr Load;
  Tuple2#(VAddr, Data) Store;
} Mem deriving (Bits, Eq);

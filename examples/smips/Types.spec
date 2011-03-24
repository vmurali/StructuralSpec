typedef Bit#(32) VAddr;
typedef Bit#(32) Inst;
typedef Bit#(32) Data;
typedef Bit#(5) RegIndex;
typedef Int#(32) SData;

typedef struct {
  RegIndex index;
  Maybe#(Data) data;
} Wb deriving (Bits, Eq);

typedef union tagged {
  Data Load;
  Tuple2#(VAddr, Data) Store;
} Mem deriving (Bits, Eq);

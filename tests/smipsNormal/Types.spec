include RegFileNormal;

typedef Bit#(32) VAddr;
typedef Bit#(32) Inst;
typedef Bit#(32) Data;
typedef 32 NumRegs;
typedef TLog#(NumRegs) RegIndexSz;
typedef Bit#(RegIndexSz) RegIndex;
typedef Int#(32) SData;

typedef struct {
  RegIndex index;
  Maybe#(Data) data;
} Wb deriving (Bits, Eq);

typedef union tagged {
  VAddr Load;
  MemWrite Store;
} Mem deriving (Bits, Eq);

typedef struct {
  VAddr pc;
  Bool epoch;
} PcQ deriving (Bits, Eq);

typedef RegFileWriteNormal#(NumRegs, Data) RegWrite;
typedef RegFileWriteNormal#(TExp#(32), Data) MemWrite;

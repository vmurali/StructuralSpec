import Vector::*;
import HaskellLib::*;
import Connectable::*;
import Base::*;
import Primitive::*;
export Types::*;

typedef Bit#(32) VAddr;
typedef Bit#(32) Inst;
typedef Bit#(32) Data;
typedef 32 NumRegs;
typedef TLog#(NumRegs) RegIndexSz;
typedef Bit#(RegIndexSz) RegIndex;
typedef Int#(32) SData;

typedef union tagged {
  VAddr Load;
  Pair#(VAddr, Data) Store;
} Mem deriving (Bits, Eq);


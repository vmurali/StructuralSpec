import Vector::*;
import HaskellLib::*;
import Connectable::*;
import Base::*;
import Primitive::*;

import Library::*;
import Types::*;
import RegFile::*;
interface Registers_;
  interface Vector#(2, RegRead#(RegIndexSz, Data)) read;
  interface OutputEn#(Tuple2#(RegIndex, Data)) write;
  method Action specCycleInputDone();
  method Action specCycleOutputDone();
  method Bool isSupplied();
endinterface

interface Registers;
  interface Vector#(2, RegRead_#(RegIndexSz, Data)) read;
  interface OutputEn_#(Tuple2#(RegIndex, Data)) write;
  method Action specCycleInputDone();
  method Action specCycleOutputDone();
  method Bool isSupplied();
endinterface

module _Registers(Tuple2#(Registers_, Registers)) ;
  Tuple2#(Vector#(2, RegRead_#(RegIndexSz, Data)), Vector#(2, RegRead#(RegIndexSz, Data))) read__ <- replicateTupleM(_RegRead);
  Tuple2#(Vector#(2, RegRead#(RegIndexSz, Data)), Vector#(2, RegRead_#(RegIndexSz, Data))) read_ = tuple2(tpl_2(asIfc(read__)), tpl_1(asIfc(read__)));
  Tuple2#(OutputEn_#(Tuple2#(RegIndex, Data)), OutputEn#(Tuple2#(RegIndex, Data))) write__ <- _OutputEn;
  Tuple2#(OutputEn#(Tuple2#(RegIndex, Data)), OutputEn_#(Tuple2#(RegIndex, Data))) write_ = tuple2(tpl_2(asIfc(write__)), tpl_1(asIfc(write__)));
  return tuple2(
    interface Registers_;
      interface read = tpl_1(asIfc(read_));
      interface write = tpl_1(asIfc(write_));
      method Action specCycleInputDone();
        _specCycleInputDone(tpl_1(asIfc(read_)));
        _specCycleInputDone(tpl_1(asIfc(write_)));
      endmethod
      method Action specCycleOutputDone();
        _specCycleOutputDone(tpl_1(asIfc(read_)));
        _specCycleOutputDone(tpl_1(asIfc(write_)));
      endmethod
      method Bool isSupplied = True  && _isSupplied(tpl_1(asIfc(read_))) && _isSupplied(tpl_1(asIfc(write_)));
    endinterface,
    interface Registers;
      interface read = tpl_2(asIfc(read_));
      interface write = tpl_2(asIfc(write_));
      method Action specCycleInputDone();
        _specCycleInputDone(tpl_2(asIfc(read_)));
        _specCycleInputDone(tpl_2(asIfc(write_)));
      endmethod
      method Action specCycleOutputDone();
        _specCycleOutputDone(tpl_2(asIfc(read_)));
        _specCycleOutputDone(tpl_2(asIfc(write_)));
      endmethod
      method Bool isSupplied = True  && _isSupplied(tpl_2(asIfc(read_))) && _isSupplied(tpl_2(asIfc(write_)));
    endinterface);
endmodule

instance Connectable#(Registers, Registers_) ;
  module mkConnection#(Registers a, Registers_ b)();
    mkConnection(asIfc(a.read), asIfc(b.read));
    mkConnection(asIfc(a.write), asIfc(b.write));
  endmodule
endinstance

instance Connectable#(Registers_, Registers) ;
  module mkConnection#(Registers_ a, Registers b)();
    mkConnection(asIfc(a.read), asIfc(b.read));
    mkConnection(asIfc(a.write), asIfc(b.write));
  endmodule
endinstance

instance Sync_#(Registers);
  function Action _specCycleInputDone(Registers x) = x.specCycleInputDone;
  function Action _specCycleOutputDone(Registers x) = x.specCycleOutputDone;
  function Bool _isSupplied(Registers x) = x.isSupplied;
endinstance

instance Sync_#(Registers_);
  function Action _specCycleInputDone(Registers_ x) = x.specCycleInputDone;
  function Action _specCycleOutputDone(Registers_ x) = x.specCycleOutputDone;
  function Bool _isSupplied(Registers_ x) = x.isSupplied;
endinstance


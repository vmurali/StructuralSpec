import Vector::*;
import HaskellLib::*;
import Connectable::*;
import Base::*;
import Primitive::*;

import Library::*;
import Types::*;
import Fifo::*;
import RegFile::*;
interface Writeback_;
  interface GuardedAction#(Wb) wb;
  interface OutputEn_#(RegIndex) wbIndex;
  interface FifoDeq_#(Data) dataQ;
  interface OutputEn_#(Tuple2#(RegIndex, Data)) regWrite;
  method Action specCycleInputDone();
  method Action specCycleOutputDone();
  method Bool isSupplied();
endinterface

interface Writeback;
  interface GuardedAction_#(Wb) wb;
  interface OutputEn#(RegIndex) wbIndex;
  interface FifoDeq#(Data) dataQ;
  interface OutputEn#(Tuple2#(RegIndex, Data)) regWrite;
  method Action specCycleInputDone();
  method Action specCycleOutputDone();
  method Bool isSupplied();
endinterface

module _Writeback(Tuple2#(Writeback_, Writeback)) ;
  Tuple2#(GuardedAction_#(Wb), GuardedAction#(Wb)) wb__ <- _GuardedAction;
  Tuple2#(GuardedAction#(Wb), GuardedAction_#(Wb)) wb_ = tuple2(tpl_2(asIfc(wb__)), tpl_1(asIfc(wb__)));
  Tuple2#(OutputEn_#(RegIndex), OutputEn#(RegIndex)) wbIndex_ <- _OutputEn;
  Tuple2#(FifoDeq_#(Data), FifoDeq#(Data)) dataQ_ <- _FifoDeq;
  Tuple2#(OutputEn_#(Tuple2#(RegIndex, Data)), OutputEn#(Tuple2#(RegIndex, Data))) regWrite_ <- _OutputEn;
  return tuple2(
    interface Writeback_;
      interface wb = tpl_1(asIfc(wb_));
      interface wbIndex = tpl_1(asIfc(wbIndex_));
      interface dataQ = tpl_1(asIfc(dataQ_));
      interface regWrite = tpl_1(asIfc(regWrite_));
      method Action specCycleInputDone();
        _specCycleInputDone(tpl_1(asIfc(wb_)));
        _specCycleInputDone(tpl_1(asIfc(wbIndex_)));
        _specCycleInputDone(tpl_1(asIfc(dataQ_)));
        _specCycleInputDone(tpl_1(asIfc(regWrite_)));
      endmethod
      method Action specCycleOutputDone();
        _specCycleOutputDone(tpl_1(asIfc(wb_)));
        _specCycleOutputDone(tpl_1(asIfc(wbIndex_)));
        _specCycleOutputDone(tpl_1(asIfc(dataQ_)));
        _specCycleOutputDone(tpl_1(asIfc(regWrite_)));
      endmethod
      method Bool isSupplied = True  && _isSupplied(tpl_1(asIfc(wb_))) && _isSupplied(tpl_1(asIfc(wbIndex_))) && _isSupplied(tpl_1(asIfc(dataQ_))) && _isSupplied(tpl_1(asIfc(regWrite_)));
    endinterface,
    interface Writeback;
      interface wb = tpl_2(asIfc(wb_));
      interface wbIndex = tpl_2(asIfc(wbIndex_));
      interface dataQ = tpl_2(asIfc(dataQ_));
      interface regWrite = tpl_2(asIfc(regWrite_));
      method Action specCycleInputDone();
        _specCycleInputDone(tpl_2(asIfc(wb_)));
        _specCycleInputDone(tpl_2(asIfc(wbIndex_)));
        _specCycleInputDone(tpl_2(asIfc(dataQ_)));
        _specCycleInputDone(tpl_2(asIfc(regWrite_)));
      endmethod
      method Action specCycleOutputDone();
        _specCycleOutputDone(tpl_2(asIfc(wb_)));
        _specCycleOutputDone(tpl_2(asIfc(wbIndex_)));
        _specCycleOutputDone(tpl_2(asIfc(dataQ_)));
        _specCycleOutputDone(tpl_2(asIfc(regWrite_)));
      endmethod
      method Bool isSupplied = True  && _isSupplied(tpl_2(asIfc(wb_))) && _isSupplied(tpl_2(asIfc(wbIndex_))) && _isSupplied(tpl_2(asIfc(dataQ_))) && _isSupplied(tpl_2(asIfc(regWrite_)));
    endinterface);
endmodule

instance Connectable#(Writeback, Writeback_) ;
  module mkConnection#(Writeback a, Writeback_ b)();
    mkConnection(asIfc(a.wb), asIfc(b.wb));
    mkConnection(asIfc(a.wbIndex), asIfc(b.wbIndex));
    mkConnection(asIfc(a.dataQ), asIfc(b.dataQ));
    mkConnection(asIfc(a.regWrite), asIfc(b.regWrite));
  endmodule
endinstance

instance Connectable#(Writeback_, Writeback) ;
  module mkConnection#(Writeback_ a, Writeback b)();
    mkConnection(asIfc(a.wb), asIfc(b.wb));
    mkConnection(asIfc(a.wbIndex), asIfc(b.wbIndex));
    mkConnection(asIfc(a.dataQ), asIfc(b.dataQ));
    mkConnection(asIfc(a.regWrite), asIfc(b.regWrite));
  endmodule
endinstance

instance Sync_#(Writeback);
  function Action _specCycleInputDone(Writeback x) = x.specCycleInputDone;
  function Action _specCycleOutputDone(Writeback x) = x.specCycleOutputDone;
  function Bool _isSupplied(Writeback x) = x.isSupplied;
endinstance

instance Sync_#(Writeback_);
  function Action _specCycleInputDone(Writeback_ x) = x.specCycleInputDone;
  function Action _specCycleOutputDone(Writeback_ x) = x.specCycleOutputDone;
  function Bool _isSupplied(Writeback_ x) = x.isSupplied;
endinstance


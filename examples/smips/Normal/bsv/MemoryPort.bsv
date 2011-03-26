import Vector::*;
import HaskellLib::*;
import Connectable::*;
import Base::*;
import Primitive::*;

import Library::*;
import Fifo::*;
import Types::*;
interface Memory_;
  interface FifoDeq_#(VAddr) instReqQ;
  interface GuardedAction_#(Inst) instQ;
  interface FifoDeq_#(Tuple2#(VAddr, Data)) dataReqQ;
  interface GuardedAction_#(Inst) dataQ;
  method Action specCycleInputDone();
  method Action specCycleOutputDone();
  method Bool isSupplied();
endinterface

interface Memory;
  interface FifoDeq#(VAddr) instReqQ;
  interface GuardedAction#(Inst) instQ;
  interface FifoDeq#(Tuple2#(VAddr, Data)) dataReqQ;
  interface GuardedAction#(Inst) dataQ;
  method Action specCycleInputDone();
  method Action specCycleOutputDone();
  method Bool isSupplied();
endinterface

module _Memory(Tuple2#(Memory_, Memory)) ;
  Tuple2#(FifoDeq_#(VAddr), FifoDeq#(VAddr)) instReqQ_ <- _FifoDeq;
  Tuple2#(GuardedAction_#(Inst), GuardedAction#(Inst)) instQ_ <- _GuardedAction;
  Tuple2#(FifoDeq_#(Tuple2#(VAddr, Data)), FifoDeq#(Tuple2#(VAddr, Data))) dataReqQ_ <- _FifoDeq;
  Tuple2#(GuardedAction_#(Inst), GuardedAction#(Inst)) dataQ_ <- _GuardedAction;
  return tuple2(
    interface Memory_;
      interface instReqQ = tpl_1(asIfc(instReqQ_));
      interface instQ = tpl_1(asIfc(instQ_));
      interface dataReqQ = tpl_1(asIfc(dataReqQ_));
      interface dataQ = tpl_1(asIfc(dataQ_));
      method Action specCycleInputDone();
        _specCycleInputDone(tpl_1(asIfc(instReqQ_)));
        _specCycleInputDone(tpl_1(asIfc(instQ_)));
        _specCycleInputDone(tpl_1(asIfc(dataReqQ_)));
        _specCycleInputDone(tpl_1(asIfc(dataQ_)));
      endmethod
      method Action specCycleOutputDone();
        _specCycleOutputDone(tpl_1(asIfc(instReqQ_)));
        _specCycleOutputDone(tpl_1(asIfc(instQ_)));
        _specCycleOutputDone(tpl_1(asIfc(dataReqQ_)));
        _specCycleOutputDone(tpl_1(asIfc(dataQ_)));
      endmethod
      method Bool isSupplied = True  && _isSupplied(tpl_1(asIfc(instReqQ_))) && _isSupplied(tpl_1(asIfc(instQ_))) && _isSupplied(tpl_1(asIfc(dataReqQ_))) && _isSupplied(tpl_1(asIfc(dataQ_)));
    endinterface,
    interface Memory;
      interface instReqQ = tpl_2(asIfc(instReqQ_));
      interface instQ = tpl_2(asIfc(instQ_));
      interface dataReqQ = tpl_2(asIfc(dataReqQ_));
      interface dataQ = tpl_2(asIfc(dataQ_));
      method Action specCycleInputDone();
        _specCycleInputDone(tpl_2(asIfc(instReqQ_)));
        _specCycleInputDone(tpl_2(asIfc(instQ_)));
        _specCycleInputDone(tpl_2(asIfc(dataReqQ_)));
        _specCycleInputDone(tpl_2(asIfc(dataQ_)));
      endmethod
      method Action specCycleOutputDone();
        _specCycleOutputDone(tpl_2(asIfc(instReqQ_)));
        _specCycleOutputDone(tpl_2(asIfc(instQ_)));
        _specCycleOutputDone(tpl_2(asIfc(dataReqQ_)));
        _specCycleOutputDone(tpl_2(asIfc(dataQ_)));
      endmethod
      method Bool isSupplied = True  && _isSupplied(tpl_2(asIfc(instReqQ_))) && _isSupplied(tpl_2(asIfc(instQ_))) && _isSupplied(tpl_2(asIfc(dataReqQ_))) && _isSupplied(tpl_2(asIfc(dataQ_)));
    endinterface);
endmodule

instance Connectable#(Memory, Memory_) ;
  module mkConnection#(Memory a, Memory_ b)();
    mkConnection(asIfc(a.instReqQ), asIfc(b.instReqQ));
    mkConnection(asIfc(a.instQ), asIfc(b.instQ));
    mkConnection(asIfc(a.dataReqQ), asIfc(b.dataReqQ));
    mkConnection(asIfc(a.dataQ), asIfc(b.dataQ));
  endmodule
endinstance

instance Connectable#(Memory_, Memory) ;
  module mkConnection#(Memory_ a, Memory b)();
    mkConnection(asIfc(a.instReqQ), asIfc(b.instReqQ));
    mkConnection(asIfc(a.instQ), asIfc(b.instQ));
    mkConnection(asIfc(a.dataReqQ), asIfc(b.dataReqQ));
    mkConnection(asIfc(a.dataQ), asIfc(b.dataQ));
  endmodule
endinstance

instance Sync_#(Memory);
  function Action _specCycleInputDone(Memory x) = x.specCycleInputDone;
  function Action _specCycleOutputDone(Memory x) = x.specCycleOutputDone;
  function Bool _isSupplied(Memory x) = x.isSupplied;
endinstance

instance Sync_#(Memory_);
  function Action _specCycleInputDone(Memory_ x) = x.specCycleInputDone;
  function Action _specCycleOutputDone(Memory_ x) = x.specCycleOutputDone;
  function Bool _isSupplied(Memory_ x) = x.isSupplied;
endinstance


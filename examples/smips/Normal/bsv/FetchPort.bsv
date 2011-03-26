import Vector::*;
import HaskellLib::*;
import Connectable::*;
import Base::*;
import Primitive::*;

import Library::*;
import Types::*;
interface Fetch_;
  interface Output_#(Bool) currEpoch;
  interface GuardedAction_#(Tuple2#(VAddr, Bool)) pcQ;
  interface GuardedAction_#(VAddr) instReqQ;
  interface OutputEn#(VAddr) branchPc;
  method Action specCycleInputDone();
  method Action specCycleOutputDone();
  method Bool isSupplied();
endinterface

interface Fetch;
  interface Output#(Bool) currEpoch;
  interface GuardedAction#(Tuple2#(VAddr, Bool)) pcQ;
  interface GuardedAction#(VAddr) instReqQ;
  interface OutputEn_#(VAddr) branchPc;
  method Action specCycleInputDone();
  method Action specCycleOutputDone();
  method Bool isSupplied();
endinterface

module _Fetch(Tuple2#(Fetch_, Fetch)) ;
  Tuple2#(Output_#(Bool), Output#(Bool)) currEpoch_ <- _Output(False, ?, True, True);
  Tuple2#(GuardedAction_#(Tuple2#(VAddr, Bool)), GuardedAction#(Tuple2#(VAddr, Bool))) pcQ_ <- _GuardedAction;
  Tuple2#(GuardedAction_#(VAddr), GuardedAction#(VAddr)) instReqQ_ <- _GuardedAction;
  Tuple2#(OutputEn_#(VAddr), OutputEn#(VAddr)) branchPc__ <- _OutputEn;
  Tuple2#(OutputEn#(VAddr), OutputEn_#(VAddr)) branchPc_ = tuple2(tpl_2(asIfc(branchPc__)), tpl_1(asIfc(branchPc__)));
  return tuple2(
    interface Fetch_;
      interface currEpoch = tpl_1(asIfc(currEpoch_));
      interface pcQ = tpl_1(asIfc(pcQ_));
      interface instReqQ = tpl_1(asIfc(instReqQ_));
      interface branchPc = tpl_1(asIfc(branchPc_));
      method Action specCycleInputDone();
        _specCycleInputDone(tpl_1(asIfc(currEpoch_)));
        _specCycleInputDone(tpl_1(asIfc(pcQ_)));
        _specCycleInputDone(tpl_1(asIfc(instReqQ_)));
        _specCycleInputDone(tpl_1(asIfc(branchPc_)));
      endmethod
      method Action specCycleOutputDone();
        _specCycleOutputDone(tpl_1(asIfc(currEpoch_)));
        _specCycleOutputDone(tpl_1(asIfc(pcQ_)));
        _specCycleOutputDone(tpl_1(asIfc(instReqQ_)));
        _specCycleOutputDone(tpl_1(asIfc(branchPc_)));
      endmethod
      method Bool isSupplied = True  && _isSupplied(tpl_1(asIfc(currEpoch_))) && _isSupplied(tpl_1(asIfc(pcQ_))) && _isSupplied(tpl_1(asIfc(instReqQ_))) && _isSupplied(tpl_1(asIfc(branchPc_)));
    endinterface,
    interface Fetch;
      interface currEpoch = tpl_2(asIfc(currEpoch_));
      interface pcQ = tpl_2(asIfc(pcQ_));
      interface instReqQ = tpl_2(asIfc(instReqQ_));
      interface branchPc = tpl_2(asIfc(branchPc_));
      method Action specCycleInputDone();
        _specCycleInputDone(tpl_2(asIfc(currEpoch_)));
        _specCycleInputDone(tpl_2(asIfc(pcQ_)));
        _specCycleInputDone(tpl_2(asIfc(instReqQ_)));
        _specCycleInputDone(tpl_2(asIfc(branchPc_)));
      endmethod
      method Action specCycleOutputDone();
        _specCycleOutputDone(tpl_2(asIfc(currEpoch_)));
        _specCycleOutputDone(tpl_2(asIfc(pcQ_)));
        _specCycleOutputDone(tpl_2(asIfc(instReqQ_)));
        _specCycleOutputDone(tpl_2(asIfc(branchPc_)));
      endmethod
      method Bool isSupplied = True  && _isSupplied(tpl_2(asIfc(currEpoch_))) && _isSupplied(tpl_2(asIfc(pcQ_))) && _isSupplied(tpl_2(asIfc(instReqQ_))) && _isSupplied(tpl_2(asIfc(branchPc_)));
    endinterface);
endmodule

instance Connectable#(Fetch, Fetch_) ;
  module mkConnection#(Fetch a, Fetch_ b)();
    mkConnection(asIfc(a.currEpoch), asIfc(b.currEpoch));
    mkConnection(asIfc(a.pcQ), asIfc(b.pcQ));
    mkConnection(asIfc(a.instReqQ), asIfc(b.instReqQ));
    mkConnection(asIfc(a.branchPc), asIfc(b.branchPc));
  endmodule
endinstance

instance Connectable#(Fetch_, Fetch) ;
  module mkConnection#(Fetch_ a, Fetch b)();
    mkConnection(asIfc(a.currEpoch), asIfc(b.currEpoch));
    mkConnection(asIfc(a.pcQ), asIfc(b.pcQ));
    mkConnection(asIfc(a.instReqQ), asIfc(b.instReqQ));
    mkConnection(asIfc(a.branchPc), asIfc(b.branchPc));
  endmodule
endinstance

instance Sync_#(Fetch);
  function Action _specCycleInputDone(Fetch x) = x.specCycleInputDone;
  function Action _specCycleOutputDone(Fetch x) = x.specCycleOutputDone;
  function Bool _isSupplied(Fetch x) = x.isSupplied;
endinstance

instance Sync_#(Fetch_);
  function Action _specCycleInputDone(Fetch_ x) = x.specCycleInputDone;
  function Action _specCycleOutputDone(Fetch_ x) = x.specCycleOutputDone;
  function Bool _isSupplied(Fetch_ x) = x.isSupplied;
endinstance


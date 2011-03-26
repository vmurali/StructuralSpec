import Vector::*;
import HaskellLib::*;
import Connectable::*;
import Base::*;
import Primitive::*;

import Library::*;
import Types::*;
interface Cop_;
  interface OutputEn_#(Data) write;
  interface Output#(Data) read;
  method Action specCycleInputDone();
  method Action specCycleOutputDone();
  method Bool isSupplied();
endinterface

interface Cop;
  interface OutputEn#(Data) write;
  interface Output_#(Data) read;
  method Action specCycleInputDone();
  method Action specCycleOutputDone();
  method Bool isSupplied();
endinterface

module _Cop(Tuple2#(Cop_, Cop)) ;
  Tuple2#(OutputEn_#(Data), OutputEn#(Data)) write_ <- _OutputEn;
  Tuple2#(Output_#(Data), Output#(Data)) read__ <- _Output(False, ?, True, True);
  Tuple2#(Output#(Data), Output_#(Data)) read_ = tuple2(tpl_2(asIfc(read__)), tpl_1(asIfc(read__)));
  return tuple2(
    interface Cop_;
      interface write = tpl_1(asIfc(write_));
      interface read = tpl_1(asIfc(read_));
      method Action specCycleInputDone();
        _specCycleInputDone(tpl_1(asIfc(write_)));
        _specCycleInputDone(tpl_1(asIfc(read_)));
      endmethod
      method Action specCycleOutputDone();
        _specCycleOutputDone(tpl_1(asIfc(write_)));
        _specCycleOutputDone(tpl_1(asIfc(read_)));
      endmethod
      method Bool isSupplied = True  && _isSupplied(tpl_1(asIfc(write_))) && _isSupplied(tpl_1(asIfc(read_)));
    endinterface,
    interface Cop;
      interface write = tpl_2(asIfc(write_));
      interface read = tpl_2(asIfc(read_));
      method Action specCycleInputDone();
        _specCycleInputDone(tpl_2(asIfc(write_)));
        _specCycleInputDone(tpl_2(asIfc(read_)));
      endmethod
      method Action specCycleOutputDone();
        _specCycleOutputDone(tpl_2(asIfc(write_)));
        _specCycleOutputDone(tpl_2(asIfc(read_)));
      endmethod
      method Bool isSupplied = True  && _isSupplied(tpl_2(asIfc(write_))) && _isSupplied(tpl_2(asIfc(read_)));
    endinterface);
endmodule

instance Connectable#(Cop, Cop_) ;
  module mkConnection#(Cop a, Cop_ b)();
    mkConnection(asIfc(a.write), asIfc(b.write));
    mkConnection(asIfc(a.read), asIfc(b.read));
  endmodule
endinstance

instance Connectable#(Cop_, Cop) ;
  module mkConnection#(Cop_ a, Cop b)();
    mkConnection(asIfc(a.write), asIfc(b.write));
    mkConnection(asIfc(a.read), asIfc(b.read));
  endmodule
endinstance

instance Sync_#(Cop);
  function Action _specCycleInputDone(Cop x) = x.specCycleInputDone;
  function Action _specCycleOutputDone(Cop x) = x.specCycleOutputDone;
  function Bool _isSupplied(Cop x) = x.isSupplied;
endinstance

instance Sync_#(Cop_);
  function Action _specCycleInputDone(Cop_ x) = x.specCycleInputDone;
  function Action _specCycleOutputDone(Cop_ x) = x.specCycleOutputDone;
  function Bool _isSupplied(Cop_ x) = x.isSupplied;
endinstance

interface RevCop_;
  interface OutputEn#(Data) write;
  interface Output_#(Data) read;
  method Action specCycleInputDone();
  method Action specCycleOutputDone();
  method Bool isSupplied();
endinterface

interface RevCop;
  interface OutputEn_#(Data) write;
  interface Output#(Data) read;
  method Action specCycleInputDone();
  method Action specCycleOutputDone();
  method Bool isSupplied();
endinterface

module _RevCop(Tuple2#(RevCop_, RevCop)) ;
  Tuple2#(OutputEn_#(Data), OutputEn#(Data)) write__ <- _OutputEn;
  Tuple2#(OutputEn#(Data), OutputEn_#(Data)) write_ = tuple2(tpl_2(asIfc(write__)), tpl_1(asIfc(write__)));
  Tuple2#(Output_#(Data), Output#(Data)) read_ <- _Output(False, ?, True, True);
  return tuple2(
    interface RevCop_;
      interface write = tpl_1(asIfc(write_));
      interface read = tpl_1(asIfc(read_));
      method Action specCycleInputDone();
        _specCycleInputDone(tpl_1(asIfc(write_)));
        _specCycleInputDone(tpl_1(asIfc(read_)));
      endmethod
      method Action specCycleOutputDone();
        _specCycleOutputDone(tpl_1(asIfc(write_)));
        _specCycleOutputDone(tpl_1(asIfc(read_)));
      endmethod
      method Bool isSupplied = True  && _isSupplied(tpl_1(asIfc(write_))) && _isSupplied(tpl_1(asIfc(read_)));
    endinterface,
    interface RevCop;
      interface write = tpl_2(asIfc(write_));
      interface read = tpl_2(asIfc(read_));
      method Action specCycleInputDone();
        _specCycleInputDone(tpl_2(asIfc(write_)));
        _specCycleInputDone(tpl_2(asIfc(read_)));
      endmethod
      method Action specCycleOutputDone();
        _specCycleOutputDone(tpl_2(asIfc(write_)));
        _specCycleOutputDone(tpl_2(asIfc(read_)));
      endmethod
      method Bool isSupplied = True  && _isSupplied(tpl_2(asIfc(write_))) && _isSupplied(tpl_2(asIfc(read_)));
    endinterface);
endmodule

instance Connectable#(RevCop, RevCop_) ;
  module mkConnection#(RevCop a, RevCop_ b)();
    mkConnection(asIfc(a.write), asIfc(b.write));
    mkConnection(asIfc(a.read), asIfc(b.read));
  endmodule
endinstance

instance Connectable#(RevCop_, RevCop) ;
  module mkConnection#(RevCop_ a, RevCop b)();
    mkConnection(asIfc(a.write), asIfc(b.write));
    mkConnection(asIfc(a.read), asIfc(b.read));
  endmodule
endinstance

instance Sync_#(RevCop);
  function Action _specCycleInputDone(RevCop x) = x.specCycleInputDone;
  function Action _specCycleOutputDone(RevCop x) = x.specCycleOutputDone;
  function Bool _isSupplied(RevCop x) = x.isSupplied;
endinstance

instance Sync_#(RevCop_);
  function Action _specCycleInputDone(RevCop_ x) = x.specCycleInputDone;
  function Action _specCycleOutputDone(RevCop_ x) = x.specCycleOutputDone;
  function Bool _isSupplied(RevCop_ x) = x.isSupplied;
endinstance

(* synthesize *)
module mkCop(RevCop) ;
  Tuple2#(RevCop_, RevCop) mod_ <- _RevCop;

  rule r1;
    if((tpl_1(asIfc(mod_))).write.en)
    begin
      if((tpl_1(asIfc(mod_))).write.data == 1)
        $display("Passed\n");
      else
        $display("Failed\n");
    end
  endrule

  return tpl_2(asIfc(mod_));
endmodule


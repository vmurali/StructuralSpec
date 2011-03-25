import Base::*;
import Connectable::*;

interface Output_#(type t);
  method Action _write(t x);
endinterface

interface Output#(type t);
  method t _read();
endinterface

instance Sync_#(Output#(t));
  function Action _specCycleInputDone(Output#(t) x) = noAction;
  function Action _specCycleOutputDone(Output#(t) x) = noAction;
  function Bool _isSupplied(Output#(t) x) = True;
endinstance

instance Sync_#(Output_#(t));
  function Action _specCycleInputDone(Output_#(t) x) = noAction;
  function Action _specCycleOutputDone(Output_#(t) x) = noAction;
  function Bool _isSupplied(Output_#(t) x) = True;
endinstance

instance Connectable#(Output_#(t), Output#(t));
  module mkConnection#(Output_#(t) a, Output#(t) b)();
    rule r;
      a <= b;
    endrule
  endmodule
endinstance

instance Connectable#(Output#(t), Output_#(t));
  module mkConnection#(Output#(t) a, Output_#(t) b)();
    rule r;
      b <= a;
    endrule
  endmodule
endinstance

module _Output#(Bool enValid, OutputPulse_ en, Bool g1, Bool g2)(Tuple2#(Output_#(t), Output#(t))) provisos(Bits#(t, tSz));
  Wire#(t) w <- mkWire;

  return tuple2(
    interface Output_;
      method Action _write(t x) if(g1);
        w <= x;
        if(enValid)
          en;
      endmethod
    endinterface,
    interface Output;
      method t _read() if(g2);
        return w;
      endmethod
    endinterface);
endmodule

interface OutputPulse_;
  method Action _read();
endinterface

instance Sync_#(OutputPulse_);
  function Action _specCycleInputDone(OutputPulse_ x) = noAction;
  function Action _specCycleOutputDone(OutputPulse_ x) = noAction;
  function Bool _isSupplied(OutputPulse_ x) = True;
endinstance

typedef Output#(Bool) OutputPulse;

instance Connectable#(OutputPulse_, OutputPulse);
  module mkConnection#(OutputPulse_ a, OutputPulse b)();
    rule r;
      if(b)
        a;
    endrule
  endmodule
endinstance

instance Connectable#(OutputPulse, OutputPulse_);
  module mkConnection#(OutputPulse a, OutputPulse_ b)();
    rule r;
      if(a)
        b;
    endrule
  endmodule
endinstance

module _OutputPulse#(Bool enValid, OutputPulse_ en, Bool g1, Bool g2)(Tuple2#(OutputPulse_, OutputPulse));
  Pulse w <- mkPulse;

  return tuple2(
    interface OutputPulse_;
      method Action _read() if(g1);
        w.send;
        if(enValid)
          en;
      endmethod
    endinterface,
    interface OutputPulse;
      method Bool _read() if(g2);
        return w;
      endmethod
    endinterface);
endmodule

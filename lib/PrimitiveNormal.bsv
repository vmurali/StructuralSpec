import Base::*;
import Connectable::*;

interface Output#(type t);
  method Action _write(t x);
  method Bool isSupplied();
  method Action specCycleDone();
endinterface

interface Output_#(type t);
  method t _read();
  method Bool isSupplied();
  method Action specCycleDone();
endinterface

module _Output#(Bool enValid, OutputPulse en, Bool g1, Bool g2)(Tuple2#(Output#(t), Output_#(t))) provisos(Bits#(t, tSz));
  Wire#(t) w <- mkWire;

  return tuple2(
    interface Output;
      method Action _write(t x) if(g1);
        w <= x;
        if(enValid)
          en;
      endmethod
      method Bool isSupplied = True;
      method Action specCycleDone = noAction;
    endinterface,
    interface Output_;
      method t _read() if(g2);
        return w;
      endmethod
      method Bool isSupplied = True;
      method Action specCycleDone = noAction;
    endinterface);
endmodule

instance Connectable#(Output#(t), Output_#(t));
  module mkConnection#(Output#(t) a, Output_#(t) b)();
    rule r;
      a <= b;
    endrule
  endmodule
endinstance

instance Connectable#(Output_#(t), Output#(t));
  module mkConnection#(Output_#(t) a, Output#(t) b)();
    rule r;
      b <= a;
    endrule
  endmodule
endinstance

interface OutputPulse;
  method Action _read();
  method Bool isSupplied();
  method Action specCycleDone();
endinterface

interface OutputPulse_;
  method Bool _read();
  method Bool isSupplied();
  method Action specCycleDone();
endinterface

module _OutputPulse#(Bool enValid, OutputPulse en, Bool g1, Bool g2)(Tuple2#(OutputPulse, OutputPulse_));
  Pulse w <- mkPulse;

  return tuple2(
    interface OutputPulse;
      method Action _read() if(g1);
        w.send;
        if(enValid)
          en;
      endmethod
      method Bool isSupplied = True;
      method Action specCycleDone = noAction;
    endinterface,
    interface OutputPulse_;
      method Bool _read() if(g2);
        return w;
      endmethod
      method Bool isSupplied = True;
      method Action specCycleDone = noAction;
    endinterface);
endmodule

instance Connectable#(OutputPulse, OutputPulse_);
  module mkConnection#(OutputPulse a, OutputPulse_ b)();
    rule r;
      if(b)
        a;
    endrule
  endmodule
endinstance

instance Connectable#(OutputPulse_, OutputPulse);
  module mkConnection#(OutputPulse_ a, OutputPulse b)();
    rule r;
      if(a)
        b;
    endrule
  endmodule
endinstance
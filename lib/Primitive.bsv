import Base::*;
import Connectable::*;

interface Output#(type t);
  method Action _write(t x);
endinterface

interface Output_#(type t);
  method t _read();
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
    endinterface,
    interface Output_;
      method t _read() if(g2);
        return w;
      endmethod
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

typedef function Action send() OutputPulse;

interface OutputPulse_;
  method Bool _read();
endinterface

module _OutputPulse#(Bool enValid, OutputPulse en, Bool g1, Bool g2)(Tuple2#(OutputPulse, OutputPulse_));
  Pulse w <- mkPulse;

  function Action send();
  action
    _when_(g1, action w.send; if(enValid) en; endaction);
  endaction
  endfunction

  return tuple2(
    send,
    interface OutputPulse_;
      method Bool _read() if(g2);
        return w;
      endmethod
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

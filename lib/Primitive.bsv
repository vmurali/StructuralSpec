import Base::*;
import Connectable::*;

interface Output#(type t);
  method Action _write(t x);
endinterface

interface Output_#(type t);
  method t _read();
endinterface

module _Output#(Bool enValid, Enable en, Bool g1, Bool g2)(Tuple2#(Output#(t), Output_#(t))) provisos(Bits#(t, tSz));
  BaseWire#(t) w <- mkBaseWire;

  return tuple2(
    interface Output;
      method Action _write(t x) if(g1);
        w <= x;
        if(enValid)
          en.send;
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

interface Enable;
  method Action send();
endinterface

interface Enable_;
  method Bool _read();
endinterface

module _Enable#(Bool enValid, Enable en, Bool g1, Bool g2)(Tuple2#(Enable, Enable_));
  BasePulse w <- mkBasePulse;

  return tuple2(
    interface Enable;
      method Action send() if(g1);
        w.send;
      endmethod
    endinterface,
    interface Enable_;
      method Bool _read() if(g2);
        return w;
      endmethod
    endinterface);
endmodule

instance Connectable#(Enable, Enable_);
  module mkConnection#(Enable a, Enable_ b)();
    rule r;
      if(b)
        a.send;
    endrule
  endmodule
endinstance

instance Connectable#(Enable_, Enable);
  module mkConnection#(Enable_ a, Enable b)();
    rule r;
      if(a)
        b.send;
    endrule
  endmodule
endinstance

typedef function Action send() Send;

instance Connectable#(Send, Enable_);
  module mkConnection#(Send a, Enable_ b)();
    rule r;
      if(b)
        a;
    endrule
  endmodule
endinstance

instance Connectable#(Enable_, Send);
  module mkConnection#(Enable_ a, Send b)();
    rule r;
      if(a)
        b;
    endrule
  endmodule
endinstance

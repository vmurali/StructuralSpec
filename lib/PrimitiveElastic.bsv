import Base::*;
import Connectable::*;

typedef function Action send() Send;

interface Output#(type t);
  method Action _write(t x);
  interface Send validate;
  method Bool canAccept();

  method t invWrite();
  method Bool invValidate();
  interface Send invCanSend;
endinterface

interface Output_#(type t);
  method t _read();
  interface Bool valid;
  interface Send done;

  method Action invRead();
  interface Send invValid;
  method Bool invDone();
endinterface

module _Output#(Bool enValid, Enable en, Bool g1, Bool g2)(Tuple2#(Output#(t), Output_#(t))) provisos(Bits#(t, tSz));
  BasePulse   valid <- mkBasePulse;
  BaseWire#(t) data <- mkBaseWire;
  BasePulse    done <- mkBasePulse;

  return tuple2(
    interface Output;
      method Action _write(t x) if(g1 && !done);
        valid.send;
        data <= x;
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

typedef function Action send() Enable;

interface Enable_;
  method Bool _read();
endinterface

module _Enable#(Bool enValid, Enable en, Bool g1, Bool g2)(Tuple2#(Enable, Enable_));
  BasePulse w <- mkBasePulse;

  function Action send();
  action
    _when_(g1, w.send);
  endaction
  endfunction

  return tuple2(
    send,
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
        a;
    endrule
  endmodule
endinstance

instance Connectable#(Enable_, Enable);
  module mkConnection#(Enable_ a, Enable b)();
    rule r;
      if(a)
        b;
    endrule
  endmodule
endinstance

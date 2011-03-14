import Base::*;

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

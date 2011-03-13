import Base::*;

interface Input#(type t);
  method t _read();
endinterface

interface Output#(type t);
  method Action _write(t x);
endinterface

typedef Output#(t) RevInput#(type t);
typedef Input#(t) RevOutput#(type t);

module _Input#(Bool guard1, Bool guard2)(Tuple2#(Input#(t), Output#(t))) provisos(Bits#(t, tSz));
  BaseWire#(t) w <- mkBaseWire;

  return tuple2(
    interface Input;
      method t _read() if(guard1);
        return w;
      endmethod
    endinterface,
    interface Output;
      method Action _write(t x) if(guard2);
        w <= x;
      endmethod
    endinterface);
endmodule

module _Output#(Bool guard2, Bool guard1)(Tuple2#(Output#(t), Input#(t))) provisos(Bits#(t, tSz));
  Tuple2#(Input#(t), Output#(t)) x <- _Input(guard1, guard2);
  return tuple2(tpl_2(x), tpl_1(x));
endmodule

interface Enable;
  method Action send();
endinterface

interface RevEnable;
  method Bool _read();
endinterface

typedef Enable RevRevEnable;

module _Enable#(Bool guard1, Bool guard2)(Tuple2#(Enable, RevEnable));
  BasePulse w <- mkBasePulse;

  return tuple2(
    interface Enable;
      method Action send() if(guard1);
        w.send;
      endmethod
    endinterface,
    interface RevEnable;
      method Bool _read() if(guard2);
        return w;
      endmethod
    endinterface);
endmodule

module _RevEnable#(Bool guard2, Bool guard1)(Tuple2#(RevEnable, Enable));
  Tuple2#(Enable, RevEnable) x <- _Enable(guard1, guard2);
  return tuple2(tpl_2(x), tpl_1(x));
endmodule

module _RevRevEnable#(Bool guard1, Bool guard2)(Tuple2#(Enable, RevEnable));
  Tuple2#(Enable, RevEnable) x <- _Enable(guard1, guard2);
  return x;
endmodule

import Base::*;

interface Input#(type t);
  method t _read();
  method Action _write(t x);
endinterface

typedef Input#(t) Output#(type t);

module mkInput#(Bool guard1, Bool guard2)(Tuple2#(Input#(Bool), Output#(Bool)));
  BaseWire#(Bool) w <- mkBaseWire;

  Input#((Bool)) i = (
    interface Input#(Bool);
      method Bool _read() if(guard1);
        return w;
      endmethod

      method Action _write(Bool x) if (guard2);
        w <= x;
      endmethod
    endinterface);

  return tuple2(i,i);
endmodule

module mkOutput#(Bool guard2, Bool guard1)(Tuple2#(Output#(Bool), Input#(Bool)));
  Tuple2#(Input#(Bool), Output#(Bool)) _this <- mkInput(guard1, guard2);
  return tuple2(tpl_2(_this), tpl_1(_this));
endmodule

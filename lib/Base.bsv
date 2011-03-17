interface Wire#(type t);
  method t _read();
  method Action _write(t d);
endinterface

import "BVI" mkWire =
module mkWire(Wire#(t)) provisos(Bits#(t, tSz));
  parameter width = valueOf(tSz);
  method out _read;
  method _write(in) enable(en);
  schedule _read CF _write;
  schedule _read CF _read;
  schedule _write C _write;
  default_clock ck();
  default_reset no_reset;
  path(in, out);
endmodule

interface Pulse;
  method Bool _read();
  method Action send();
endinterface

import "BVI" mkPulse =
module mkPulse(Pulse);
  method out _read;
  method send() enable(en);
  schedule _read CF send;
  schedule _read CF _read;
  schedule send C send;
  default_clock ck();
  default_reset no_reset;
  path(en, out);
endmodule

import "BVI" mkReg =
module mkReg#(t init)(Reg#(t)) provisos(Bits#(t, tSz));
  parameter width = valueOf(tSz);
  parameter init = pack(init);
  method out _read;
  method _write (in) enable(en);
  schedule _read CF _write;
  schedule _read CF _read;
  schedule _write C _write;
  default_clock ck(clk);
  default_reset rt(rst_n) clocked_by (ck);
endmodule

import "BVI" mkRegU =
module mkRegU(Reg#(t)) provisos(Bits#(t, tSz));
  parameter width = valueOf(tSz);
  method out _read;
  method _write (in) enable(en);
  schedule _read CF _write;
  schedule _read CF _read;
  schedule _write C _write;
  default_clock ck(clk);
  default_reset no_reset;
endmodule

interface BaseWire#(type t);
  method t _read();
  method Action _write(t d);
endinterface

import "BVI" mkBaseWire =
module mkBaseWire(BaseWire#(t));
  parameter width = valueOf(SizeOf#(t));
  method out _read;
  method _write(in) enable(en);
  schedule _read CF _write;
  schedule _read CF _read;
  schedule _write C _write;
  default_clock ck();
  default_reset no_reset;
  path(in, out);
endmodule

interface PulseWire;
  method Bool _read();
  method Action _write(void x);
endinterface

import "BVI" mkPulseWire =
module mkPulseWire(PulseWire);
  method out _read;
  method _write(in) enable(en);
  schedule _read CF _write;
  schedule _read CF _read;
  schedule _write C _write;
  default_clock ck();
  default_reset no_reset;
  path(en, out);
endmodule

import "BVI" mkReg =
module mkReg#(t init)(Reg#(t));
  parameter width = valueOf(SizeOf#(t));
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
module mkRegU(Reg#(t));
  parameter width = valueOf(SizeOf#(t));
  method out _read;
  method _write (in) enable(en);
  schedule _read CF _write;
  schedule _read CF _read;
  schedule _write C _write;
  default_clock ck(clk);
  default_reset no_reset;
endmodule

interface Wire#(type t);
  method t _read();
  method Action _write(t d);
endinterface

import "BVI" mkWire =
module mkWire(Wire#(t));
  parameter Width = valueOf(SizeOf#(t));
  method out _read;
  method _write(in) enable(en);
  schedule _read CF _write;
  schedule _read CF _read;
  schedule _write C _write;
  default_clock ck();
  default_reset no_reset;
  path(x, y);
endmodule

import "BVI" mkReg =
module mkReg#(t init)(Reg#(t));
  parameter Width = valueOf(SizeOf#(t));
  parameter Init = pack(init);
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
  parameter Width = valueOf(SizeOf#(t));
  method out _read;
  method _write (in) enable(en);
  schedule _read CF _write;
  schedule _read CF _read;
  schedule _write C _write;
  default_clock ck(clk);
  default_reset no_reset;
endmodule

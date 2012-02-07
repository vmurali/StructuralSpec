(* always_ready *)
interface Wire#(type t);
  method Action write(t x);
  method t _read;
endinterface

import "BVI" mkWire =
module mkWire(Wire#(t)) provisos(Bits#(t, tSz));
  parameter width = valueOf(tSz);
  method OUT_READ _read clocked_by(no_clock);
  method write(IN_WRITE) enable(IN_EN_WRITE);
  schedule _read CF (_read, write);
  schedule write C write;
  default_clock ck();
  default_reset no_reset;
  path(IN_WRITE, OUT_READ);
endmodule

(* always_ready *)
interface Pulse;
  method Action send;
  method Bool _read;
endinterface

import "BVI" mkPulse =
module mkPulse(Pulse);
  method OUT_READ _read clocked_by(no_clock);
  method send() enable(IN_EN_WRITE);
  schedule _read CF (_read, send);
  schedule send C send;
  default_clock ck();
  default_reset no_reset;
  path(OUT_READ, IN_EN_WRITE);
  path(IN_EN_WRITE, OUT_READ);
endmodule

(* always_ready *)
interface Reg#(type t);
  method t _read;
  method Action _write(t d);
endinterface

import "BVI" mkReg =
module mkReg#(t init)(Reg#(t)) provisos(Bits#(t, tSz));
  parameter width = valueOf(tSz);
  parameter init = pack(init);
  method OUT_READ _read;
  method _write (IN_WRITE) enable(IN_EN_WRITE);
  schedule _read CF (_read, _write);
  schedule _write C _write;
  default_clock ck(CLK);
  default_reset rt(RST_N) clocked_by (ck);
endmodule

import "BVI" mkRegU =
module mkRegU(Reg#(t)) provisos(Bits#(t, tSz));
  parameter width = valueOf(tSz);
  method OUT_READ _read;
  method _write (IN_WRITE) enable(IN_EN_WRITE);
  schedule _read CF (_read, _write);
  schedule _write C _write;
  default_clock ck(CLK);
  default_reset rt(RST_N) clocked_by (ck);
endmodule

function Bit#(m) truncate(Bit#(n) x) = valueOf(m) == 0? 0: x[valueOf(m)-1:0];
function Bit#(m) truncateLSB(Bit#(n) x) = valueOf(n) == 0? 0: x[valueOf(n)-1:valueOf(n)-valueOf(m)];

import Vector::*;

function Bit#(m) zeroExtend(Bit#(n) x);
  function Bit#(1) zeroOrNorm(Integer i) = i < valueOf(n)? x[i]: 0;
  return pack(genWith(zeroOrNorm));
endfunction

function Bit#(m) signExtend(Bit#(n) x);
  function Bit#(1) signOrNorm(Integer i) = i < valueOf(n)? x[i]: (valueOf(n) == 0? ?: x[valueOf(n)-1]);
  return pack(genWith(signOrNorm));
endfunction

typedef struct {
  t1 fst;
  t2 snd;
} Pair#(type t1, type t2) deriving (Bits, Eq);

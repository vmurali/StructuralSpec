(* always_ready *)
interface Wire#(type t);
  method t _read;
  method Action write(t x);
  method Bool readValid;
  method Action readConsumed;
  method Action writeValid;
  method Bool writeConsumed;
  method Action writeEnValid;
  method Bool writeEnConsumed;
endinterface

import "BVI" mkWire =
module mkWire(Wire#(t)) provisos(Bits#(t, tSz));
  parameter width = valueOf(tSz);
  method OUT_READ _read;
  method write(IN_WRITE) enable(IN_EN_WRITE);
  method OUT_READ_VALID readValid;
  method readConsumed() enable(OUT_READ_CONSUMED);
  method writeValid() enable(IN_WRITE_VALID);
  method IN_WRITE_CONSUMED writeConsumed;
  method writeEnValid() enable(IN_EN_WRITE_VALID);
  method IN_EN_WRITE_CONSUMED writeEnConsumed;
  schedule (_read, readValid, writeConsumed, writeEnConsumed) CF (_read, readValid, readConsumed, write, writeValid, writeConsumed, writeEnValid, writeEnConsumed);
  schedule write CF (readConsumed, writeValid, writeEnValid);
  schedule readConsumed CF (write, writeValid, writeEnValid);
  schedule writeValid CF (write, readConsumed, writeEnValid);
  schedule writeEnValid CF (write, readConsumed, writeValid);
  schedule write C write;
  schedule readConsumed C readConsumed;
  schedule writeValid C writeValid;
  schedule writeEnValid C writeEnValid;
  default_clock ck();
  default_reset no_reset;
  path(IN_WRITE, OUT_READ);
  path(IN_WRITE_VALID, OUT_READ_VALID);
  path(IN_WRITE_CONSUMED, OUT_READ_CONSUMED);
endmodule

(* always_ready *)
interface Pulse;
  method Action send;
  method Bool _read;
  method Action sendValid;
  method Bool sendConsumed;
  method Bool readValid;
  method Action readConsumed;
endinterface

import "BVI" mkPulse =
module mkPulse(Pulse);
  method OUT_READ _read;
  method send() enable(IN_EN_WRITE);
  method sendValid() enable(IN_EN_WRITE_VALID);
  method IN_EN_WRITE_CONSUMED sendConsumed;
  method OUT_READ_VALID readValid;
  method readConsumed() enable(OUT_READ_CONSUMED);
  schedule (_read, sendConsumed, readValid) CF (_read, send, sendValid, sendConsumed, readValid, readConsumed);
  schedule send CF (sendValid, readConsumed);
  schedule sendValid CF (send, readConsumed);
  schedule readConsumed CF (send, sendValid);
  schedule send C send;
  schedule sendValid C sendValid;
  schedule readConsumed C readConsumed;
  default_clock ck();
  default_reset no_reset;
  path(OUT_READ, IN_EN_WRITE);
  path(OUT_READ_VALID, IN_EN_WRITE_VALID);
  path(OUT_READ_CONSUMED, IN_EN_WRITE_CONSUMED);
endmodule

(* always_ready *)
interface Reg#(type t);
  method t _read;
  method Action _write(t d);
  method Bool readValid;
  method Action readConsumed;
  method Action writeValid;
  method Bool writeConsumed;
  method Action writeEnValid;
  method Bool writeEnConsumed;
endinterface

import "BVI" mkReg =
module mkReg#(t init)(Reg#(t)) provisos(Bits#(t, tSz));
  parameter width = valueOf(tSz);
  parameter init = pack(init);
  method OUT_READ _read;
  method _write (IN_WRITE) enable(IN_EN_WRITE);
  method OUT_READ_VALID readValid;
  method readConsumed() enable(OUT_READ_CONSUMED);
  method writeValid() enable (IN_WRITE_VALID);
  method IN_WRITE_CONSUMED writeConsumed;
  method writeEnValid() enable (IN_EN_WRITE_VALID);
  method IN_EN_WRITE_CONSUMED writeEnConsumed;
  schedule (_read, readValid, writeConsumed, writeEnConsumed) CF (_read, _write, readValid, readConsumed, writeValid, writeConsumed, writeEnValid, writeEnConsumed);
  schedule _write CF (readConsumed, writeValid, writeEnValid);
  schedule readConsumed CF (_write, writeValid, writeEnValid);
  schedule writeValid CF (_write, readConsumed, writeEnValid);
  schedule writeEnValid CF (_write, readConsumed, writeValid);
  schedule _write C _write;
  schedule readConsumed C readConsumed;
  schedule writeValid C writeValid;
  schedule writeEnValid C writeEnValid;
  default_clock ck(CLK);
  default_reset rt(RST_N) clocked_by (ck);

  path (IN_WRITE_CONSUMED, IN_WRITE_VALID);
  path (IN_WRITE_CONSUMED, IN_EN_WRITE_VALID);
  path (IN_EN_WRITE_CONSUMED, IN_WRITE_VALID);
  path (IN_EN_WRITE_CONSUMED, IN_EN_WRITE_VALID);
endmodule

import "BVI" mkRegU =
module mkRegU(Reg#(t)) provisos(Bits#(t, tSz));
  parameter width = valueOf(tSz);
  method OUT_READ _read;
  method _write (IN_WRITE) enable(IN_EN_WRITE);
  method OUT_READ_VALID readValid;
  method readConsumed() enable(OUT_READ_CONSUMED);
  method writeValid() enable (IN_WRITE_VALID);
  method IN_WRITE_CONSUMED writeConsumed;
  method writeEnValid() enable (IN_EN_WRITE_VALID);
  method IN_EN_WRITE_CONSUMED writeEnConsumed;
  schedule (_read, readValid, writeConsumed, writeEnConsumed) CF (_read, _write, readValid, readConsumed, writeValid, writeConsumed, writeEnValid, writeEnConsumed);
  schedule _write CF (readConsumed, writeValid, writeEnValid);
  schedule readConsumed CF (_write, writeValid, writeEnValid);
  schedule writeValid CF (_write, readConsumed, writeEnValid);
  schedule writeEnValid CF (_write, readConsumed, writeValid);
  schedule _write C _write;
  schedule readConsumed C readConsumed;
  schedule writeValid C writeValid;
  schedule writeEnValid C writeEnValid;
  default_clock ck(CLK);
  default_reset (RST_N) clocked_by(ck);

  path (IN_WRITE_CONSUMED, IN_WRITE_VALID);
  path (IN_WRITE_CONSUMED, IN_EN_WRITE_VALID);
  path (IN_EN_WRITE_CONSUMED, IN_WRITE_VALID);
  path (IN_EN_WRITE_CONSUMED, IN_EN_WRITE_VALID);
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

(* always_ready *)
interface WireNormal#(type t);
  method t _read;
  method Action write(t x);
endinterface

import "BVI" mkWireNormal =
module mkWireNormal(WireNormal#(t)) provisos(Bits#(t, tSz));
  parameter width = valueOf(tSz);
  method OUT_READ _read;
  method write(IN_WRITE) enable(IN_EN_WRITE);
  schedule _read CF (_read, write);
  schedule write C write;
  default_clock ck();
  default_reset no_reset;
  path(IN_WRITE, OUT_READ);
endmodule

(* always_ready *)
interface PulseNormal;
  method Action send;
  method Bool _read;
endinterface

import "BVI" mkPulseNormal =
module mkPulseNormal(PulseNormal);
  method OUT_READ _read;
  method send() enable(IN_EN_WRITE);
  schedule _read CF (_read, send);
  schedule send C send;
  default_clock ck();
  default_reset no_reset;
  path(OUT_READ, IN_EN_WRITE);
endmodule

(* always_ready *)
interface RegNormal#(type t);
  method t _read;
  method Action _write(t d);
endinterface

import "BVI" mkRegNormal =
module mkRegNormal#(t init)(RegNormal#(t)) provisos(Bits#(t, tSz));
  parameter width = valueOf(tSz);
  parameter init = pack(init);
  method OUT_READ _read;
  method _write(IN_WRITE) enable(IN_EN_WRITE);
  schedule _read CF (_read, _write);
  schedule _write C _write;
  default_clock ck(CLK);
  default_reset rt(RST_N) clocked_by (ck);
endmodule

import "BVI" mkRegUNormal =
module mkRegUNormal(RegNormal#(t)) provisos(Bits#(t, tSz));
  parameter width = valueOf(tSz);
  method OUT_READ _read;
  method _write (IN_WRITE) enable(IN_EN_WRITE);
  schedule _read CF (_read, _write);
  schedule _write C _write;
  default_clock ck(CLK);
  default_reset no_reset;
endmodule

import Vector::*;

typeclass Sync_#(type t);
  function Action _specCycleDone(t x);
  function Bool _isSupplied(t x);
endtypeclass

instance Sync_#(Vector#(num, t)) provisos(Sync_#(t));
  function Action _specCycleDone(Vector#(num, t) xs) = joinActions(map(_specCycleDone, xs));
  function Bool _isSupplied(Vector#(num, t) xs) = foldl(\&& , True, map(_isSupplied, xs));
endinstance

interface Reg#(type t);
  method t _read();
  method Action _write(t d);
  method Bool isSupplied;
  method Action specCycleDone;
endinterface

instance Sync_#(Reg#(t));
  function Action _specCycleDone(Reg#(t) x) = x.specCycleDone;
  function Bool _isSupplied(Reg#(t) x) = x.isSupplied;
endinstance

import "BVI" mkReg =
module mkReg#(t init)(Reg#(t)) provisos(Bits#(t, tSz));
  parameter width = valueOf(tSz);
  parameter init = pack(init);
  method out _read;
  method _write (in) enable(en);
  method True isSupplied;
  method specCycleDone() enable(dummy);
  schedule (_read, isSupplied, specCycleDone) CF (_read, _write, isSupplied, specCycleDone);
  schedule _write CF (_read, isSupplied, specCycleDone);
  schedule _write C _write;
  default_clock ck(clk);
  default_reset rt(rst_n) clocked_by (ck);
endmodule

import "BVI" mkRegU =
module mkRegU(Reg#(t)) provisos(Bits#(t, tSz));
  parameter width = valueOf(tSz);
  method out _read;
  method _write (in) enable(en);
  method True isSupplied;
  method specCycleDone() enable(dummy);
  schedule (_read, isSupplied, specCycleDone) CF (_read, _write, isSupplied, specCycleDone);
  schedule _write CF (_read, isSupplied, specCycleDone);
  schedule _write C _write;
  default_clock ck(clk);
  default_reset no_reset;
endmodule

typedef Reg#(t) Wire#(type t);

import "BVI" mkWire =
module mkWire(Wire#(t)) provisos(Bits#(t, tSz));
  parameter width = valueOf(tSz);
  method out _read;
  method _write(in) enable(en);
  method True isSupplied;
  method specCycleDone() enable(dummy);
  schedule (_read, isSupplied, specCycleDone) CF (_read, _write, isSupplied, specCycleDone);
  schedule _write CF (_read, isSupplied, specCycleDone);
  schedule _write C _write;
  default_clock ck();
  default_reset no_reset;
  path(in, out);
endmodule

interface Pulse;
  method Bool _read();
  method Action send();
  method Bool isSupplied;
  method Action specCycleDone;
endinterface

instance Sync_#(Pulse);
  function Action _specCycleDone(Pulse x) = x.specCycleDone;
  function Bool _isSupplied(Pulse x) = x.isSupplied;
endinstance

import "BVI" mkPulse =
module mkPulse(Pulse);
  method out _read;
  method send() enable(en);
  method True isSupplied;
  method specCycleDone() enable(dummy);
  schedule (_read, isSupplied, specCycleDone) CF (_read, send, isSupplied, specCycleDone);
  schedule send CF (_read, isSupplied, specCycleDone);
  schedule send C send;
  default_clock ck();
  default_reset no_reset;
  path(en, out);
endmodule

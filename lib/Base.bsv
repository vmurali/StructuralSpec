import Vector::*;

typeclass Sync_#(type t);
  function Action _specCycleInputDone(t x);
  function Action _specCycleOutputDone(t x);
  function Bool _isSupplied(t x);
  function Bool _isAvailable(t x);
endtypeclass

instance Sync_#(Vector#(num, t)) provisos(Sync_#(t));
  function Action _specCycleInputDone(Vector#(num, t) xs) = joinActions(map(_specCycleInputDone, xs));
  function Action _specCycleOutputDone(Vector#(num, t) xs) = joinActions(map(_specCycleOutputDone, xs));
  function Bool _isSupplied(Vector#(num, t) xs) = foldl(\&& , True, map(_isSupplied, xs));
  function Bool _isAvailable(Vector#(num, t) xs) = foldl(\&& , True, map(_isAvailable, xs));
endinstance

interface Reg#(type t);
  method t _read();
  method Action _write(t d);
endinterface

instance Sync_#(Reg#(t));
  function Action _specCycleInputDone(Reg#(t) x) = noAction;
  function Action _specCycleOutputDone(Reg#(t) x) = noAction;
  function Bool _isSupplied(Reg#(t) x) = True;
  function Bool _isAvailable(Reg#(t) x) = True;
endinstance

import "BVI" mkReg =
module mkReg#(t init)(Reg#(t)) provisos(Bits#(t, tSz));
  parameter width = valueOf(tSz);
  parameter init = pack(init);
  method out _read;
  method _write (in) enable(en);
  schedule _read CF (_read, _write);
  schedule _write C _write;
  default_clock ck(clk);
  default_reset rt(rst_n) clocked_by (ck);
endmodule

import "BVI" mkRegU =
module mkRegU(Reg#(t)) provisos(Bits#(t, tSz));
  parameter width = valueOf(tSz);
  method out _read;
  method _write (in) enable(en);
  schedule _read CF (_read, _write);
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
  schedule _read CF (_read, _write);
  schedule _write C _write;
  default_clock ck();
  default_reset no_reset;
  path(in, out);
endmodule

interface Pulse;
  method Bool _read();
  method Action send();
endinterface

instance Sync_#(Pulse);
  function Action _specCycleInputDone(Pulse x) = noAction;
  function Action _specCycleOutputDone(Pulse x) = noAction;
  function Bool _isSupplied(Pulse x) = True;
  function Bool _isAvailable(Pulse x) = True;
endinstance

import "BVI" mkPulse =
module mkPulse(Pulse);
  method out _read;
  method send() enable(en);
  schedule _read CF (_read, send);
  schedule send C send;
  default_clock ck();
  default_reset no_reset;
  path(en, out);
endmodule

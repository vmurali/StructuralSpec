import Base::*;
import Vector::*;

interface RegFileLoadVerilog_#(numeric type reads, numeric type writes, numeric type n, type t);
  method Vector#(reads, t) read(Vector#(reads, Bit#(n)) index);
  method Action write(Vector#(writes, Bool) en, Vector#(writes, Bit#(n)) index, Vector#(writes, t) data);
endinterface

instance Sync_#(RegFileLoadVerilog_#(reads, writes, n, t));
  function Bool _isSupplied(x) = True;
  function Action _specCycleDone(x) = noAction;
endinstance

import "BVI" RegFileLoadVerilog_ =
module mkRegFileLoadVerilog_#(String file, Bool binary)(RegFileLoadVerilog_#(reads, writes, n, t)) provisos(Bits#(t, tSz));
  parameter reads = valueOf(reads);
  parameter writes = valueOf(writes);
  parameter width = valueOf(tSz);
  parameter n = valueOf(n);
  parameter size = valueOf(TExp#(n));
  parameter file = file;
  parameter binary = pack(binary);

  method readResp read (readReq);
  method write(writeEn, writeIndex, writeData) enable(dummy);

  schedule read CF (read, write);
  schedule write C write;

  default_clock ck(clk);
  default_reset rt(rst_n);
endmodule


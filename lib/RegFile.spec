include Library;

import RegFileLoadVerilog::*;

port RegRead#(numeric type n, type t);
  Output#(Bit#(n)) req;
  Input#(t) resp;
endport

port RegWrite#(numeric type n, type t);
  Default OutputEn#(Tuple2#(Bit#(n), t)) write;
endport

port RegFile#(numeric type reads, numeric type writes, numeric type n, type t);
  Reverse RegRead#(n, t)[reads] read;
  Reverse OutputEn#(Tuple2#(Bit#(n), t))[writes] write;
endport

partition mkRegFileLoad#(String file, Bool binary) implements RegFile#(reads, writes, n, t) provisos(Bits#(t, tSz));
  RegFileLoadVerilog_#(reads, writes, n, t) regFile <- mkRegFileLoadVerilog_(file, binary);

  Vector#(reads, Bit#(n)) req = newVector;
  for(Integer i = 0; i < valueOf(reads); i = i + 1)
    req[i] = read[i].req;
  Vector#(reads, t) resp = regFile.read(req);

  for(Integer i = 0; i < valueOf(reads); i = i + 1)
  begin
    rule r1;
      read[i].resp := resp[i];
    endrule
  end

  Vector#(writes, Bool) enables;
  Vector#(writes, Bit#(n)) index;
  Vector#(writes, t) data;
  for(Integer i = 0; i < valueOf(writes); i = i + 1)
  begin
    enables[i] = write[i].en;
    index[i] = tpl_1(write[i].data);
    data[i] = tpl_2(write[i].data);
  end

  rule r2;
    regFile.write(enables, index, data);
  endrule

  rule r3;
    specCycleDone;
  endrule
endpartition

partition mkRegFile#(t init) implements RegFile#(reads, writes, n, t) provisos(Bits#(t, tSz));
  Reg#(Vector#(TExp#(n), t)) regs <- mkReg(replicate(init));

  for(Integer i = 0; i < valueOf(reads); i = i + 1)
  begin
    rule r1;
      read[i].resp := regs[read[i].req];
    endrule
  end

  rule r2;
    Vector#(TExp#(n), t) tempRegs = regs;
    for(Integer i = 0; i < valueOf(writes); i = i + 1)
      tempRegs[tpl_1(write[i])] = tpl_2(write[i]);
    regs <= tempRegs;
  endrule

  rule r3;
    specCycleDone;
  endrule
endpartition

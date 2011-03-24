include Library;

import RegFileVerilog::*;

port RegRead#(numeric type n, type t);
  Output#(Bit#(n)) req;
  Input#(t) resp;
endport

port RegWrite#(numeric type n, type t);
  OutputEn#(Tuple2(Bit#(n), t)) write;
endport

port RegFile#(numeric type reads, numeric type writes, numeric type n, type t);
  Reverse RegRead#(n, t)[reads] read;
  Reverse RegWrite#(n, t)[writes] write;
endport

partition mkRegFileLoad#(String file, Bool binary) implements RegFile#(reads, writes, n, t) provisos(Bits#(t, tSz));
  RegFileVerilog_#(reads, writes, n, t) regFile <- regFileVerilog_(file, binary);

  rule r1;
    Vector#(reads, t) resp = unpack(regFile.read(pack(read.req)));
    for(Integer i = 0; i < valueOf(reads); i = i + 1)
      read.resp[i] := resp[i];

    Vector#(writes, Bool) enables;
    Vector#(writes, Bit#(n)) index;
    Vector#(writes, t) data;
    for(Integer i = 0; i < valueOf(writes); i = i + 1)
    begin
      enables[i] = write.write.en;
      index[i] = tpl_1(write.write);
      data[i] = tpl_2(write.write);
    end
    regFile.write(pack(enables), pack(index), pack(data));
  endrule
endpartition

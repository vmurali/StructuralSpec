include Library;

import RegFileVerilog::*;

port RegFileRead#(numeric type size, type t);
  ConditionalOutput#(Index#(size)) req;
  Input#(t) resp;
endport

port RegFile#(numeric type reads, numeric type writes, numeric type size, type t);
  Reverse RegFileRead#(size, t)[reads] read;
  Reverse ConditionalOutput#(Pair#(Index#(size), t))[writes] write;
endport

partition RegFile#(reads, writes, size, t) mkRegFileFunc#(function t init(Integer i)) provisos(Bits#(t, tSz));
  Reg#(Vector#(size, t)) regFileReg <- mkReg(genWith(init));

  atomic a;
    Vector#(size, t) regFile = regFileReg;
    for(Integer i = 0; i < valueOf(reads); i = i + 1)
      read[i].resp := regFile[read[i].req];
    for(Integer i = 0; i < valueOf(writes); i = i + 1)
      if(write[i].en)
        regFile[write[i].fst] = write[i].snd;
    regFileReg <= regFile;
  endatomic
endpartition

partinst RegFile#(reads, writes, size, t) mkRegFile#(t init) provisos(Bits#(t, tSz)) = mkRegFileFunc(constFn(init));

partition RegFile#(reads, writes, size, t) mkMultiplePorts#(function _m__#(RegFile#(1, 1, size, t)) mkSingle()) provisos(Bits#(t, tSz));
  Vector#(writes, Vector#(reads, RegFile#(1, 1, size, t))) rf <- replicateM(replicateM(mkSingle));
  Reg#(Vector#(size, Index#(writes))) whichReg <- mkReg(replicate(0));

  atomic a;
    Vector#(size, Index#(writes)) which = whichReg;
    for(Integer i = 0; i < valueOf(reads); i = i + 1)
    begin
      rf[valueOf(writes) > 1? which[read[i].req] : 0][i].read[0].req := read[i].req;
      read[i].resp := rf[valueOf(writes) > 1? which[read[i].req] : 0][i].read[0].resp;
    end
    for(Integer i = 0; i < valueOf(writes); i = i + 1)
      if(write[i].en)
      begin
        which[write[i].fst] = fromInteger(i);
        for(Integer j = 0; j < valueOf(reads); j = j + 1)
          rf[i][j].write[0] := write[i];
      end
    whichReg <= which;
  endatomic
endpartition

partition RegFile#(1, 1, size, t) mkRegFileLoadSingle#(Integer mode, String file) provisos(Bits#(t, tSz));
  RegFileVerilog_#(size, t) rf <- mkRegFileVerilogLoad(mode, file);

  atomic a;
    read[0].resp := rf.read(read[0].req);
    if(write[0].en)
      rf.write(write[0].fst, write[0].snd);
  endatomic
endpartition

partinst RegFile#(reads, writes, size, t) mkRegFileU provisos(Bits#(t, tSz)) = mkMultiplePorts(mkRegFileLoadSingle(0, ""));

partinst RegFile#(reads, writes, size, t) mkRegFileBinary#(String file) provisos(Bits#(t, tSz)) = mkMultiplePorts(mkRegFileLoadSingle(1, file));

partinst RegFile#(reads, writes, size, t) mkRegFileVmh#(String file) provisos(Bits#(t, tSz)) = mkMultiplePorts(mkRegFileLoadSingle(2, file));

include Library;

include LibraryNormal;

import RegFileVerilog::*;

port RegFileReadNormal#(numeric type size, type t);
  ConditionalOutputNormal#(Index#(size)) req;
  InputNormal#(t) resp;
endport

typedef struct {
  Index#(size) index;
  t data;
} RegFileWriteNormal#(numeric type size, type t) deriving (Bits, Eq);

port RegFileNormal#(numeric type reads, numeric type writes, numeric type size, type t);
  Reverse RegFileReadNormal#(size, t)[reads] read;
  Reverse ConditionalOutputNormal#(RegFileWriteNormal#(size, t))[writes] write;
endport

partition RegFileNormal#(reads, writes, size, t) mkRegFileFuncNormal#(function t init(Integer i)) provisos(Bits#(t, tSz));
  RegNormal#(Vector#(size, t)) regFileReg <- mkRegNormal(genWith(init));

  atomic a;
    Vector#(size, t) regFile = regFileReg;
    for(Integer i = 0; i < valueOf(reads); i = i + 1)
      read[i].resp := regFile[read[i].req];
    for(Integer i = 0; i < valueOf(writes); i = i + 1)
      if(write[i].en)
        regFile[write[i].index] = write[i].data;
    regFileReg <= regFile;
  endatomic
endpartition

partinst RegFileNormal#(reads, writes, size, t) mkRegFileNormal#(t init) provisos(Bits#(t, tSz)) = mkRegFileFuncNormal(constFn(init));

partition RegFileNormal#(reads, writes, size, t) mkMultiplePortsNormal#(function _m__#(RegFileNormal#(1, 1, size, t)) mkSingleNormal()) provisos(Bits#(t, tSz));
  Vector#(writes, Vector#(reads, RegFileNormal#(1, 1, size, t))) rf <- replicateM(replicateM(mkSingleNormal));
  RegNormal#(Vector#(size, Index#(writes))) whichReg <- mkRegNormal(replicate(0));

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
        which[write[i].index] = fromInteger(i);
        for(Integer j = 0; j < valueOf(reads); j = j + 1)
          rf[i][j].write[0] := write[i];
      end
    whichReg <= which;
  endatomic
endpartition

partition RegFileNormal#(1, 1, size, t) mkRegFileLoadSingleNormal#(Integer mode, String file) provisos(Bits#(t, tSz));
  RegFileVerilogNormal_#(size, t) rf <- mkRegFileVerilogLoadNormal(mode, file);

  atomic a;
    read[0].resp := rf.read(read[0].req);
    if(write[0].en)
      rf.write(write[0].index, write[0].data);
  endatomic
endpartition

partinst RegFileNormal#(reads, writes, size, t) mkRegFileUNormal provisos(Bits#(t, tSz)) = mkMultiplePortsNormal(mkRegFileLoadSingleNormal(0, ""));

partinst RegFileNormal#(reads, writes, size, t) mkRegFileBinaryNormal#(String file) provisos(Bits#(t, tSz)) = mkMultiplePortsNormal(mkRegFileLoadSingleNormal(1, file));

partinst RegFileNormal#(reads, writes, size, t) mkRegFileVmhNormal#(String file) provisos(Bits#(t, tSz)) = mkMultiplePortsNormal(mkRegFileLoadSingleNormal(2, file));

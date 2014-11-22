include Library;
include RegsFile;

portalias Bram = RegFile;

partition Bram#(1, 1, size, t) mkBramLoadSingle#(Integer mode, String file) provisos(Bits#(t, tSz));
  RegFile#(1, 1, size, t) rf <- mkRegFileLoadSingle(mode, file);

  Reg#(Index#(size)) valueReadReg <- mkReg(?);

  atomic a;
    valueReadReg <= read[0].req;
    read[0].resp := rf.read[0].resp;
    rf.read[0].req := valueReadReg;
    if(write[0].en)
      rf.write[0] := write[0];
  endatomic
endpartition

partinst Bram#(reads, writes, size, t) mkBramU provisos(Bits#(t, tSz)) = mkMultiplePorts(mkBramLoadSingle(0, ""));

partinst Bram#(reads, writes, size, t) mkBramBinary#(String file) provisos(Bits#(t, tSz)) = mkMultiplePorts(mkBramLoadSingle(1, file));

partinst Bram#(reads, writes, size, t) mkBramVmh#(String file) provisos(Bits#(t, tSz)) = mkMultiplePorts(mkBramLoadSingle(2, file));

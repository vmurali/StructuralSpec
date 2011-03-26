import Vector::*;
import HaskellLib::*;
import Connectable::*;
import Base::*;
import Primitive::*;

import Library::*;
import RegFileLoadVerilog::*;

interface RegRead_#(numeric type n, type t);
  interface Output_#(Bit#(n)) req;
  interface Output#(t) resp;
  method Action specCycleInputDone();
  method Action specCycleOutputDone();
  method Bool isSupplied();
endinterface

interface RegRead#(numeric type n, type t);
  interface Output#(Bit#(n)) req;
  interface Output_#(t) resp;
  method Action specCycleInputDone();
  method Action specCycleOutputDone();
  method Bool isSupplied();
endinterface

module _RegRead(Tuple2#(RegRead_#(n, t), RegRead#(n, t))) provisos(Bits#(t, _sZt));
  Tuple2#(Output_#(Bit#(n)), Output#(Bit#(n))) req_ <- _Output(False, ?, True, True);
  Tuple2#(Output_#(t), Output#(t)) resp__ <- _Output(False, ?, True, True);
  Tuple2#(Output#(t), Output_#(t)) resp_ = tuple2(tpl_2(asIfc(resp__)), tpl_1(asIfc(resp__)));
  return tuple2(
    interface RegRead_;
      interface req = tpl_1(asIfc(req_));
      interface resp = tpl_1(asIfc(resp_));
      method Action specCycleInputDone();
        _specCycleInputDone(tpl_1(asIfc(req_)));
        _specCycleInputDone(tpl_1(asIfc(resp_)));
      endmethod
      method Action specCycleOutputDone();
        _specCycleOutputDone(tpl_1(asIfc(req_)));
        _specCycleOutputDone(tpl_1(asIfc(resp_)));
      endmethod
      method Bool isSupplied = True  && _isSupplied(tpl_1(asIfc(req_))) && _isSupplied(tpl_1(asIfc(resp_)));
    endinterface,
    interface RegRead;
      interface req = tpl_2(asIfc(req_));
      interface resp = tpl_2(asIfc(resp_));
      method Action specCycleInputDone();
        _specCycleInputDone(tpl_2(asIfc(req_)));
        _specCycleInputDone(tpl_2(asIfc(resp_)));
      endmethod
      method Action specCycleOutputDone();
        _specCycleOutputDone(tpl_2(asIfc(req_)));
        _specCycleOutputDone(tpl_2(asIfc(resp_)));
      endmethod
      method Bool isSupplied = True  && _isSupplied(tpl_2(asIfc(req_))) && _isSupplied(tpl_2(asIfc(resp_)));
    endinterface);
endmodule

instance Connectable#(RegRead#(n, t), RegRead_#(n, t)) provisos(Bits#(t, _sZt));
  module mkConnection#(RegRead#(n, t) a, RegRead_#(n, t) b)();
    mkConnection(asIfc(a.req), asIfc(b.req));
    mkConnection(asIfc(a.resp), asIfc(b.resp));
  endmodule
endinstance

instance Connectable#(RegRead_#(n, t), RegRead#(n, t)) provisos(Bits#(t, _sZt));
  module mkConnection#(RegRead_#(n, t) a, RegRead#(n, t) b)();
    mkConnection(asIfc(a.req), asIfc(b.req));
    mkConnection(asIfc(a.resp), asIfc(b.resp));
  endmodule
endinstance

instance Sync_#(RegRead#(n, t));
  function Action _specCycleInputDone(RegRead#(n, t) x) = x.specCycleInputDone;
  function Action _specCycleOutputDone(RegRead#(n, t) x) = x.specCycleOutputDone;
  function Bool _isSupplied(RegRead#(n, t) x) = x.isSupplied;
endinstance

instance Sync_#(RegRead_#(n, t));
  function Action _specCycleInputDone(RegRead_#(n, t) x) = x.specCycleInputDone;
  function Action _specCycleOutputDone(RegRead_#(n, t) x) = x.specCycleOutputDone;
  function Bool _isSupplied(RegRead_#(n, t) x) = x.isSupplied;
endinstance

interface RegWrite_#(numeric type n, type t);
  interface OutputEn_#(Tuple2#(Bit#(n), t)) write;
  method Action _write((Tuple2#(Bit#(n), t)) x);
  method Action specCycleInputDone();
  method Action specCycleOutputDone();
  method Bool isSupplied();
endinterface

interface RegWrite#(numeric type n, type t);
  interface OutputEn#(Tuple2#(Bit#(n), t)) write;
  method (Tuple2#(Bit#(n), t)) _read();
  method Action specCycleInputDone();
  method Action specCycleOutputDone();
  method Bool isSupplied();
endinterface

module _RegWrite(Tuple2#(RegWrite_#(n, t), RegWrite#(n, t))) provisos(Bits#(t, _sZt));
  Tuple2#(OutputEn_#(Tuple2#(Bit#(n), t)), OutputEn#(Tuple2#(Bit#(n), t))) write_ <- _OutputEn;
  return tuple2(
    interface RegWrite_;
      interface write = tpl_1(asIfc(write_));
      method _write = (tpl_1(asIfc(write_)))._write;
      method Action specCycleInputDone();
        _specCycleInputDone(tpl_1(asIfc(write_)));
      endmethod
      method Action specCycleOutputDone();
        _specCycleOutputDone(tpl_1(asIfc(write_)));
      endmethod
      method Bool isSupplied = True  && _isSupplied(tpl_1(asIfc(write_)));
    endinterface,
    interface RegWrite;
      interface write = tpl_2(asIfc(write_));
      method _read = (tpl_2(asIfc(write_)))._read;
      method Action specCycleInputDone();
        _specCycleInputDone(tpl_2(asIfc(write_)));
      endmethod
      method Action specCycleOutputDone();
        _specCycleOutputDone(tpl_2(asIfc(write_)));
      endmethod
      method Bool isSupplied = True  && _isSupplied(tpl_2(asIfc(write_)));
    endinterface);
endmodule

instance Connectable#(RegWrite#(n, t), RegWrite_#(n, t)) provisos(Bits#(t, _sZt));
  module mkConnection#(RegWrite#(n, t) a, RegWrite_#(n, t) b)();
    mkConnection(asIfc(a.write), asIfc(b.write));
  endmodule
endinstance

instance Connectable#(RegWrite_#(n, t), RegWrite#(n, t)) provisos(Bits#(t, _sZt));
  module mkConnection#(RegWrite_#(n, t) a, RegWrite#(n, t) b)();
    mkConnection(asIfc(a.write), asIfc(b.write));
  endmodule
endinstance

instance Sync_#(RegWrite#(n, t));
  function Action _specCycleInputDone(RegWrite#(n, t) x) = x.specCycleInputDone;
  function Action _specCycleOutputDone(RegWrite#(n, t) x) = x.specCycleOutputDone;
  function Bool _isSupplied(RegWrite#(n, t) x) = x.isSupplied;
endinstance

instance Sync_#(RegWrite_#(n, t));
  function Action _specCycleInputDone(RegWrite_#(n, t) x) = x.specCycleInputDone;
  function Action _specCycleOutputDone(RegWrite_#(n, t) x) = x.specCycleOutputDone;
  function Bool _isSupplied(RegWrite_#(n, t) x) = x.isSupplied;
endinstance

interface RegFile_#(numeric type reads, numeric type writes, numeric type n, type t);
  interface Vector#(reads, RegRead#(n, t)) read;
  interface Vector#(writes, OutputEn#(Tuple2#(Bit#(n), t))) write;
  method Action specCycleInputDone();
  method Action specCycleOutputDone();
  method Bool isSupplied();
endinterface

interface RegFile#(numeric type reads, numeric type writes, numeric type n, type t);
  interface Vector#(reads, RegRead_#(n, t)) read;
  interface Vector#(writes, OutputEn_#(Tuple2#(Bit#(n), t))) write;
  method Action specCycleInputDone();
  method Action specCycleOutputDone();
  method Bool isSupplied();
endinterface

module _RegFile(Tuple2#(RegFile_#(reads, writes, n, t), RegFile#(reads, writes, n, t))) provisos(Bits#(t, _sZt));
  Tuple2#(Vector#(reads, RegRead_#(n, t)), Vector#(reads, RegRead#(n, t))) read__ <- replicateTupleM(_RegRead);
  Tuple2#(Vector#(reads, RegRead#(n, t)), Vector#(reads, RegRead_#(n, t))) read_ = tuple2(tpl_2(asIfc(read__)), tpl_1(asIfc(read__)));
  Tuple2#(Vector#(writes, OutputEn_#(Tuple2#(Bit#(n), t))), Vector#(writes, OutputEn#(Tuple2#(Bit#(n), t)))) write__ <- replicateTupleM(_OutputEn);
  Tuple2#(Vector#(writes, OutputEn#(Tuple2#(Bit#(n), t))), Vector#(writes, OutputEn_#(Tuple2#(Bit#(n), t)))) write_ = tuple2(tpl_2(asIfc(write__)), tpl_1(asIfc(write__)));
  return tuple2(
    interface RegFile_;
      interface read = tpl_1(asIfc(read_));
      interface write = tpl_1(asIfc(write_));
      method Action specCycleInputDone();
        _specCycleInputDone(tpl_1(asIfc(read_)));
        _specCycleInputDone(tpl_1(asIfc(write_)));
      endmethod
      method Action specCycleOutputDone();
        _specCycleOutputDone(tpl_1(asIfc(read_)));
        _specCycleOutputDone(tpl_1(asIfc(write_)));
      endmethod
      method Bool isSupplied = True  && _isSupplied(tpl_1(asIfc(read_))) && _isSupplied(tpl_1(asIfc(write_)));
    endinterface,
    interface RegFile;
      interface read = tpl_2(asIfc(read_));
      interface write = tpl_2(asIfc(write_));
      method Action specCycleInputDone();
        _specCycleInputDone(tpl_2(asIfc(read_)));
        _specCycleInputDone(tpl_2(asIfc(write_)));
      endmethod
      method Action specCycleOutputDone();
        _specCycleOutputDone(tpl_2(asIfc(read_)));
        _specCycleOutputDone(tpl_2(asIfc(write_)));
      endmethod
      method Bool isSupplied = True  && _isSupplied(tpl_2(asIfc(read_))) && _isSupplied(tpl_2(asIfc(write_)));
    endinterface);
endmodule

instance Connectable#(RegFile#(reads, writes, n, t), RegFile_#(reads, writes, n, t)) provisos(Bits#(t, _sZt));
  module mkConnection#(RegFile#(reads, writes, n, t) a, RegFile_#(reads, writes, n, t) b)();
    mkConnection(asIfc(a.read), asIfc(b.read));
    mkConnection(asIfc(a.write), asIfc(b.write));
  endmodule
endinstance

instance Connectable#(RegFile_#(reads, writes, n, t), RegFile#(reads, writes, n, t)) provisos(Bits#(t, _sZt));
  module mkConnection#(RegFile_#(reads, writes, n, t) a, RegFile#(reads, writes, n, t) b)();
    mkConnection(asIfc(a.read), asIfc(b.read));
    mkConnection(asIfc(a.write), asIfc(b.write));
  endmodule
endinstance

instance Sync_#(RegFile#(reads, writes, n, t));
  function Action _specCycleInputDone(RegFile#(reads, writes, n, t) x) = x.specCycleInputDone;
  function Action _specCycleOutputDone(RegFile#(reads, writes, n, t) x) = x.specCycleOutputDone;
  function Bool _isSupplied(RegFile#(reads, writes, n, t) x) = x.isSupplied;
endinstance

instance Sync_#(RegFile_#(reads, writes, n, t));
  function Action _specCycleInputDone(RegFile_#(reads, writes, n, t) x) = x.specCycleInputDone;
  function Action _specCycleOutputDone(RegFile_#(reads, writes, n, t) x) = x.specCycleOutputDone;
  function Bool _isSupplied(RegFile_#(reads, writes, n, t) x) = x.isSupplied;
endinstance

module mkRegFileLoad#(String file, Bool binary)(RegFile#(reads, writes, n, t)) provisos(Bits#(t, tSz));
  Tuple2#(RegFile_#(reads, writes, n, t), RegFile#(reads, writes, n, t)) mod_ <- _RegFile;

  RegFileLoadVerilog_#(reads, writes, n, t) regFile <- mkRegFileLoadVerilog_(file, binary);

  Vector#(reads, Bit#(n)) req = newVector;
  for(Integer i = 0; i < valueOf(reads); i = i + 1)
    req[i] = (tpl_1(asIfc(mod_))).read[i].req;
  Vector#(reads, t) resp = regFile.read(req);

  for(Integer i = 0; i < valueOf(reads); i = i + 1)
  begin
    rule r1;
      (tpl_1(asIfc(mod_))).read[i].resp._write( resp[i]);
    endrule
  end

  Vector#(writes, Bool) enables;
  Vector#(writes, Bit#(n)) index;
  Vector#(writes, t) data;
  for(Integer i = 0; i < valueOf(writes); i = i + 1)
  begin
    enables[i] = (tpl_1(asIfc(mod_))).write[i].en;
    index[i] = tpl_1((tpl_1(asIfc(mod_))).write[i].data);
    data[i] = tpl_2((tpl_1(asIfc(mod_))).write[i].data);
  end

  rule r2;
    regFile.write(enables, index, data);
  endrule

  rule r3;
    (tpl_1(asIfc(mod_))).specCycleInputDone;
    (tpl_1(asIfc(mod_))).specCycleOutputDone;
    _specCycleInputDone(asIfc(regFile));
    _specCycleOutputDone(asIfc(regFile));
 endrule

  return tpl_2(asIfc(mod_));
endmodule

module mkRegFile#(t init)(RegFile#(reads, writes, n, t)) provisos(Bits#(t, tSz));
  Tuple2#(RegFile_#(reads, writes, n, t), RegFile#(reads, writes, n, t)) mod_ <- _RegFile;

  Reg#(Vector#(TExp#(n), t)) regs <- mkReg(replicate(init));

  for(Integer i = 0; i < valueOf(reads); i = i + 1)
  begin
    rule r1;
      (tpl_1(asIfc(mod_))).read[i].resp._write( regs[(tpl_1(asIfc(mod_))).read[i].req]);
    endrule
  end

  rule r2;
    Vector#(TExp#(n), t) tempRegs = regs;
    for(Integer i = 0; i < valueOf(writes); i = i + 1)
      tempRegs[tpl_1((tpl_1(asIfc(mod_))).write[i])] = tpl_2((tpl_1(asIfc(mod_))).write[i]);
    regs <= tempRegs;
  endrule

  rule r3;
    (tpl_1(asIfc(mod_))).specCycleInputDone;
    (tpl_1(asIfc(mod_))).specCycleOutputDone;
    _specCycleInputDone(asIfc(regs));
    _specCycleOutputDone(asIfc(regs));
 endrule

  return tpl_2(asIfc(mod_));
endmodule


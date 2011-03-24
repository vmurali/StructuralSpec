import Vector::*;
import HaskellLib::*;
import Connectable::*;
import Base::*;
import Primitive::*;

import Library::*;
import RegFileVerilog::*;

interface RegRead#(numeric type n, type t);
  interface Output#(Bit#(n)) req;
  interface Output_#(t) resp;
  method Action specCycleDone();
  method Bool isSupplied();
endinterface

interface RegRead_#(numeric type n, type t);
  interface Output_#(Bit#(n)) req;
  interface Output#(t) resp;
  method Action specCycleDone();
  method Bool isSupplied();
endinterface

module _RegRead(Tuple2#(RegRead#(n, t), RegRead_#(n, t))) provisos(Bits#(t, _sZt));
  Tuple2#(Output#(Bit#(n)), Output_#(Bit#(n))) req_ <- _Output(False, ?, True, True);
  Tuple2#(Output#(t), Output_#(t)) resp__ <- _Output(False, ?, True, True);
  Tuple2#(Output_#(t), Output#(t)) resp_ = tuple2(tpl_2(asIfc(resp__)), tpl_1(asIfc(resp__)));
  return tuple2(
    interface RegRead;
      interface req = tpl_1(asIfc(req_));
      interface resp = tpl_1(asIfc(resp_));
      method Action specCycleDone();
        _specCycleDone(tpl_1(asIfc(req_)));
        _specCycleDone(tpl_1(asIfc(resp_)));
      endmethod
      method Bool isSupplied = _isSupplied(tpl_1(asIfc(req_))) && _isSupplied(tpl_1(asIfc(resp_)));
    endinterface,
    interface RegRead_;
      interface req = tpl_2(asIfc(req_));
      interface resp = tpl_2(asIfc(resp_));
      method Action specCycleDone();
        _specCycleDone(tpl_2(asIfc(req_)));
        _specCycleDone(tpl_2(asIfc(resp_)));
      endmethod
      method Bool isSupplied = _isSupplied(tpl_2(asIfc(req_))) && _isSupplied(tpl_2(asIfc(resp_)));
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
  function Action _specCycleDone(RegRead#(n, t) x) = x.specCycleDone;
  function Bool _isSupplied(RegRead#(n, t) x) = x.isSupplied;
endinstance

instance Sync_#(RegRead_#(n, t));
  function Action _specCycleDone(RegRead_#(n, t) x) = x.specCycleDone;
  function Bool _isSupplied(RegRead_#(n, t) x) = x.isSupplied;
endinstance

interface RegWrite#(numeric type n, type t);
  interface OutputEn#(Tuple2(Bit#(n), t)) write;
  method Action specCycleDone();
  method Bool isSupplied();
endinterface

interface RegWrite_#(numeric type n, type t);
  interface OutputEn_#(Tuple2(Bit#(n), t)) write;
  method Action specCycleDone();
  method Bool isSupplied();
endinterface

module _RegWrite(Tuple2#(RegWrite#(n, t), RegWrite_#(n, t))) provisos(Bits#(t, _sZt));
  Tuple2#(OutputEn#(Tuple2(Bit#(n), t)), OutputEn_#(Tuple2(Bit#(n), t))) write_ <- _OutputEn;
  return tuple2(
    interface RegWrite;
      interface write = tpl_1(asIfc(write_));
      method Action specCycleDone();
        _specCycleDone(tpl_1(asIfc(write_)));
      endmethod
      method Bool isSupplied = _isSupplied(tpl_1(asIfc(write_)));
    endinterface,
    interface RegWrite_;
      interface write = tpl_2(asIfc(write_));
      method Action specCycleDone();
        _specCycleDone(tpl_2(asIfc(write_)));
      endmethod
      method Bool isSupplied = _isSupplied(tpl_2(asIfc(write_)));
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
  function Action _specCycleDone(RegWrite#(n, t) x) = x.specCycleDone;
  function Bool _isSupplied(RegWrite#(n, t) x) = x.isSupplied;
endinstance

instance Sync_#(RegWrite_#(n, t));
  function Action _specCycleDone(RegWrite_#(n, t) x) = x.specCycleDone;
  function Bool _isSupplied(RegWrite_#(n, t) x) = x.isSupplied;
endinstance

interface RegFile#(numeric type reads, numeric type writes, numeric type n, type t);
  interface Vector#(reads, RegRead_#(n, t)) read;
  interface Vector#(writes, RegWrite_#(n, t)) write;
  method Action specCycleDone();
  method Bool isSupplied();
endinterface

interface RegFile_#(numeric type reads, numeric type writes, numeric type n, type t);
  interface Vector#(reads, RegRead#(n, t)) read;
  interface Vector#(writes, RegWrite#(n, t)) write;
  method Action specCycleDone();
  method Bool isSupplied();
endinterface

module _RegFile(Tuple2#(RegFile#(reads, writes, n, t), RegFile_#(reads, writes, n, t))) provisos(Bits#(t, _sZt));
  Tuple2#(Vector#(reads, RegRead#(n, t)), Vector#(reads, RegRead_#(n, t))) read__ <- replicateTupleM(_RegRead);
  Tuple2#(Vector#(reads, RegRead_#(n, t)), Vector#(reads, RegRead#(n, t))) read_ = tuple2(tpl_2(asIfc(read__)), tpl_1(asIfc(read__)));
  Tuple2#(Vector#(writes, RegWrite#(n, t)), Vector#(writes, RegWrite_#(n, t))) write__ <- replicateTupleM(_RegWrite);
  Tuple2#(Vector#(writes, RegWrite_#(n, t)), Vector#(writes, RegWrite#(n, t))) write_ = tuple2(tpl_2(asIfc(write__)), tpl_1(asIfc(write__)));
  return tuple2(
    interface RegFile;
      interface read = tpl_1(asIfc(read_));
      interface write = tpl_1(asIfc(write_));
      method Action specCycleDone();
        _specCycleDone(tpl_1(asIfc(read_)));
        _specCycleDone(tpl_1(asIfc(write_)));
      endmethod
      method Bool isSupplied = _isSupplied(tpl_1(asIfc(read_))) && _isSupplied(tpl_1(asIfc(write_)));
    endinterface,
    interface RegFile_;
      interface read = tpl_2(asIfc(read_));
      interface write = tpl_2(asIfc(write_));
      method Action specCycleDone();
        _specCycleDone(tpl_2(asIfc(read_)));
        _specCycleDone(tpl_2(asIfc(write_)));
      endmethod
      method Bool isSupplied = _isSupplied(tpl_2(asIfc(read_))) && _isSupplied(tpl_2(asIfc(write_)));
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
  function Action _specCycleDone(RegFile#(reads, writes, n, t) x) = x.specCycleDone;
  function Bool _isSupplied(RegFile#(reads, writes, n, t) x) = x.isSupplied;
endinstance

instance Sync_#(RegFile_#(reads, writes, n, t));
  function Action _specCycleDone(RegFile_#(reads, writes, n, t) x) = x.specCycleDone;
  function Bool _isSupplied(RegFile_#(reads, writes, n, t) x) = x.isSupplied;
endinstance

module mkRegFileLoad#(String file, Bool binary)(RegFile_#(reads, writes, n, t)) provisos(Bits#(t, tSz));
  Tuple2#(RegFile#(reads, writes, n, t), RegFile_#(reads, writes, n, t)) mod_ <- _RegFile;

  RegFileVerilog_#(reads, writes, n, t) regFile <- regFileVerilog_(file, binary);

  rule r1;
    Vector#(reads, t) resp = unpack(regFile.read(pack((tpl_1(asIfc(mod_))).read.req)));
    for(Integer i = 0; i < valueOf(reads); i = i + 1)
      (tpl_1(asIfc(mod_))).read.resp[i] <= resp[i];

    Vector#(writes, Bool) enables;
    Vector#(writes, Bit#(n)) index;
    Vector#(writes, t) data;
    for(Integer i = 0; i < valueOf(writes); i = i + 1)
    begin
      enables[i] = (tpl_1(asIfc(mod_))).write.write.en;
      index[i] = tpl_1((tpl_1(asIfc(mod_))).write.write);
      data[i] = tpl_2((tpl_1(asIfc(mod_))).write.write);
    end
    regFile.write(pack(enables), pack(index), pack(data));
  endrule

  return tpl_2(asIfc(mod_));
endmodule


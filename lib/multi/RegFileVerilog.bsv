(* always_ready *)
interface RegFileVerilog_#(numeric type size, type t);
  method t read(Bit#(TLog#(size)) addr);
  method Action readReqValid;
  method Bool readReqConsumed;
  method Bool readRespValid;
  method Action readRespConsumed;
  method Action write(Bit#(TLog#(size)) addr, t data);
  method Action writeEnValid;
  method Bool writeEnConsumed;
  method Action writeIndexValid;
  method Bool writeIndexConsumed;
  method Action writeDataValid;
  method Bool writeDataConsumed;
endinterface

import "BVI" mkRegFileVerilogLoad =
module mkRegFileVerilogLoad#(Integer mode, String file)(RegFileVerilog_#(size, t)) provisos(Bits#(t, tSz));
  parameter width = valueOf(tSz);
  parameter n = valueOf(TLog#(size));
  parameter size = valueOf(size);
  parameter file = file;
  parameter mode = mode;

  method READ_RESP_READ read (READ_REQ_WRITE);
  method write(WRITE_INDEX_WRITE, WRITE_DATA_WRITE) enable(WRITE_EN_WRITE);
  method readReqValid() enable(READ_REQ_WRITE_VALID);
  method READ_REQ_WRITE_CONSUMED readReqConsumed;
  method READ_RESP_READ_VALID readRespValid;
  method readRespConsumed() enable(READ_RESP_READ_CONSUMED);
  method writeEnValid() enable(WRITE_EN_WRITE_VALID);
  method WRITE_EN_WRITE_CONSUMED writeEnConsumed;
  method writeIndexValid() enable(WRITE_INDEX_WRITE_VALID);
  method WRITE_INDEX_WRITE_CONSUMED writeIndexConsumed;
  method writeDataValid() enable(WRITE_DATA_WRITE_VALID);
  method WRITE_DATA_WRITE_CONSUMED writeDataConsumed;

  schedule (read, readReqConsumed, readRespValid, writeEnConsumed, writeIndexConsumed, writeDataConsumed) CF (read, write, readReqValid, readReqConsumed, readRespValid, readRespConsumed, writeEnValid, writeEnConsumed, writeIndexValid, writeIndexConsumed, writeDataValid, writeDataConsumed);
  schedule readReqValid CF (readRespConsumed, write, writeEnValid, writeIndexValid, writeDataValid);
  schedule readRespConsumed CF (readReqValid, write, writeEnValid, writeIndexValid, writeDataValid);
  schedule write CF (readRespConsumed, readReqValid, writeEnValid, writeIndexValid, writeDataValid);
  schedule writeEnValid CF (readRespConsumed, readReqValid, write, writeIndexValid, writeDataValid);
  schedule writeIndexValid CF (readRespConsumed, readReqValid, write, writeEnValid, writeDataValid);
  schedule writeDataValid CF (readRespConsumed, readReqValid, write, writeEnValid, writeIndexValid);
  schedule write C write;
  schedule readReqValid C readReqValid;
  schedule readRespConsumed C readRespConsumed;
  schedule writeEnValid C writeEnValid;
  schedule writeIndexValid C writeIndexValid;
  schedule writeDataValid C writeDataValid;

  default_clock ck(CLK);
  default_reset rt(RST_N) clocked_by(ck);

  path (READ_REQ_WRITE, READ_RESP_READ);
  path (READ_REQ_WRITE_VALID, READ_RESP_READ_VALID);
  path (READ_REQ_WRITE_CONSUMED, READ_RESP_READ_CONSUMED);
  path (WRITE_EN_WRITE_CONSUMED, WRITE_EN_WRITE_VALID);
  path (WRITE_EN_WRITE_CONSUMED, WRITE_INDEX_WRITE_VALID);
  path (WRITE_EN_WRITE_CONSUMED, WRITE_DATA_WRITE_VALID);
  path (WRITE_INDEX_WRITE_CONSUMED, WRITE_EN_WRITE_VALID);
  path (WRITE_INDEX_WRITE_CONSUMED, WRITE_INDEX_WRITE_VALID);
  path (WRITE_INDEX_WRITE_CONSUMED, WRITE_DATA_WRITE_VALID);
  path (WRITE_DATA_WRITE_CONSUMED, WRITE_EN_WRITE_VALID);
  path (WRITE_DATA_WRITE_CONSUMED, WRITE_INDEX_WRITE_VALID);
  path (WRITE_DATA_WRITE_CONSUMED, WRITE_DATA_WRITE_VALID);
endmodule

(* always_ready *)
interface RegFileVerilogNormal_#(numeric type size, type t);
  method t read(Bit#(TLog#(size)) addr);
  method Action write(Bit#(TLog#(size)) addr, t data);
endinterface

import "BVI" mkRegFileVerilogLoadNormal =
module mkRegFileVerilogLoadNormal#(Integer mode, String file)(RegFileVerilogNormal_#(size, t)) provisos(Bits#(t, tSz));
  parameter width = valueOf(tSz);
  parameter n = valueOf(TLog#(size));
  parameter size = valueOf(size);
  parameter file = file;
  parameter mode = mode;

  method READ_RESP_READ read (READ_REQ_WRITE);
  method write(WRITE_INDEX_WRITE, WRITE_DATA_WRITE) enable(WRITE_EN_WRITE);

  schedule read CF (read, write);
  schedule write C write;

  default_clock ck(CLK);
  default_reset rt(RST_N) clocked_by(ck);

  path (READ_REQ_WRITE, READ_RESP_READ);
endmodule

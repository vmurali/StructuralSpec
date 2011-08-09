import Vector::*;

(* always_ready *)
interface Rand32;
  method Action req;
  method Action reqValid;
  method Bool reqConsumed;
  method Bit#(32) resp;
  method Bool respValid;
  method Action respConsumed;
endinterface

import "BVI" mkRand32 =
module mkRand32#(Bit#(32) seed)(Rand32);
  parameter seed = seed;
  method req() enable(REQ_WRITE);
  method reqValid() enable(REQ_WRITE_VALID);
  method REQ_WRITE_CONSUMED reqConsumed;
  method RESP_READ resp;
  method RESP_READ_VALID respValid;
  method respConsumed() enable(RESP_READ_CONSUMED);
  default_clock ck(CLK);
  default_reset no_reset;
  schedule (resp, reqConsumed, respValid) CF (req, reqValid, reqConsumed, resp, respValid, respConsumed);
  schedule req CF (reqValid, respConsumed);
  schedule reqValid CF (req, respConsumed);
  schedule respConsumed CF (req, reqValid);
  schedule req C req;
  schedule reqValid C reqValid;
  schedule respConsumed C respConsumed;
  path(REQ_WRITE, RESP_READ);
endmodule

(* always_ready *)
interface Rand32Normal;
  method Action req;
  method Bit#(32) resp;
endinterface

import "BVI" mkRand32Normal =
module mkRand32Normal#(Bit#(32) seed)(Rand32Normal);
  parameter seed = seed;
  method req() enable(REQ_WRITE);
  method RESP_READ resp;
  default_clock ck(CLK);
  default_reset no_reset;
  schedule resp CF (req, resp);
  schedule req C req;
  path(REQ_WRITE, RESP_READ);
endmodule

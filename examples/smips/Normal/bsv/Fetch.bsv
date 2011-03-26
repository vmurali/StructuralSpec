import Vector::*;
import HaskellLib::*;
import Connectable::*;
import Base::*;
import Primitive::*;

import Library::*;
import Types::*;
import FetchPort::*;
(* synthesize *)
module mkFetch(Fetch) ;
  Tuple2#(Fetch_, Fetch) mod_ <- _Fetch;

  Reg#(VAddr)   pc <- mkReg(0);
  Reg#(Bool) epoch <- mkRegU;

  rule r1;
    (tpl_1(asIfc(mod_))).instReqQ.data._write( pc);
    (tpl_1(asIfc(mod_))).pcQ.data._write( tuple2(pc + 4, epoch));
  endrule

  rule r2;
    (tpl_1(asIfc(mod_))).currEpoch._write( epoch);

    if((tpl_1(asIfc(mod_))).branchPc.en)
    begin
      pc <= (tpl_1(asIfc(mod_))).branchPc;
      epoch <= !epoch;
    end
    else
      pc <= pc + 4;
  endrule

  return tpl_2(asIfc(mod_));
endmodule


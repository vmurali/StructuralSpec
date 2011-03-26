import Vector::*;
import HaskellLib::*;
import Connectable::*;
import Base::*;
import Primitive::*;

import Library::*;
import Types::*;
import RegFile::*;
import MemoryPort::*;
(* synthesize *)
module mkMemory(Memory) ;
  Tuple2#(Memory_, Memory) mod_ <- _Memory;

  RegFile#(2, 1, 20, Data) regs <- mkRegFileLoad("memory.vmh", False);

  rule r1;
    regs.read[0].req._write( truncate((tpl_1(asIfc(mod_))).instReqQ.first));
    (tpl_1(asIfc(mod_))).instReqQ.deq;
    (tpl_1(asIfc(mod_))).instQ.enq._write( regs.read[0].resp);
  endrule

  rule r2;
    case ((tpl_1(asIfc(mod_))).dataReqQ.first.data) matches
      tagged Valid .d:
        regs.write.data._write( tuple2((tpl_1(asIfc(mod_))).dataReqQ.first.index, d));
      tagged Invalid:
        begin
          regs.read[1].req._write( (tpl_1(asIfc(mod_))).dataReqQ.first.index);
          (tpl_1(asIfc(mod_))).dataQ.enq._write( regs.read[1].resp);
        end
    endcase
  endrule

  return tpl_2(asIfc(mod_));
endmodule


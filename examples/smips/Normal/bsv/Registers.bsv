import Vector::*;
import HaskellLib::*;
import Connectable::*;
import Base::*;
import Primitive::*;

import Library::*;
import Types::*;
import RegFile::*;
import RegistersPort::*;
(* synthesize *)
module mkRegisters(Registers) ;
  Tuple2#(Registers_, Registers) mod_ <- _Registers;

  RegFile#(2, 1, RegIndexSz, Data) regs <- mkRegFile(0);

  rule r1;
    for(Integer i = 0; i < 2; i = i + 1)
    begin
      regs.read[i].req._write( (tpl_1(asIfc(mod_))).read[i].req);

      (tpl_1(asIfc(mod_))).read[i].resp._write( tpl_1((tpl_1(asIfc(mod_))).write.data) == (tpl_1(asIfc(mod_))).read[i].req?
                        tpl_2((tpl_1(asIfc(mod_))).write.data):
                        regs.read[i].resp);
    end

    if((tpl_1(asIfc(mod_))).write.en && tpl_1((tpl_1(asIfc(mod_))).write.data) != 0)
      regs.write[0].data._write( (tpl_1(asIfc(mod_))).write.data);
  endrule

  return tpl_2(asIfc(mod_));
endmodule


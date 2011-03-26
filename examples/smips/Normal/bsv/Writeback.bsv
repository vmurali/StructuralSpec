import Vector::*;
import HaskellLib::*;
import Connectable::*;
import Base::*;
import Primitive::*;

import Library::*;
import Types::*;
import Fifo::*;
import RegFile::*;
import WritebackPort::*;
(* synthesize *)
module mkWriteback(Writeback) ;
  Tuple2#(Writeback_, Writeback) mod_ <- _Writeback;

  Fifo#(1, Wb) wbQ <- mkLFifo;

  mkConnection(asIfc((tpl_1(asIfc(mod_))).wb), wbQ.enq);

  rule r1;
    (tpl_1(asIfc(mod_))).wbIndex.data._write( wbQ.deq.index);
    if(wbQ.deq.data matches tagged Valid .d)
    begin
      (tpl_1(asIfc(mod_))).regWrite._write( tuple2(wbQ.deq.index, d));
      wbQ.deq.deq;
    end
    else
    begin
      (tpl_1(asIfc(mod_))).regWrite._write( tuple2(wbQ.deq.index, (tpl_1(asIfc(mod_))).dataQ.first));
      wbQ.deq.deq;
      (tpl_1(asIfc(mod_))).dataQ.deq;
    end
  endrule

  return tpl_2(asIfc(mod_));
endmodule


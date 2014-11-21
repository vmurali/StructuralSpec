import Vector::*;
import HaskellLib::*;
import Connectable::*;
import Base::*;
import Primitive::*;
export Processor::*;

import Core::*;
export Core::*;

import Mem::*;
export Mem::*;

(* synthesize *)
module mkProcessor(Empty) ;
  Tuple2#(Empty_, Empty) mod_ <- _Empty;

  let core <- mkCore;
  let  mem <- mkMemory;

  mkConnection(asIfc(core.mem), asIfc( mem));

  return tpl_2(asIfc(mod_));
endmodule


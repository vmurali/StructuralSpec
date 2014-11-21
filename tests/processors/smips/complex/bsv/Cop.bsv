import Vector::*;
import HaskellLib::*;
import Connectable::*;
import Base::*;
import Primitive::*;
export Cop::*;

import Library::*;
export Library::*;

import Types::*;
export Types::*;

interface Cop_;
  interface ConditionalOutput#(Pair#(RegIndex, Data)) write;
  interface Output_#(Data) read;
endinterface

interface Cop;
  interface ConditionalOutput_#(Pair#(RegIndex, Data)) write;
  interface Output#(Data) read;
endinterface

module _Cop(Tuple2#(Cop_, Cop)) ;
  Tuple2#(ConditionalOutput_#(Pair#(RegIndex, Data)), ConditionalOutput#(Pair#(RegIndex, Data))) write_ <- _ConditionalOutput(True, True);
  Tuple2#(Output_#(Data), Output#(Data)) read_ <- _Output(True, True);
  return tuple2(
    interface Cop_;
      interface write = tpl_2(asIfc(write_));
      interface read = tpl_1(asIfc(read_));
    endinterface,
    interface Cop;
      interface write = tpl_1(asIfc(write_));
      interface read = tpl_2(asIfc(read_));
    endinterface);
endmodule

instance Connectable#(Cop, Cop_) ;
  module mkConnection#(Cop a, Cop_ b)();
    mkConnection(asIfc(a.write), asIfc(b.write));
    mkConnection(asIfc(a.read), asIfc(b.read));
  endmodule
endinstance

instance Connectable#(Cop_, Cop) ;
  module mkConnection#(Cop_ a, Cop b)();
    mkConnection(asIfc(b), asIfc(a));
  endmodule
endinstance

(* synthesize *)
module mkCop(Cop) ;
  Tuple2#(Cop_, Cop) mod_ <- _Cop;

  Reg#(Data) finishReg <- mkReg(0);
  Reg#(Data)  statsReg <- mkReg(0);

  (* fire_when_enabled *) rule r1;
    if((tpl_1(asIfc(mod_))).write.en)
    begin
      if((tpl_1(asIfc(mod_))).write.fst == 21)
      begin
        if((tpl_1(asIfc(mod_))).write.snd == 1)
          $display("Passed");
        else
          $display("Failed");
        finishReg <= (tpl_1(asIfc(mod_))).write.snd;
      end
      else
        statsReg <= (tpl_1(asIfc(mod_))).write.snd;
    end
  endrule

  (* fire_when_enabled *) rule r2;
    (tpl_1(asIfc(mod_))).read.write( statsReg);
  endrule

  (* fire_when_enabled *) rule r3;
    if(finishReg != 0)
      $finish(truncate(finishReg));
  endrule

  return tpl_2(asIfc(mod_));
endmodule


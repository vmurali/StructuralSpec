import Vector::*;
import HaskellLib::*;
import Connectable::*;
import Base::*;
import Primitive::*;

import Library::*;
import Types::*;
import Fifo::*;
import RegFile::*;
import Cop::*;
import ExecutePort::*;
(* synthesize *)
module mkExecute(Execute) ;
  Tuple2#(Execute_, Execute) mod_ <- _Execute;

  let inst = (tpl_1(asIfc(mod_))).instQ.first;
  let reg1 = inst[25:21];
  let reg2 = inst[20:16];
  match {.pcPlus4, .epoch} = (tpl_1(asIfc(mod_))).pcQ.first;

  rule r1;
    (tpl_1(asIfc(mod_))).regRead[0].req._write( reg1);
    (tpl_1(asIfc(mod_))).regRead[1].req._write( reg2);
  endrule

  rule r2;
    if(epoch != (tpl_1(asIfc(mod_))).currEpoch)
    begin
      (tpl_1(asIfc(mod_))).pcQ.deq;
      (tpl_1(asIfc(mod_))).instQ.deq;
    end
    else
    begin
      let stall = (isSrcValid(inst)[0] && (tpl_1(asIfc(mod_))).wbIndex.en && (tpl_1(asIfc(mod_))).wbIndex.data == reg1 || isSrcValid(inst)[1] && (tpl_1(asIfc(mod_))).wbIndex.en && (tpl_1(asIfc(mod_))).wbIndex.data == reg2);
      if(!stall)
      begin
        (tpl_1(asIfc(mod_))).pcQ.deq;
        (tpl_1(asIfc(mod_))).instQ.deq;
        let dest = getDest(inst);
        if(isBranch(inst))
        begin
          match {.taken, .branchTarget, .isLinked} = branch(inst, (tpl_1(asIfc(mod_))).regRead[0].resp, (tpl_1(asIfc(mod_))).regRead[1].resp, pcPlus4);
          if(taken)
            (tpl_1(asIfc(mod_))).branchPc.data._write( branchTarget);
          if(isLinked)
            (tpl_1(asIfc(mod_))).wbQ.data._write( Wb{index: dest, data: tagged Valid pcPlus4});
        end
        else
        begin
          let res = aluDataAddr(inst, (tpl_1(asIfc(mod_))).regRead[0].resp, (tpl_1(asIfc(mod_))).regRead[1].resp);
          if(isLoad(inst))
          begin
            (tpl_1(asIfc(mod_))).wbQ.data._write( Wb{index: dest, data: tagged Invalid});
            (tpl_1(asIfc(mod_))).dataReqQ.data._write( tagged Load aluDataAddr(inst, (tpl_1(asIfc(mod_))).regRead[0].resp, (tpl_1(asIfc(mod_))).regRead[1].resp));
          end
          else if(isStore(inst))
            (tpl_1(asIfc(mod_))).dataReqQ.data._write( tagged Store tuple2(res, (tpl_1(asIfc(mod_))).regRead[1].resp));
          else if(copRead(inst))
            (tpl_1(asIfc(mod_))).wbQ.data._write( Wb{index: dest, data: tagged Valid ((tpl_1(asIfc(mod_))).cop.read)});
          else if(copWrite(inst))
            (tpl_1(asIfc(mod_))).cop.write._write( (tpl_1(asIfc(mod_))).regRead[1].resp);
          else
            (tpl_1(asIfc(mod_))).wbQ.data._write( Wb{index: dest, data: tagged Valid res});
        end
      end
    end
  endrule

  return tpl_2(asIfc(mod_));
endmodule


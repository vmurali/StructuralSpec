import Vector::*;
import HaskellLib::*;
import Connectable::*;
import Base::*;
import Primitive::*;
export Execute::*;

import Library::*;
export Library::*;

import Types::*;
export Types::*;

import RegsFile::*;
export RegsFile::*;

import Cop::*;
export Cop::*;

import Fifo::*;
export Fifo::*;

import ExecUtils::*;
export ExecUtils::*;

interface Execute_;
  interface FifoDeq_#(Pair#(VAddr, Bool)) pcQ;
  interface FifoDeq_#(Inst) instQ;
  interface FifoEnq_#(Mem) dataReqQ;
  interface Vector#(2, RegFileRead_#(NumRegs, Data)) regRead;
  interface FifoEnq_#(Pair#(RegIndex, Maybe#(Data))) wbQ;
  interface ConditionalOutput#(RegIndex) wbIndex;
  interface Output#(Bool) currEpoch;
  interface ConditionalOutput_#(VAddr) branchPc;
  interface Cop cop;
endinterface

interface Execute;
  interface FifoDeq#(Pair#(VAddr, Bool)) pcQ;
  interface FifoDeq#(Inst) instQ;
  interface FifoEnq#(Mem) dataReqQ;
  interface Vector#(2, RegFileRead#(NumRegs, Data)) regRead;
  interface FifoEnq#(Pair#(RegIndex, Maybe#(Data))) wbQ;
  interface ConditionalOutput_#(RegIndex) wbIndex;
  interface Output_#(Bool) currEpoch;
  interface ConditionalOutput#(VAddr) branchPc;
  interface Cop_ cop;
endinterface

module _Execute(Tuple2#(Execute_, Execute)) ;
  Tuple2#(FifoDeq_#(Pair#(VAddr, Bool)), FifoDeq#(Pair#(VAddr, Bool))) pcQ_ <- _FifoDeq;
  Tuple2#(FifoDeq_#(Inst), FifoDeq#(Inst)) instQ_ <- _FifoDeq;
  Tuple2#(FifoEnq_#(Mem), FifoEnq#(Mem)) dataReqQ_ <- _FifoEnq;
  Tuple2#(Vector#(2, RegFileRead_#(NumRegs, Data)), Vector#(2, RegFileRead#(NumRegs, Data))) regRead_ <- replicateTupleM(_RegFileRead);
  Tuple2#(FifoEnq_#(Pair#(RegIndex, Maybe#(Data))), FifoEnq#(Pair#(RegIndex, Maybe#(Data)))) wbQ_ <- _FifoEnq;
  Tuple2#(ConditionalOutput_#(RegIndex), ConditionalOutput#(RegIndex)) wbIndex_ <- _ConditionalOutput(True, True);
  Tuple2#(Output_#(Bool), Output#(Bool)) currEpoch_ <- _Output(True, True);
  Tuple2#(ConditionalOutput_#(VAddr), ConditionalOutput#(VAddr)) branchPc_ <- _ConditionalOutput(True, True);
  Tuple2#(Cop_, Cop) cop_ <- _Cop;
  return tuple2(
    interface Execute_;
      interface pcQ = tpl_1(asIfc(pcQ_));
      interface instQ = tpl_1(asIfc(instQ_));
      interface dataReqQ = tpl_1(asIfc(dataReqQ_));
      interface regRead = tpl_1(asIfc(regRead_));
      interface wbQ = tpl_1(asIfc(wbQ_));
      interface wbIndex = tpl_2(asIfc(wbIndex_));
      interface currEpoch = tpl_2(asIfc(currEpoch_));
      interface branchPc = tpl_1(asIfc(branchPc_));
      interface cop = tpl_2(asIfc(cop_));
    endinterface,
    interface Execute;
      interface pcQ = tpl_2(asIfc(pcQ_));
      interface instQ = tpl_2(asIfc(instQ_));
      interface dataReqQ = tpl_2(asIfc(dataReqQ_));
      interface regRead = tpl_2(asIfc(regRead_));
      interface wbQ = tpl_2(asIfc(wbQ_));
      interface wbIndex = tpl_1(asIfc(wbIndex_));
      interface currEpoch = tpl_1(asIfc(currEpoch_));
      interface branchPc = tpl_2(asIfc(branchPc_));
      interface cop = tpl_1(asIfc(cop_));
    endinterface);
endmodule

instance Connectable#(Execute, Execute_) ;
  module mkConnection#(Execute a, Execute_ b)();
    mkConnection(asIfc(a.pcQ), asIfc(b.pcQ));
    mkConnection(asIfc(a.instQ), asIfc(b.instQ));
    mkConnection(asIfc(a.dataReqQ), asIfc(b.dataReqQ));
    mkConnection(asIfc(a.regRead), asIfc(b.regRead));
    mkConnection(asIfc(a.wbQ), asIfc(b.wbQ));
    mkConnection(asIfc(a.wbIndex), asIfc(b.wbIndex));
    mkConnection(asIfc(a.currEpoch), asIfc(b.currEpoch));
    mkConnection(asIfc(a.branchPc), asIfc(b.branchPc));
    mkConnection(asIfc(a.cop), asIfc(b.cop));
  endmodule
endinstance

instance Connectable#(Execute_, Execute) ;
  module mkConnection#(Execute_ a, Execute b)();
    mkConnection(asIfc(b), asIfc(a));
  endmodule
endinstance

(* synthesize *)
module mkExecute(Execute) ;
  Tuple2#(Execute_, Execute) mod_ <- _Execute;

  let inst = (tpl_1(asIfc(mod_))).instQ.first;
  let reg1 = inst[25:21];
  let reg2 = inst[20:16];
  match tagged Pair{snd:.epoch, fst:.pcPlus4} = (tpl_1(asIfc(mod_))).pcQ.first;

  (* fire_when_enabled *) rule r1;
    (tpl_1(asIfc(mod_))).regRead[0].req.write( reg1);
    (tpl_1(asIfc(mod_))).regRead[1].req.write( reg2);
  endrule

  (* fire_when_enabled *) rule r2;
    if(epoch != (tpl_1(asIfc(mod_))).currEpoch)
    begin
      (tpl_1(asIfc(mod_))).pcQ.deq;
      (tpl_1(asIfc(mod_))).instQ.deq;
    end
    else
    begin
      let stall = (isSrcValid(inst)[0] && (tpl_1(asIfc(mod_))).wbIndex.en && (tpl_1(asIfc(mod_))).wbIndex == reg1 || isSrcValid(inst)[1] && (tpl_1(asIfc(mod_))).wbIndex.en && (tpl_1(asIfc(mod_))).wbIndex == reg2);
      if(!stall)
      begin
        (tpl_1(asIfc(mod_))).pcQ.deq;
        (tpl_1(asIfc(mod_))).instQ.deq;
        let dest = getDest(inst);
        if(isBranch(inst))
        begin
          match {.taken, .branchTarget, .isLinked} = branch(inst, (tpl_1(asIfc(mod_))).regRead[0].resp, (tpl_1(asIfc(mod_))).regRead[1].resp, pcPlus4);
          if(taken)
            (tpl_1(asIfc(mod_))).branchPc.write( branchTarget);
          if(isLinked)
            (tpl_1(asIfc(mod_))).wbQ.enq.write( Pair{fst: dest, snd: tagged Valid pcPlus4});
        end
        else
        begin
          let res = aluDataAddr(inst, (tpl_1(asIfc(mod_))).regRead[0].resp, (tpl_1(asIfc(mod_))).regRead[1].resp);
          if(isLoad(inst))
          begin
            (tpl_1(asIfc(mod_))).wbQ.enq.write( Pair{fst: dest, snd: tagged Invalid});
            (tpl_1(asIfc(mod_))).dataReqQ.enq.write( tagged Load aluDataAddr(inst, (tpl_1(asIfc(mod_))).regRead[0].resp, (tpl_1(asIfc(mod_))).regRead[1].resp));
          end
          else if(isStore(inst))
            (tpl_1(asIfc(mod_))).dataReqQ.enq.write( tagged Store (Pair{fst: res, snd: (tpl_1(asIfc(mod_))).regRead[1].resp}));
          else if(copRead(inst))
            (tpl_1(asIfc(mod_))).wbQ.enq.write( Pair{fst: dest, snd: tagged Valid ((tpl_1(asIfc(mod_))).cop.read)});
          else if(copWrite(inst))
            (tpl_1(asIfc(mod_))).cop.write.write( Pair{fst: copReg(inst), snd: (tpl_1(asIfc(mod_))).regRead[1].resp});
          else
            (tpl_1(asIfc(mod_))).wbQ.enq.write( Pair{fst: dest, snd: tagged Valid res});
        end
      end
    end
  endrule

  return tpl_2(asIfc(mod_));
endmodule


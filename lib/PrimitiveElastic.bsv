import Base::*;
import Connectable::*;
import Vector::*;

typedef function Action send() Send;

typeclass Sync_#(type t);
  function Action _specCycleDone(t x);
  function Bool _isSupplied(t x);
endtypeclass

instance Sync_#(Vector#(num, t)) provisos(Sync_#(t));
  function Action _specCycleDone(Vector#(num, t) xs) = joinActions(map(_specCycleDone, xs));
  function Bool _isSupplied(Vector#(num, t) xs) = foldl(\&& , True, map(_isSupplied, xs));
endinstance

interface OutputCarry_#(type t);
  method Action connected();
  method Maybe#(t) write();
  method Bool justFinish();
  method Action canAccept();
  method Action isSupplied();
  method Bool specCycleDone();
endinterface

interface Output#(type t);
  method Action _write(t x);
  method Action justFinish();
  method Bool canAccept();
  method Bool isSupplied();
  method Action specCycleDone();
endinterface

interface Output_#(type t);
  method t _read();
  method Bool isValid();
  method Bool canFinish();
  method Action specCycleDone();

  method Bool isSupplied();

  interface OutputCarry_#(t) carry;
endinterface

module _Output#(Bool enValid, OutputPulse en, Bool g1, Bool g2)(Tuple2#(Output#(t), Output_#(t))) provisos(Bits#(t, tSz));
  Pulse         carryWire <- mkPulse;

  Reg#(Maybe#(t)) dataReg <- mkReg(tagged Invalid);
  Reg#(Bool)      usedReg <- mkReg(True);
  Reg#(Bool)  suppliedReg <- mkReg(False);

  Pulse         dataValid <- mkPulse;
  Wire#(t)       dataWire <- mkWire;
  Pulse      justFinishIn <- mkPulse;
  Pulse            usedIn <- mkPulse;
  Pulse   specCycleDoneIn <- mkPulse;

  Maybe#(t)        dataIn  = dataValid?
                               tagged Valid dataWire:
                               tagged Invalid;

  Pulse    canAcceptCarry <- mkPulse;
  Pulse   isSuppliedCarry <- mkPulse;

  Bool       canAcceptOut  = carryWire?
                               canAcceptCarry:
                               !suppliedReg && usedReg;

  Bool      isSuppliedOut  = carryWire?
                               isSuppliedCarry:
                               suppliedReg || dataValid;

  Maybe#(t)       dataOut  = case (dataReg) matches
                               Invalid : dataIn;
                               default : dataReg;
                             endcase;

  rule r(!carryWire);
    if(usedIn)
      dataReg <= tagged Invalid;
    else if(dataValid)
      dataReg <= tagged Valid dataWire;

    if(usedIn)
      usedReg <= True;
    else if(dataValid)
      usedReg <= False;

    if(specCycleDoneIn)
      suppliedReg <= False;
    else if(dataValid)
      suppliedReg <= True;
  endrule

  return tuple2(
    interface Output;
      method Action _write(t x) if(g1 && canAcceptOut);
        dataValid.send;
        dataWire <= x;

        if(enValid)
          en;
      endmethod

      method Action justFinish() if(canAcceptOut);
        dataValid.send;
        justFinishIn.send;

        if(enValid)
          en.justFinish;
      endmethod

      method canAccept = canAcceptOut;

      method isSupplied = isSuppliedOut;

      method Action specCycleDone() if(isSuppliedOut);
        specCycleDoneIn.send;
      endmethod
    endinterface,

    interface Output_;
      method t _read() if(g2 && isValid(dataOut));
        return validValue(dataOut);
      endmethod

      method isValid = isValid(dataOut);

      method canFinish = isValid(dataOut);

      method Action specCycleDone();
        usedIn.send;
      endmethod

      method isSupplied = True;

      interface OutputCarry_ carry;
        method Action connected();
          carryWire.send;
        endmethod

        method write = justFinishIn? tagged Invalid: dataIn;

        method justFinish = justFinishIn;

        method Action canAccept();
          canAcceptCarry.send;
        endmethod

        method Action isSupplied();
          isSuppliedCarry.send;
        endmethod

        method specCycleDone = specCycleDoneIn;
      endinterface
    endinterface);
endmodule

instance Connectable#(Output#(t), Output_#(t)) provisos(Bits#(t, tSz));
  module mkConnection#(Output#(t) a, Output_#(t) b)();
    rule r1;
      b.carry.connected;
      if(a.canAccept)
        b.carry.canAccept;
      if(a.isSupplied)
        b.carry.isSupplied;
    endrule

    rule r2;
      if(b.carry.write matches tagged Valid .x)
        a <= x;
    endrule

    rule r3;
      if(b.carry.justFinish)
        a.justFinish;
    endrule

    rule r4;
      if(b.carry.specCycleDone)
        a.specCycleDone;
    endrule
  endmodule
endinstance

instance Connectable#(Output_#(t), Output#(t)) provisos(Bits#(t, tSz));
  module mkConnection#(Output_#(t) a, Output#(t) b)();
    mkConnection(asIfc(b), asIfc(a));
  endmodule
endinstance

instance Sync_#(Output#(t));
  function Action _specCycleDone(Output#(t) x) = x.specCycleDone;
  function Bool _isSupplied(Output#(t) x) = x.isSupplied;
endinstance

instance Sync_#(Output_#(t));
  function Action _specCycleDone(Output_#(t) x) = x.specCycleDone;
  function Bool _isSupplied(Output_#(t) x) = x.isSupplied;
endinstance

interface OutputPulse;
  method Action _read;
  method Action justFinish();
  method Bool canAccept();
  method Bool isSupplied();
  method Action specCycleDone();
endinterface

module _OutputPulse#(Bool enValid, OutputPulse en, Bool g1, Bool g2)(Tuple2#(OutputPulse, Output_#(Bool)));
  Pulse            carryWire <- mkPulse;

  Reg#(Maybe#(Bool)) dataReg <- mkReg(tagged Invalid);
  Reg#(Bool)         usedReg <- mkReg(True);
  Reg#(Bool)     suppliedReg <- mkReg(False);

  Pulse            dataValid <- mkPulse;
  Pulse             dataWire <- mkPulse;
  Pulse         justFinishIn <- mkPulse;
  Pulse               usedIn <- mkPulse;
  Pulse      specCycleDoneIn <- mkPulse;

  Maybe#(Bool)        dataIn  = dataValid?
                                  tagged Valid dataWire:
                                  tagged Invalid;

  Pulse       canAcceptCarry <- mkPulse;
  Pulse      isSuppliedCarry <- mkPulse;

  Bool          canAcceptOut  = carryWire?
                                  canAcceptCarry:
                                  !suppliedReg && usedReg;

  Bool         isSuppliedOut  = carryWire?
                                  isSuppliedCarry:
                                  suppliedReg || dataValid;

  Maybe#(Bool)       dataOut  = case (dataReg) matches
                                  Invalid : dataIn;
                                  default : dataReg;
                                endcase;

  rule r(!carryWire);
    if(usedIn)
      dataReg <= tagged Invalid;
    else if(dataValid)
      dataReg <= tagged Valid dataWire;

    if(usedIn)
      usedReg <= True;
    else if(dataValid)
      usedReg <= False;

    if(specCycleDoneIn)
      suppliedReg <= False;
    else if(dataValid)
      suppliedReg <= True;
  endrule

  return tuple2(
    interface OutputPulse;
      method Action _read if(g1 && canAcceptOut);
        dataValid.send;
        dataWire.send;

        if(enValid)
          en;
      endmethod

      method Action justFinish() if(canAcceptOut);
        dataValid.send;
        justFinishIn.send;

        if(enValid)
          en.justFinish;
      endmethod

      method canAccept = canAcceptOut;

      method isSupplied = isSuppliedOut;

      method Action specCycleDone() if(isSuppliedOut);
        specCycleDoneIn.send;
      endmethod
    endinterface,

    interface Output_;
      method Bool _read() if(g2 && isValid(dataOut));
        return validValue(dataOut);
      endmethod

      method isValid = isValid(dataOut);

      method canFinish = isValid(dataOut);

      method Action specCycleDone();
        usedIn.send;
      endmethod

      method isSupplied = True;

      interface OutputCarry_ carry;
        method Action connected();
          carryWire.send;
        endmethod

        method write = justFinishIn? tagged Invalid: dataIn;

        method justFinish = justFinishIn;

        method Action canAccept();
          canAcceptCarry.send;
        endmethod

        method Action isSupplied();
          isSuppliedCarry.send;
        endmethod

        method specCycleDone = specCycleDoneIn;
      endinterface
    endinterface);
endmodule

instance Connectable#(OutputPulse, Output_#(Bool));
  module mkConnection#(OutputPulse a, Output_#(Bool) b)();
    rule r1;
      b.carry.connected;
      if(a.canAccept)
        b.carry.canAccept;
      if(a.isSupplied)
        b.carry.isSupplied;
    endrule

    rule r2;
      if(b.carry.write matches tagged Valid .x)
        a;
    endrule

    rule r3;
      if(b.carry.justFinish)
        a.justFinish;
    endrule

    rule r4;
      if(b.carry.specCycleDone)
        a.specCycleDone;
    endrule
  endmodule
endinstance

instance Connectable#(Output_#(Bool), OutputPulse);
  module mkConnection#(Output_#(Bool) a, OutputPulse b)();
    mkConnection(asIfc(b), asIfc(a));
  endmodule
endinstance

instance Sync_#(OutputPulse);
  function Action _specCycleDone(OutputPulse x) = x.specCycleDone;
  function Bool _isSupplied(OutputPulse x) = x.isSupplied;
endinstance

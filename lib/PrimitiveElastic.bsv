import Base::*;
import Connectable::*;
import Vector::*;

typedef function Action send() Send;

interface Output_Carry_#(type t);
  method Action connected;
  method Action validWrite(t x);
  method Bool used;
endinterface

interface Output#(type t);
  method Action _write(t x);
  method Action justFinish;
  method Bool canAccept;
  method Bool isSupplied;
  method Action specCycleDone;

  interface Output_Carry_#(t) carry;
endinterface

interface Output_#(type t);
  method t _read;
  method Bool isValid;
  method Bool canFinish;
  method Action specCycleDone;

  method Bool isSupplied();
endinterface

module _Output#(Bool enValid, OutputPulse en, Bool g1, Bool g2)(Tuple2#(Output#(t), Output_#(t))) provisos(Bits#(t, tSz));
  Pulse           carryWire <- mkPulse;
  Pulse   dataOutValidCarry <- mkPulse;
  Wire#(t) dataOutDataCarry <- mkWire;

  Maybe#(t)    dataOutCarry  = dataOutValidCarry?
                                 tagged Valid dataOutDataCarry:
                                 tagged Invalid;

  Reg#(Maybe#(t))   dataReg <- mkReg(tagged Invalid);
  Reg#(Bool)        usedReg <- mkReg(True);
  Reg#(Bool)    suppliedReg <- mkReg(False);

  Pulse           dataValid <- mkPulse;
  Wire#(t)         dataWire <- mkWire;
  Pulse              usedIn <- mkPulse;
  Pulse     specCycleDoneIn <- mkPulse;

  Maybe#(t)          dataIn  = dataValid?
                                 tagged Valid dataWire:
                                 tagged Invalid;

  Bool         canAcceptOut  = !suppliedReg && usedReg;

  Bool        isSuppliedOut  = suppliedReg || dataValid;

  Maybe#(t)         dataOut  = carryWire?
                                 dataOutCarry:
                                 case (dataReg) matches
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

        if(enValid)
          en.justFinish;
      endmethod

      method canAccept = canAcceptOut;

      method isSupplied = isSuppliedOut;

      method Action specCycleDone() if(suppliedReg || usedReg); //suppliedReg || canAcceptOut
        specCycleDoneIn.send;

        if(!suppliedReg)
        begin
          dataValid.send;

          if(enValid)
            en.justFinish;
        end
      endmethod

      interface Output_Carry_ carry;
        method Action connected = carryWire.send;

        method Action validWrite(t x);
          dataOutValidCarry.send;
          dataOutDataCarry <= x;
        endmethod

        method Bool used = usedIn;
      endinterface
    endinterface,

    interface Output_;
      method t _read if(g2 && isValid(dataOut));
        return validValue(dataOut);
      endmethod

      method isValid = isValid(dataOut);

      method canFinish = isValid(dataOut);

      method Action specCycleDone if(isValid(dataOut));
        usedIn.send;
      endmethod

      method isSupplied = True;
    endinterface);
endmodule

instance Connectable#(Output#(t), Output_#(t)) provisos(Bits#(t, tSz));
  module mkConnection#(Output#(t) a, Output_#(t) b)();
    rule r1;
      if(a.carry.used)
        b.specCycleDone;
    endrule

    rule r2;
      if(b.isValid)
        a.carry.validWrite(b);
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

  interface Output_Carry_#(Bool) carry;
endinterface

typedef Output_#(Bool) OutputPulse_;

module _OutputPulse#(Bool enValid, OutputPulse en, Bool g1, Bool g2)(Tuple2#(OutputPulse, Output_#(Bool)));
  Pulse              carryWire <- mkPulse;
  Pulse      dataOutValidCarry <- mkPulse;
  Wire#(Bool) dataOutDataCarry <- mkWire;

  Maybe#(Bool)    dataOutCarry  = dataOutValidCarry?
                                    tagged Valid dataOutDataCarry:
                                    tagged Invalid;

  Reg#(Maybe#(Bool))   dataReg <- mkReg(tagged Invalid);
  Reg#(Bool)           usedReg <- mkReg(True);
  Reg#(Bool)       suppliedReg <- mkReg(False);

  Pulse              dataValid <- mkPulse;
  Pulse               dataWire <- mkPulse;
  Pulse                 usedIn <- mkPulse;
  Pulse        specCycleDoneIn <- mkPulse;

  Maybe#(Bool)          dataIn  = dataValid?
                                    tagged Valid dataWire:
                                    tagged Invalid;

  Bool            canAcceptOut  = !suppliedReg && usedReg;

  Bool           isSuppliedOut  = suppliedReg || dataValid;

  Maybe#(Bool)         dataOut  = carryWire?
                                    dataOutCarry:
                                    case (dataReg) matches
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

        if(enValid)
          en.justFinish;
      endmethod

      method canAccept = canAcceptOut;

      method isSupplied = isSuppliedOut;

      method Action specCycleDone() if(suppliedReg || usedReg); //suppliedReg || canAcceptOut
        specCycleDoneIn.send;

        if(!suppliedReg)
        begin
          dataValid.send;

          if(enValid)
            en.justFinish;
        end
      endmethod

      interface Output_Carry_ carry;
        method Action connected = carryWire.send;

        method Action validWrite(Bool x);
          dataOutValidCarry.send;
          dataOutDataCarry <= x;
        endmethod

        method Bool used = usedIn;
      endinterface
    endinterface,

    interface Output_;
      method Bool _read if(g2 && isValid(dataOut));
        return validValue(dataOut);
      endmethod

      method isValid = isValid(dataOut);

      method canFinish = isValid(dataOut);

      method Action specCycleDone if(isValid(dataOut));
        usedIn.send;
      endmethod

      method isSupplied = True;
    endinterface);
endmodule

instance Connectable#(OutputPulse, Output_#(Bool));
  module mkConnection#(OutputPulse a, Output_#(Bool) b)();
    rule r1;
      if(a.carry.used)
        b.specCycleDone;
    endrule

    rule r2;
      if(b.isValid)
        a.carry.validWrite(b);
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

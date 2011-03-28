import Base::*;
import Connectable::*;
import Vector::*;

interface Output_#(type t);
  method Action _write(t x);
  method Action justFinish;
  method Bool canAccept;
  method Bool isSupplied;
  method Action specCycleInputDone;
  method Action specCycleOutputDone;

  method Action connected;
  method Action validWrite(t x);
  method Bool used;
endinterface

interface Output#(type t);
  method t _read;
  method Bool isValid;
  method Bool canFinish;
  method Action specCycleInputDone;
  method Action specCycleOutputDone;

  method Bool isSupplied;

  method Action connected;
  method Action specCycleDoneCarry;
  method t rawRead;
endinterface

instance Connectable#(Output_#(t), Output#(t)) provisos(Bits#(t, tSz));
  module mkConnection#(Output_#(t) a, Output#(t) b)();
    rule r1;
      a.connected;
      b.connected;
    endrule

    rule r2;
      if(a.used)
        b.specCycleDoneCarry;
    endrule

    rule r3;
      if(b.isValid)
        a.validWrite(b.rawRead);
    endrule
  endmodule
endinstance

instance Connectable#(Output#(t), Output_#(t)) provisos(Bits#(t, tSz));
  module mkConnection#(Output#(t) a, Output_#(t) b)();
    mkConnection(asIfc(b), asIfc(a));
  endmodule
endinstance

instance Sync_#(Output#(t));
  function Action _specCycleInputDone(Output#(t) x) = x.specCycleInputDone;
  function Action _specCycleOutputDone(Output#(t) x) = x.specCycleOutputDone;
  function Bool _isSupplied(Output#(t) x) = x.isSupplied;
endinstance

instance Sync_#(Output_#(t));
  function Action _specCycleInputDone(Output_#(t) x) = x.specCycleInputDone;
  function Action _specCycleOutputDone(Output_#(t) x) = x.specCycleOutputDone;
  function Bool _isSupplied(Output_#(t) x) = x.isSupplied;
endinstance

module _Output#(Bool enValid, OutputPulse_ en, Bool g1, Bool g2)(Tuple2#(Output_#(t), Output#(t))) provisos(Bits#(t, tSz));
  Pulse           carryWire <- mkPulse;
  Pulse          carry2Wire <- mkPulse;
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
  Pulse     specCycleDoneIn <- mkPulse;
  Pulse        usedInDirect <- mkPulse;
  Pulse         usedInCarry <- mkPulse;
  Bool               usedIn  = carry2Wire?
                                 usedInCarry:
                                 usedInDirect;

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
    else if(dataIn matches tagged Valid .d)
      dataReg <= tagged Valid d;

    if(usedIn)
      usedReg <= True;
    else if(dataIn matches tagged Valid .*)
      usedReg <= False;

    if(specCycleDoneIn)
      suppliedReg <= False;
    else if(dataIn matches tagged Valid .*)
      suppliedReg <= True;
  endrule

  return tuple2(
    interface Output_;
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

      method Action specCycleInputDone = noAction;

      method Action specCycleOutputDone if(isSuppliedOut);
        specCycleDoneIn.send;
      endmethod

      method Action connected = carryWire.send;

      method Action validWrite(t x);
        dataOutValidCarry.send;
        dataOutDataCarry <= x;
      endmethod

      method Bool used = usedIn;
    endinterface,

    interface Output;
      method t _read if(g2 && isValid(dataOut));
        return validValue(dataOut);
      endmethod

      method isValid = isValid(dataOut);

      method canFinish = isValid(dataOut);

      method Action specCycleInputDone if(isValid(dataOut));
        usedInDirect.send;
      endmethod

      method Action specCycleOutputDone = noAction;

      method isSupplied = True;

      method Action connected = carry2Wire.send;

      method Action specCycleDoneCarry = usedInCarry.send;

      method t rawRead = validValue(dataOut);
    endinterface);
endmodule

interface OutputPulse_;
  method Action _read;
  method Action justFinish();
  method Bool canAccept();
  method Bool isSupplied();
  method Action specCycleInputDone();
  method Action specCycleOutputDone();

  method Action connected;
  method Action validWrite(Bool x);
  method Bool used;
endinterface

typedef Output#(Bool) OutputPulse;

instance Connectable#(OutputPulse_, OutputPulse);
  module mkConnection#(OutputPulse_ a, OutputPulse b)();
    rule r1;
      a.connected;
      b.connected;
    endrule

    rule r2;
      if(a.used)
        b.specCycleDoneCarry;
    endrule

    rule r3;
      if(b.isValid)
        a.validWrite(b.rawRead);
    endrule
  endmodule
endinstance

instance Connectable#(OutputPulse, OutputPulse_);
  module mkConnection#(OutputPulse a, OutputPulse_ b)();
    mkConnection(asIfc(b), asIfc(a));
  endmodule
endinstance

instance Sync_#(OutputPulse_);
  function Action _specCycleInputDone(OutputPulse_ x) = x.specCycleInputDone;
  function Action _specCycleOutputDone(OutputPulse_ x) = x.specCycleOutputDone;
  function Bool _isSupplied(OutputPulse_ x) = x.isSupplied;
endinstance

module _OutputPulse#(Bool enValid, OutputPulse_ en, Bool g1, Bool g2)(Tuple2#(OutputPulse_, OutputPulse));
  Pulse              carryWire <- mkPulse;
  Pulse             carry2Wire <- mkPulse;
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
  Pulse        specCycleDoneIn <- mkPulse;
  Pulse           usedInDirect <- mkPulse;
  Pulse            usedInCarry <- mkPulse;

  Bool                  usedIn  = carry2Wire?
                                    usedInCarry:
                                    usedInDirect;

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
    else if(dataIn matches tagged Valid .d)
      dataReg <= tagged Valid d;

    if(usedIn)
      usedReg <= True;
    else if(dataIn matches tagged Valid .*)
      usedReg <= False;

    if(specCycleDoneIn)
      suppliedReg <= False;
    else if(dataIn matches tagged Valid .*)
      suppliedReg <= True;
  endrule

  return tuple2(
    interface OutputPulse_;
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

      method Action specCycleInputDone = noAction;

      method Action specCycleOutputDone() if(isSuppliedOut);
        specCycleDoneIn.send;
      endmethod

      method Action connected = carryWire.send;

      method Action validWrite(Bool x);
        dataOutValidCarry.send;
        dataOutDataCarry <= x;
      endmethod

      method Bool used = usedIn;
    endinterface,

    interface OutputPulse;
      method Bool _read if(g2 && isValid(dataOut));
        return validValue(dataOut);
      endmethod

      method isValid = isValid(dataOut);

      method canFinish = isValid(dataOut);

      method Action specCycleInputDone if(isValid(dataOut));
        usedInDirect.send;
      endmethod

      method Action specCycleOutputDone = noAction;

      method isSupplied = True;

      method Action connected = carry2Wire.send;

      method Action specCycleDoneCarry = usedInCarry.send;

      method Bool rawRead = validValue(dataOut);
    endinterface);
endmodule

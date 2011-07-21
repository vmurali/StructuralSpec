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

  method Bool isAvailable;

  method Action connected;
  method Action carryWrite(t x);
  method Bool carryInputUsed;
endinterface

interface Output#(type t);
  method t _read;
  method Bool isValid;
  method Bool isAvailable;
  method Action specCycleInputDone;
  method Action specCycleOutputDone;

  method Bool isSupplied;

  method Action connected;
  method t carryRead;
  method Action carryInputUsed;
endinterface

instance Connectable#(Output_#(t), Output#(t)) provisos(Bits#(t, tSz));
  module mkConnection#(Output_#(t) a, Output#(t) b)();
    rule r1;
      a.connected;
      b.connected;
    endrule

    rule r2;
      if(a.carryInputUsed)
        b.carryInputUsed;
    endrule

    rule r3;
      if(b.isValid)
        a.carryWrite(b.carryRead);
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
  function Bool _isAvailable(Output#(t) x) = x.isAvailable;
endinstance

instance Sync_#(Output_#(t));
  function Action _specCycleInputDone(Output_#(t) x) = x.specCycleInputDone;
  function Action _specCycleOutputDone(Output_#(t) x) = x.specCycleOutputDone;
  function Bool _isSupplied(Output_#(t) x) = x.isSupplied;
  function Bool _isAvailable(Output_#(t) x) = x.isAvailable;
endinstance

module _Output#(Bool enValid, OutputPulse_ en, Bool g1, Bool g2)(Tuple2#(Output_#(t), Output#(t))) provisos(Bits#(t, tSz));
  Pulse     connectedWriter <- mkPulse;
  Pulse     connectedReader <- mkPulse;

  Pulse          validCarry <- mkPulse;
  Wire#(t)        dataCarry <- mkWire;

  Maybe#(t)    dataOutCarry  = validCarry?
                                 tagged Valid dataCarry:
                                 tagged Invalid;

  Reg#(Maybe#(t))   dataReg <- mkReg(tagged Invalid);
  Reg#(Bool)    suppliedReg <- mkReg(False);

  Pulse           dataValid <- mkPulse;
  Wire#(t)         dataWire <- mkWire;

  Pulse          outputDone <- mkPulse;

  Pulse     inputUsedDirect <- mkPulse;
  Pulse     _inputUsedCarry <- mkPulse;
  Bool            inputUsed  = connectedReader?
                                 _inputUsedCarry:
                                 inputUsedDirect;

  Bool           _canAccept  = !suppliedReg && !isValid(dataReg);

  Maybe#(t)          dataIn  = dataValid && _canAccept?
                                 tagged Valid dataWire:
                                 tagged Invalid;

  Bool          _isSupplied  = suppliedReg || isValid(dataIn);

  Maybe#(t)         dataOut  = connectedWriter?
                                 dataOutCarry:
                                 case (dataIn) matches
                                   tagged Valid .* : dataIn;
                                   default         : dataReg;
                                 endcase;

  rule r(!connectedWriter);
    if(inputUsed)
      dataReg <= tagged Invalid;
    else if(dataIn matches tagged Valid .*)
      dataReg <= dataIn;

    if(outputDone)
      suppliedReg <= False;
    else if(dataIn matches tagged Valid .*)
      suppliedReg <= True;
  endrule

  return tuple2(
    interface Output_;
      method Action _write(t x) if(g1);
        dataValid.send;
        dataWire <= x;

        if(enValid)
          en;
      endmethod

      method Action justFinish;
        dataValid.send;

        if(enValid)
          en.justFinish;
      endmethod

      method canAccept = _canAccept;

      method isSupplied = _isSupplied;

      method Action specCycleInputDone = noAction;

      method Action specCycleOutputDone if(connectedWriter || _isSupplied);
        outputDone.send;
      endmethod

      method isAvailable = True;

      method Action connected = connectedWriter.send;

      method Action carryWrite(t x);
        validCarry.send;
        dataCarry <= x;
      endmethod

      method Bool carryInputUsed = inputUsed;
    endinterface,

    interface Output;
      method t _read if(g2 && isValid(dataOut));
        return validValue(dataOut);
      endmethod

      method isValid = isValid(dataOut);

      method Action specCycleInputDone if(connectedReader || isValid(dataOut));
        inputUsedDirect.send;
      endmethod

      method Action specCycleOutputDone = noAction;

      method isSupplied = True;

      method isAvailable = isValid(dataOut);

      method Action connected = connectedReader.send;

      method Action carryInputUsed = _inputUsedCarry.send;

      method t carryRead = validValue(dataOut);
    endinterface);
endmodule

interface OutputPulse_;
  method Action _read;
  method Action justFinish;
  method Bool canAccept;
  method Bool isSupplied;
  method Action specCycleInputDone;
  method Action specCycleOutputDone;

  method Bool isAvailable;

  method Action connected;
  method Action carryWrite(Bool x);
  method Bool carryInputUsed;
endinterface

typedef Output#(Bool) OutputPulse;

instance Connectable#(OutputPulse_, OutputPulse);
  module mkConnection#(OutputPulse_ a, OutputPulse b)();
    rule r1;
      a.connected;
      b.connected;
    endrule

    rule r2;
      if(a.carryInputUsed)
        b.carryInputUsed;
    endrule

    rule r3;
      if(b.isValid)
        a.carryWrite(b.carryRead);
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
  function Bool _isAvailable(OutputPulse_ x) = x.isAvailable;
endinstance

module _OutputPulse#(Bool enValid, OutputPulse_ en, Bool g1, Bool g2)(Tuple2#(OutputPulse_, OutputPulse));
  Pulse     connectedWriter <- mkPulse;
  Pulse     connectedReader <- mkPulse;

  Pulse          validCarry <- mkPulse;
  Wire#(Bool)     dataCarry <- mkWire;

  Maybe#(Bool) dataOutCarry  = validCarry?
                                 tagged Valid dataCarry:
                                 tagged Invalid;

  Reg#(Maybe#(Bool))dataReg <- mkReg(tagged Invalid);
  Reg#(Bool)    suppliedReg <- mkReg(False);

  Pulse           dataValid <- mkPulse;
  Pulse            dataWire <- mkPulse;

  Pulse          outputDone <- mkPulse;

  Pulse     inputUsedDirect <- mkPulse;
  Pulse     _inputUsedCarry <- mkPulse;
  Bool            inputUsed  = connectedReader?
                                 _inputUsedCarry:
                                 inputUsedDirect;

  Bool           _canAccept  = !suppliedReg && !isValid(dataReg);

  Maybe#(Bool)       dataIn  = dataValid && _canAccept?
                                 tagged Valid dataWire:
                                 tagged Invalid;

  Bool          _isSupplied  = suppliedReg || isValid(dataIn);

  Maybe#(Bool)      dataOut  = connectedWriter?
                                 dataOutCarry:
                                 case (dataIn) matches
                                   tagged Valid .* : dataIn;
                                   default         : dataReg;
                                 endcase;

  rule r(!connectedWriter);
    if(inputUsed)
      dataReg <= tagged Invalid;
    else if(dataIn matches tagged Valid .*)
      dataReg <= dataIn;

    if(outputDone)
      suppliedReg <= False;
    else if(dataIn matches tagged Valid .*)
      suppliedReg <= True;
  endrule

  return tuple2(
    interface OutputPulse_;
      method Action _read if(g1);
        dataValid.send;
        dataWire.send;

        if(enValid)
          en;
      endmethod

      method Action justFinish;
        dataValid.send;

        if(enValid)
          en.justFinish;
      endmethod

      method canAccept = _canAccept;

      method isSupplied = _isSupplied;

      method Action specCycleInputDone = noAction;

      method Action specCycleOutputDone if(connectedWriter || _isSupplied);
        outputDone.send;
      endmethod

      method isAvailable = True;

      method Action connected = connectedWriter.send;

      method Action carryWrite(Bool x);
        validCarry.send;
        dataCarry <= x;
      endmethod

      method Bool carryInputUsed = inputUsed;
    endinterface,

    interface OutputPulse;
      method Bool _read if(g2 && isValid(dataOut));
        return validValue(dataOut);
      endmethod

      method isValid = isValid(dataOut);

      method Action specCycleInputDone if(connectedReader || isValid(dataOut));
        inputUsedDirect.send;
      endmethod

      method Action specCycleOutputDone = noAction;

      method isSupplied = True;

      method isAvailable = isValid(dataOut);

      method Action connected = connectedReader.send;

      method Action carryInputUsed = _inputUsedCarry.send;

      method Bool carryRead = validValue(dataOut);
    endinterface);
endmodule

import Base::*;
import Connectable::*;

interface Output_#(type t);
  method Action _write(t x);
  method Action connected;
  method Action carryWrite(t x);
endinterface

interface Output#(type t);
  method t _read();
endinterface

instance Sync_#(Output#(t));
  function Action _specCycleInputDone(Output#(t) x) = noAction;
  function Action _specCycleOutputDone(Output#(t) x) = noAction;
  function Bool _isSupplied(Output#(t) x) = True;
endinstance

instance Sync_#(Output_#(t));
  function Action _specCycleInputDone(Output_#(t) x) = noAction;
  function Action _specCycleOutputDone(Output_#(t) x) = noAction;
  function Bool _isSupplied(Output_#(t) x) = True;
endinstance

instance Connectable#(Output_#(t), Output#(t));
  module mkConnection#(Output_#(t) a, Output#(t) b)();
    rule c1;
      a.connected;
    endrule

    rule c2;
      a.carryWrite(b);
    endrule
  endmodule
endinstance

instance Connectable#(Output#(t), Output_#(t));
  module mkConnection#(Output#(t) a, Output_#(t) b)();
    rule c3;
      b.connected;
    endrule

    rule c4;
      b.carryWrite(a);
    endrule
  endmodule
endinstance

module _Output#(Bool enValid, OutputPulse_ en, Bool g1, Bool g2)(Tuple2#(Output_#(t), Output#(t))) provisos(Bits#(t, tSz));
  Wire#(t)      w <- mkWire;
  Pulse     carry <- mkPulse;
  Wire#(t) carryW <- mkWire;

  t       dataOut  = carry?
                       carryW:
                       w;

  return tuple2(
    interface Output_;
      method Action _write(t x) if(g1);
        w <= x;
        if(enValid)
          en;
      endmethod

      method Action connected = carry.send;

      method Action carryWrite(t x);
        carryW <= x;
      endmethod
    endinterface,
    interface Output;
      method t _read() if(g2);
        return dataOut;
      endmethod
    endinterface);
endmodule

interface OutputPulse_;
  method Action _read();
  method Action connected;
  method Action carryWrite;
endinterface

instance Sync_#(OutputPulse_);
  function Action _specCycleInputDone(OutputPulse_ x) = noAction;
  function Action _specCycleOutputDone(OutputPulse_ x) = noAction;
  function Bool _isSupplied(OutputPulse_ x) = True;
endinstance

typedef Output#(Bool) OutputPulse;

instance Connectable#(OutputPulse_, OutputPulse);
  module mkConnection#(OutputPulse_ a, OutputPulse b)();
    rule c5;
      a.connected;
    endrule

    rule c6;
      if(b)
        a.carryWrite;
    endrule
  endmodule
endinstance

instance Connectable#(OutputPulse, OutputPulse_);
  module mkConnection#(OutputPulse a, OutputPulse_ b)();
    rule c7;
      b.connected;
    endrule

    rule c8;
      if(a)
        b.carryWrite;
    endrule
  endmodule
endinstance

module _OutputPulse#(Bool enValid, OutputPulse_ en, Bool g1, Bool g2)(Tuple2#(OutputPulse_, OutputPulse));
  Pulse      w <- mkPulse;
  Pulse  carry <- mkPulse;
  Pulse carryW <- mkPulse;

  Bool dataOut  = carry?
                    carryW:
                    w;

  return tuple2(
    interface OutputPulse_;
      method Action _read() if(g1);
        w.send;
        if(enValid)
          en;
      endmethod

      method Action connected = carry.send;

      method Action carryWrite();
        carryW.send;
      endmethod
    endinterface,
    interface OutputPulse;
      method Bool _read() if(g2);
        return dataOut;
      endmethod
    endinterface);
endmodule

import Base::*;
import Connectable::*;
import Vector::*;

typedef function Action send() Send;

typeclass Sync_#(type t);
  function Action _hasBeenUsed(t x);
  function Bool _isSupplied(t x);
endtypeclass

instance Sync_#(Vector#(num, t)) provisos(Sync_#(t));
  function Action _hasBeenUsed(Vector#(num, t) xs) = joinActions(map(_hasBeenUsed, xs));
  function Bool _isSupplied(Vector#(num, t) xs) = foldl(\&& , True, map(_isSupplied, xs));
endinstance

interface Normal_#(type t);
  method Maybe#(t) dataIn;
  interface Send canAccept;
  interface Send isOutputSupplied;
endinterface

interface Reverse_#(type t);
  method Bool doneIn;
  method Action dataOut(t x);
  interface Send canFinish;
endinterface

module connect_#(Reverse_#(t) b, Normal_#(t) a)() provisos(Bits#(t, tSz));
  Reg#(Maybe#(t)) dataReg <- mkReg(Invalid);
  Reg#(Bool)      doneReg <- mkReg(True);

  rule r;
    if(b.doneIn)
      dataReg <= Nothing;
    else if(a.dataIn matches tagged Just .x)
      dataReg <= tagged Just x;

    Maybe#(t) dataOut =
      case (dataReg) matches
        tagged Just .x : return tagged Just x;
        default        : return a.dataIn;
      endcase;

    if(dataOut matches tagged Just .x)
      b.dataOut(x);

    if(b.doneIn)
      doneReg <= True;
    else if(a.dataIn matches tagged Just .*)
      doneReg <= False;

    if(doneReg)
      a.canAccept;

    if(doneReg || isValid(dataOut))
      a.isOutputSupplied;

    if(dataOut matches tagged Nothing)
      b.canFinish;
  endrule
endmodule

interface Output#(type t);
  method Action _write(t x);
  method Action justFinish();
  method Bool canAccept();
  method Bool isSupplied();
  method Action hasBeenUsed();

  interface Reverse_#(t) conn;
endinterface

interface Output_#(type t);
  method t _read();
  method Bool isValid();
  method Bool canFinish();
  method Action hasBeenUsed();
  method Bool isSupplied();

  interface Normal_#(t) conn;
endinterface

module _Output#(Bool enValid, Enable en, Bool g1, Bool g2)(Tuple2#(Output#(t), Output_#(t))) provisos(Bits#(t, tSz));
  Wire#(t)        dataIn_ <- mkWire;
  Pulse      dataInValid_ <- mkPulse;
  Pulse        canAccept_ <- mkPulse;
  Pulse isOutputSupplied_ <- mkPulse;

  Wire#(t)       dataOut_ <- mkWire;
  Pulse          isValid_ <- mkPulse;
  Pulse        canFinish_ <- mkPulse;
  Pulse             done_ <- mkPulse;

  function Action canAcceptFn();
  action
    canAccept_.send;
  endaction
  endfunction

  function Action isOutputSuppliedFn();
  action
    isOutputSupplied_.send;
  endaction
  endfunction

  function Action canFinishFn();
  action
    canFinish_.send;
  endaction
  endfunction

  return tuple2(
    interface Output;
      method Action _write(t x) if(g1 && canAccept_);
        dataIn_ <= x;
        dataInValid_.send; 
    
        if(enValid)
          en;
      endmethod
    
      method Action justFinish() if(canAccept_);
        dataInValid_.send;
    
        if(enValid)
          en.justFinish;
      endmethod
    
      method Bool canAccept();
        return canAccept_;
      endmethod
    
      method Bool isSupplied();
        return isOutputSupplied_;
      endmethod

      method Action hasBeenUsed();
      endmethod

      interface Reverse_ conn;
        method Bool doneIn();
          return done_;
        endmethod

        method Action dataOut(t x);
          isValid_.send;
          dataOut_ <= x;
        endmethod

        interface canFinish = canFinishFn;
      endinterface
    endinterface,

    interface Output_;
      method t _read() if(g2 && isValid_);
        return dataOut_;
      endmethod

      method Bool isValid();
        return isValid_;
      endmethod

      method Bool canFinish();
        return canFinish_;
      endmethod

      method Action hasBeenUsed();
        done_.send;
      endmethod

      method Bool isSupplied();
        return True;
      endmethod

      interface Normal_ conn;
        method Maybe#(t) dataIn();
          if(dataInValid_)
            return tagged Just dataIn_;
          else
            return Nothing;
        endmethod
    
        interface canAccept = canAcceptFn;
        interface isOutputSupplied = isOutputSuppliedFn;
      endinterface
    endinterface);
endmodule

instance Connectable#(Output#(t), Output_#(t)) provisos(Bits#(t, tSz));
  module mkConnection#(Output#(t) a, Output_#(t) b)();
    connect_(asIfc(a).conn, asIfc(b).conn);
  endmodule
endinstance

instance Connectable#(Output_#(t), Output#(t)) provisos(Bits#(t, tSz));
  module mkConnection#(Output_#(t) a, Output#(t) b)();
    connect_(asIfc(b).conn, asIfc(a).conn);
  endmodule
endinstance

instance Sync_#(Output#(t));
  function Action _hasBeenUsed(Output#(t) x) = x.hasBeenUsed;
  function Bool _isSupplied(Output#(t) x) = x.isSupplied;
endinstance

instance Sync_#(Output_#(t));
  function Action _hasBeenUsed(Output_#(t) x) = x.hasBeenUsed;
  function Bool _isSupplied(Output_#(t) x) = x.isSupplied;
endinstance

interface Enable;
  method Action _read();
  method Action justFinish();
  method Bool canAccept();
  method Bool isSupplied();
  method Action hasBeenUsed();

  interface Reverse_#(Bool) conn;
endinterface

interface Enable_;
  method Bool _read();
  method Bool isValid();
  method Bool canFinish();
  method Action hasBeenUsed();
  method Bool isSupplied();

  interface Normal_#(Bool) conn;
endinterface

module _Enable#(Bool enValid, Enable en, Bool g1, Bool g2)(Tuple2#(Enable, Enable_));
  Pulse           dataIn_ <- mkPulse;
  Pulse      dataInValid_ <- mkPulse;
  Pulse        canAccept_ <- mkPulse;
  Pulse isOutputSupplied_ <- mkPulse;

  Pulse          dataOut_ <- mkPulse;
  Pulse          isValid_ <- mkPulse;
  Pulse        canFinish_ <- mkPulse;
  Pulse             done_ <- mkPulse;

  function Action canAcceptFn();
  action
    canAccept_.send;
  endaction
  endfunction

  function Action isOutputSuppliedFn();
  action
    isOutputSupplied_.send;
  endaction
  endfunction

  function Action canFinishFn();
  action
    canFinish_.send;
  endaction
  endfunction

  return tuple2(
    interface Enable;
      method Action _read() if(g1 && canAccept_);
        dataIn_.send;
        dataInValid_.send; 
    
        if(enValid)
          en;
      endmethod
    
      method Action justFinish() if(canAccept_);
        dataInValid_.send;
    
        if(enValid)
          en.justFinish;
      endmethod
    
      method Bool canAccept();
        return canAccept_;
      endmethod
    
      method Bool isSupplied();
        return isOutputSupplied_;
      endmethod

      method Action hasBeenUsed();
      endmethod

      interface Reverse_ conn;
        method Bool doneIn();
          return done_;
        endmethod

        method Action dataOut(Bool x);
          isValid_.send;
          if(x)
            dataOut_.send;
        endmethod

        interface canFinish = canFinishFn;
      endinterface
    endinterface,

    interface Enable_;
      method Bool _read() if(g2 && isValid_);
        return dataOut_;
      endmethod

      method Bool isValid();
        return isValid_;
      endmethod

      method Bool canFinish();
        return canFinish_;
      endmethod

      method Action hasBeenUsed();
        done_.send;
      endmethod

      method Bool isSupplied();
        return True;
      endmethod

      interface Normal_ conn;
        method Maybe#(Bool) dataIn();
          if(dataInValid_)
            return tagged Just dataIn_;
          else
            return Nothing;
        endmethod
    
        interface canAccept = canAcceptFn;
        interface isOutputSupplied = isOutputSuppliedFn;
      endinterface
    endinterface);
endmodule

instance Connectable#(Enable, Enable_);
  module mkConnection#(Enable a, Enable_ b)();
    connect_(asIfc(a).conn, asIfc(b).conn);
  endmodule
endinstance

instance Connectable#(Enable_, Enable);
  module mkConnection#(Enable_ a, Enable b)();
    connect_(asIfc(b).conn, asIfc(a).conn);
  endmodule
endinstance

instance Sync_#(Enable);
  function Action _hasBeenUsed(Enable x) = x.hasBeenUsed;
  function Bool _isSupplied(Enable x) = x.isSupplied;
endinstance

instance Sync_#(Enable_);
  function Action _hasBeenUsed(Enable_ x) = x.hasBeenUsed;
  function Bool _isSupplied(Enable_ x) = x.isSupplied;
endinstance


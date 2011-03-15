import Base::*;
import Connectable::*;

typedef function Action send() Send;

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
  method Bool canAccept;
  method Bool isOutputSupplied;

  interface Reverse_#(t) conn;
endinterface

interface Output_#(type t);
  method t _read();
  method Bool isValid();
  method Bool canFinish();
  method Action done();

  interface Normal_#(t) conn;
endinterface

module _Output#(Bool enValid, Enable en, Bool g1, Bool g2)(Tuple2#(Output#(t), Output_#(t))) provisos(Bits#(t, tSz));
  BaseWire#(t)        dataIn_ <- mkBaseWire;
  BasePulse      dataInValid_ <- mkBasePulse;
  BasePulse        canAccept_ <- mkBasePulse;
  BasePulse isOutputSupplied_ <- mkBasePulse;

  BaseWire#(t)       dataOut_ <- mkBaseWire;
  BasePulse          isValid_ <- mkBasePulse;
  BasePulse        canFinish_ <- mkBasePulse;
  BasePulse             done_ <- mkBasePulse;

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
    
      method Bool canAccept;
        return canAccept_;
      endmethod
    
      method Bool isOutputSupplied;
        return isOutputSupplied_;
      endmethod

      interface Reverse_ conn;
        method Bool doneIn;
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

      method Action done();
        done_.send;
      endmethod

      interface Normal_ conn;
        method Maybe#(t) dataIn;
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

interface Enable;
  method Action _read;
  method Action justFinish();
  method Bool canAccept;
  method Bool isOutputSupplied;

  interface Normal_#(Bool) conn;
endinterface

/*
interface Enable_;
  method Bool _read();
  method Bool isValid();
  method Bool canFinish();
  method Action done();

  interface Reverse_#(Bool) conn;
endinterface

module _Enable#(Bool enValid, Enable en, Bool g1, Bool g2)(Tuple2#(Enable, Enable_));
  BasePulse           dataIn_ <- mkBasePulse;
  BasePulse      dataInValid_ <- mkBasePulse;
  BasePulse        canAccept_ <- mkBasePulse;
  BasePulse isOutputSupplied_ <- mkBasePulse;

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

  method Action _read() if(g1 && canAccept_);
    dataIn_.send;
    dataInValid_.send; 
  endmethod

  method Action justFinish() if(canAccept_);
    dataInValid_.send;
  endmethod

  method Bool canAccept;
    return canAccept_;
  endmethod

  method Bool isOutputSupplied;
    return isOutputSupplied_;
  endmethod

  interface Normal_ conn;
    method Maybe#(Bool) dataIn;
      if(dataInValid_)
        return tagged Just dataIn_;
      else
        return Nothing;
    endmethod

    interface canAccept = canAcceptFn;
    interface isOutputSupplied = isOutputSuppliedFn;
  endinterface
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
*/

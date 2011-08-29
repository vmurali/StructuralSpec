import Base::*;
import Connectable::*;
import Vector::*;

(* always_ready *)
interface Output_#(type t);
  (* enable = "WRITE_ENQ", prefix = "" *) method Action write((* port = "WRITE" *)t x);
  (* result = "WRITE_CONSUMED_BEFORE" *) method Bool doneBefore;
  (* result = "WRITE_NOT_FULL" *) method Bool notFull;
  (* result = "WRITE_CONSUMED" *) method Bool done;
  (* enable = "WRITE_RESET" *) method Action reset;
endinterface

(* always_ready *)
interface Output#(type t);
  (* result = "READ" *) method t _read;
  (* result = "READ_NOT_EMPTY" *) method Bool notEmpty;
  (* enable = "READ_DEQ" *) method Action deq;
endinterface

instance Connectable#(Output_#(t), Output#(t));
  module mkConnection#(Output_#(t) a, Output#(t) b)();
    rule _safe1;
      a.write(b);
    endrule
  endmodule
endinstance

instance Connectable#(Output#(t), Output_#(t));
  module mkConnection#(Output#(t) a, Output_#(t) b)();
    mkConnection(asIfc(b), asIfc(a));
  endmodule
endinstance

module _Output#(Bool g1, Bool g2)(Tuple2#(Output_#(t), Output#(t))) provisos(Bits#(t, tSz));
  RegNormal#(t)         data <- mkRegUNormal;
  WireNormal#(t)       dataW <- mkWireNormal;
  PulseNormal            enq <- mkPulseNormal;
  PulseNormal           deqL <- mkPulseNormal;
  RegNormal#(Bool)     valid <- mkRegNormal(False);
  RegNormal#(Bool) doneR <- mkRegNormal(False);
  PulseNormal         resetW <- mkPulseNormal;

  rule a;
    if(resetW)
      doneR <= False;
    else if(enq)
      doneR <= True;

    if(deqL)
      valid <= False;
    else if(enq)
      valid <= True;
  endrule

  return tuple2(
    interface Output_;
      method Action write(t x);// if(!valid && !doneR);
        dataW.write(x);
        data <= x;
        enq.send;
      endmethod
      method Bool doneBefore = doneR;
      method Bool notFull = !valid;
      method Bool done = enq || doneR;
      method Action reset = resetW.send;
    endinterface,

    interface Output;
      method t _read;// if(valid || enq);
        return enq? dataW: data;
      endmethod
      method Bool notEmpty = (valid || enq);
      method Action deq;// if(valid || enq);
        deqL.send;
      endmethod
    endinterface);
endmodule

(* always_ready *)
interface OutputPulse_;
  (* enable = "WRITE" *) method Action _read;
  (* enable = "WRITE_ENQ" *) method Action enq;
  (* result = "WRITE_CONSUMED_BEFORE" *) method Bool doneBefore;
  (* result = "WRITE_NOT_FULL" *) method Bool notFull;
  (* result = "WRITE_CONSUMED" *) method Bool done;
  (* enable = "WRITE_RESET" *) method Action reset;
endinterface

typedef Output#(Bool) OutputPulse;

instance Connectable#(OutputPulse_, OutputPulse);
  module mkConnection#(OutputPulse_ a, OutputPulse b)();
    rule _safe1;
      a.enq;
      if(b)
        a;
    endrule
  endmodule
endinstance

instance Connectable#(OutputPulse, OutputPulse_);
  module mkConnection#(OutputPulse a, OutputPulse_ b)();
    mkConnection(asIfc(b), asIfc(a));
  endmodule
endinstance

module _OutputPulse#(Bool g1, Bool g2)(Tuple2#(OutputPulse_, OutputPulse));
  RegNormal#(Bool)      data <- mkRegUNormal;
  PulseNormal          dataW <- mkPulseNormal;
  PulseNormal           enqW <- mkPulseNormal;
  PulseNormal           deqL <- mkPulseNormal;
  RegNormal#(Bool)     valid <- mkRegNormal(False);
  RegNormal#(Bool) doneR <- mkRegNormal(False);
  PulseNormal         resetW <- mkPulseNormal;

  rule a;
    if(enqW)
      data <= dataW;

    if(resetW)
      doneR <= False;
    else if(enqW)
      doneR <= True;

    if(deqL)
      valid <= False;
    else if(enqW)
      valid <= True;
  endrule

  return tuple2(
    interface OutputPulse_;
      method Action _read;// if(!valid && !doneR);
        dataW.send;
      endmethod

      method Action enq;// if(!valid && !doneR);
        enqW.send;
      endmethod

      method Bool doneBefore = doneR;
      method Bool notFull = !valid;
      method Bool done = enqW || doneR;
      method Action reset = resetW.send;
    endinterface,

    interface OutputPulse;
      method Bool _read;// if(valid || enqW);
        return enqW? dataW: data;
      endmethod
      method Bool notEmpty = (valid || enqW);
      method Action deq;// if(valid || enqW);
        deqL.send;
      endmethod
    endinterface);
endmodule

(* always_ready *)
interface ConditionalOutput_#(type t);
  (* enable = "WRITE_ENQ", prefix = "" *) method Action write((* port = "WRITE" *)t x);
  (* result = "WRITE_CONSUMED_BEFORE" *) method Bool doneBefore;
  (* result = "WRITE_NOT_FULL" *) method Bool notFull;
  (* result = "WRITE_CONSUMED" *) method Bool done;
  (* enable = "WRITE_RESET" *) method Action reset;
  (* enable = "EN_WRITE_ENQ", prefix = "" *) method Action en((* port = "EN_WRITE" *)Bool x);
  (* result = "EN_WRITE_CONSUMED_BEFORE" *) method Bool enConsumedBefore;
  (* result = "EN_WRITE_NOT_FULL" *) method Bool enNotFull;
  (* result = "EN_WRITE_CONSUMED" *) method Bool enConsumed;
  (* enable = "EN_WRITE_RESET" *) method Action enReset;
endinterface

(* always_ready *)
interface ConditionalOutput#(type t);
  (* result = "READ" *) method t _read;
  (* result = "READ_NOT_EMPTY" *) method Bool notEmpty;
  (* enable = "READ_DEQ" *) method Action deq;
  (* result = "EN_READ" *) method Bool en;
  (* result = "EN_READ_NOT_EMPTY" *) method Bool enNotEmpty;
  (* enable = "EN_READ_DEQ" *) method Action enDeq;
endinterface

instance Connectable#(ConditionalOutput_#(t), ConditionalOutput#(t));
  module mkConnection#(ConditionalOutput_#(t) a, ConditionalOutput#(t) b)();
    rule _safe1(b.en);
      a.write(b);
    endrule
  endmodule
endinstance

instance Connectable#(ConditionalOutput#(t), ConditionalOutput_#(t));
  module mkConnection#(ConditionalOutput#(t) a, ConditionalOutput_#(t) b)();
    mkConnection(asIfc(b), asIfc(a));
  endmodule
endinstance

module _ConditionalOutput#(Bool g1, Bool g2)(Tuple2#(ConditionalOutput_#(t), ConditionalOutput#(t))) provisos(Bits#(t, tSz));
  RegNormal#(t)         data <- mkRegUNormal;
  WireNormal#(t)       dataW <- mkWireNormal;
  PulseNormal            enq <- mkPulseNormal;
  PulseNormal           deqL <- mkPulseNormal;
  RegNormal#(Bool)     valid <- mkRegNormal(False);
  RegNormal#(Bool) doneR <- mkRegNormal(False);
  PulseNormal         resetW <- mkPulseNormal;

  RegNormal#(Bool)      enData <- mkRegUNormal;
  WireNormal#(Bool)    enDataW <- mkWireNormal;
  PulseNormal            enEnq <- mkPulseNormal;
  PulseNormal           enDeqL <- mkPulseNormal;
  RegNormal#(Bool)     enValid <- mkRegNormal(False);
  RegNormal#(Bool) enConsumedR <- mkRegNormal(False);
  PulseNormal         enResetW <- mkPulseNormal;

  rule a;
    if(resetW)
      doneR <= False;
    else if(enq)
      doneR <= True;

    if(deqL)
      valid <= False;
    else if(enq)
      valid <= True;

    if(enResetW)
      enConsumedR <= False;
    else if(enEnq)
      enConsumedR <= True;

    if(enDeqL)
      enValid <= False;
    else if(enEnq)
      enValid <= True;
  endrule

  return tuple2(
    interface ConditionalOutput_;
      method Action write(t x);// if(!valid && !doneR);
        dataW.write(x);
        data <= x;
        enq.send;
      endmethod
      method Bool doneBefore = doneR;
      method Bool notFull = !valid;
      method Bool done = enq || doneR;
      method Action reset = resetW.send;
      method Action en(Bool x);// if(!enValid && !enConsumedR);
        enDataW.write(x);
        enData <= x;
        enEnq.send;
      endmethod
      method Bool enConsumedBefore = enConsumedR;
      method Bool enNotFull = !enValid;
      method Bool enConsumed = enEnq || enConsumedR;
      method Action enReset = enResetW.send;
    endinterface,

    interface ConditionalOutput;
      method t _read;// if(valid || enq);
        return enq? dataW: data;
      endmethod
      method Bool notEmpty = (valid || enq);
      method Action deq;// if(valid || enq);
        deqL.send;
      endmethod
      method Bool en;// if(enValid || enEnq);
        return enEnq? enDataW: enData;
      endmethod
      method Bool enNotEmpty = (enValid || enEnq);
      method Action enDeq;// if(enValid || enEnq);
        enDeqL.send;
      endmethod
    endinterface);
endmodule




(* always_ready *)
interface OutputNormal_#(type t);
  (* always_enabled, prefix = "" *) method Action write((* port = "WRITE" *)t x);
endinterface

(* always_ready *)
interface OutputNormal#(type t);
  (* result = "READ" *) method t _read;
endinterface

instance Connectable#(OutputNormal_#(t), OutputNormal#(t));
  module mkConnection#(OutputNormal_#(t) a, OutputNormal#(t) b)();
    rule _safe1;
      a.write(b);
    endrule
  endmodule
endinstance

instance Connectable#(OutputNormal#(t), OutputNormal_#(t));
  module mkConnection#(OutputNormal#(t) a, OutputNormal_#(t) b)();
    mkConnection(asIfc(b), asIfc(a));
  endmodule
endinstance

module _OutputNormal#(Bool g1, Bool g2)(Tuple2#(OutputNormal_#(t), OutputNormal#(t))) provisos(Bits#(t, tSz));
  WireNormal#(t) dataLocal <- mkWireNormal;

  return tuple2(
    interface OutputNormal_;
      method Action write(t x) if(g1);
        dataLocal.write(x);
      endmethod
    endinterface,

    interface OutputNormal;
      method t _read if(g2);
        return dataLocal;
      endmethod
    endinterface);
endmodule

(* always_ready *)
interface OutputPulseNormal_;
  (* enable = "WRITE" *) method Action _read;
endinterface

typedef OutputNormal#(Bool) OutputPulseNormal;

instance Connectable#(OutputPulseNormal_, OutputPulseNormal);
  module mkConnection#(OutputPulseNormal_ a, OutputPulseNormal b)();
    rule _safe1(b);
      a;
    endrule
  endmodule
endinstance

instance Connectable#(OutputPulseNormal, OutputPulseNormal_);
  module mkConnection#(OutputPulseNormal a, OutputPulseNormal_ b)();
    mkConnection(asIfc(b), asIfc(a));
  endmodule
endinstance

module _OutputPulseNormal#(Bool g1, Bool g2)(Tuple2#(OutputPulseNormal_, OutputPulseNormal));
  PulseNormal dataLocal <- mkPulseNormal;

  return tuple2(
    interface OutputPulseNormal_;
      method Action _read if(g1);
        dataLocal.send;
      endmethod
    endinterface,

    interface OutputPulseNormal;
      method Bool _read if(g2);
        return dataLocal;
      endmethod
    endinterface);
endmodule

(* always_ready *)
interface ConditionalOutputNormal_#(type t);
  (* prefix = "", enable = "EN_WRITE" *) method Action write((* port = "WRITE" *)t x);
endinterface

(* always_ready *)
interface ConditionalOutputNormal#(type t);
  (* result = "READ" *) method t _read;
  (* result = "EN_READ" *) method Bool en;
endinterface

instance Connectable#(ConditionalOutputNormal_#(t), ConditionalOutputNormal#(t));
  module mkConnection#(ConditionalOutputNormal_#(t) a, ConditionalOutputNormal#(t) b)();
    rule _safe1(b.en);
      a.write(b);
    endrule
  endmodule
endinstance

instance Connectable#(ConditionalOutputNormal#(t), ConditionalOutputNormal_#(t));
  module mkConnection#(ConditionalOutputNormal#(t) a, ConditionalOutputNormal_#(t) b)();
    mkConnection(asIfc(b), asIfc(a));
  endmodule
endinstance

module _ConditionalOutputNormal#(Bool g1, Bool g2)(Tuple2#(ConditionalOutputNormal_#(t), ConditionalOutputNormal#(t))) provisos(Bits#(t, tSz));
  WireNormal#(t) dataLocal <- mkWireNormal;
  PulseNormal enLocal <- mkPulseNormal;

  return tuple2(
    interface ConditionalOutputNormal_;
      method Action write(t x) if(g1);
        dataLocal.write(x);
        enLocal.send;
      endmethod
    endinterface,

    interface ConditionalOutputNormal;
      method t _read if(g2);
        return dataLocal;
      endmethod

      method Bool en if(g2);
        return enLocal;
      endmethod
    endinterface);
endmodule

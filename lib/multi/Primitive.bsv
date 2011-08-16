import Base::*;
import Connectable::*;
import Vector::*;

(* always_ready *)
interface Output_#(type t);
  (* always_enabled, prefix = "" *) method Action write((* port = "WRITE" *)t x);
  (* enable = "WRITE_VALID" *) method Action valid;
  (* result = "WRITE_CONSUMED" *) method Bool consumed;
endinterface

(* always_ready *)
interface Output#(type t);
  (* result = "READ" *) method t _read;
  (* result = "READ_VALID" *) method Bool valid;
  (* enable = "READ_CONSUMED" *) method Action consumed;
endinterface

instance Connectable#(Output_#(t), Output#(t));
  module mkConnection#(Output_#(t) a, Output#(t) b)();
    (* preempts = "_safe1, _safe2"*)
    rule _safe1(b.valid);
      a.write(b);
      a.valid;
    endrule

    rule _safe2(b.valid);
      a.valid;
    endrule

    rule _safe3(a.consumed);
      b.consumed;
    endrule
  endmodule
endinstance

instance Connectable#(Output#(t), Output_#(t));
  module mkConnection#(Output#(t) a, Output_#(t) b)();
    mkConnection(asIfc(b), asIfc(a));
  endmodule
endinstance

module _Output#(Bool g1, Bool g2)(Tuple2#(Output_#(t), Output#(t))) provisos(Bits#(t, tSz));
  WireNormal#(t) dataLocal <- mkWireNormal;
  PulseNormal validLocal <- mkPulseNormal;
  PulseNormal consumedLocal <- mkPulseNormal;

  return tuple2(
    interface Output_;
      method Action write(t x) if(g1);
        dataLocal.write(x);
      endmethod

      method Action valid;
        validLocal.send;
      endmethod

      method Bool consumed;
        return consumedLocal;
      endmethod
    endinterface,

    interface Output;
      method t _read if(g2);
        return dataLocal;
      endmethod

      method Bool valid;
        return validLocal;
      endmethod

      method Action consumed;
        consumedLocal.send;
      endmethod
    endinterface);
endmodule

(* always_ready *)
interface OutputPulse_;
  (* enable = "WRITE" *) method Action _read;
  (* enable = "WRITE_VALID" *) method Action valid;
  (* result = "WRITE_CONSUMED" *) method Bool consumed;
endinterface

typedef Output#(Bool) OutputPulse;

instance Connectable#(OutputPulse_, OutputPulse);
  module mkConnection#(OutputPulse_ a, OutputPulse b)();
    (* preempts = "_safe1, _safe2" *)
    rule _safe1(b.valid && b);
      a;
      a.valid;
    endrule

    rule _safe2(b.valid);
      a.valid;
    endrule

    rule _safe3(a.consumed);
      b.consumed;
    endrule
  endmodule
endinstance

instance Connectable#(OutputPulse, OutputPulse_);
  module mkConnection#(OutputPulse a, OutputPulse_ b)();
    mkConnection(asIfc(b), asIfc(a));
  endmodule
endinstance

module _OutputPulse#(Bool g1, Bool g2)(Tuple2#(OutputPulse_, OutputPulse));
  PulseNormal dataLocal <- mkPulseNormal;
  PulseNormal validLocal <- mkPulseNormal;
  PulseNormal consumedLocal <- mkPulseNormal;


  return tuple2(
    interface OutputPulse_;
      method Action _read if(g1);
        dataLocal.send;
      endmethod

      method Action valid;
        validLocal.send;
      endmethod

      method Bool consumed;
        return consumedLocal;
      endmethod
    endinterface,

    interface OutputPulse;
      method Bool _read if(g2);
        return dataLocal;
      endmethod

      method Bool valid;
        return validLocal;
      endmethod

      method Action consumed;
        consumedLocal.send;
      endmethod
    endinterface);
endmodule

(* always_ready *)
interface ConditionalOutput_#(type t);
  (* prefix = "", enable = "EN_WRITE" *) method Action write((* port = "WRITE" *)t x);
  (* enable = "WRITE_VALID" *) method Action valid;
  (* enable = "EN_WRITE_VALID" *) method Action en_valid;
  (* result = "WRITE_CONSUMED" *) method Bool consumed;
  (* result = "EN_WRITE_CONSUMED" *) method Bool en_consumed;
endinterface

(* always_ready *)
interface ConditionalOutput#(type t);
  (* result = "READ" *) method t _read;
  (* result = "EN_READ" *) method Bool en;
  (* result = "READ_VALID" *) method Bool valid;
  (* result = "EN_READ_VALID" *) method Bool en_valid;
  (* enable = "READ_CONSUMED" *) method Action consumed;
  (* enable = "EN_READ_CONSUMED" *) method Action en_consumed;
endinterface

instance Connectable#(ConditionalOutput_#(t), ConditionalOutput#(t));
  module mkConnection#(ConditionalOutput_#(t) a, ConditionalOutput#(t) b)();
    (* preempts = "_safe1, _safe2" *)
    rule _safe1(b.valid && b.en_valid && b.en);
      a.write(b);
      a.valid;
      a.en_valid;
    endrule

    rule _safe2(b.valid && b.en_valid);
      a.valid;
      a.en_valid;
    endrule

    rule _safe3(a.consumed && a.en_consumed);
      b.consumed;
      b.en_consumed;
    endrule
  endmodule
endinstance

instance Connectable#(ConditionalOutput#(t), ConditionalOutput_#(t));
  module mkConnection#(ConditionalOutput#(t) a, ConditionalOutput_#(t) b)();
    mkConnection(asIfc(b), asIfc(a));
  endmodule
endinstance

module _ConditionalOutput#(Bool g1, Bool g2)(Tuple2#(ConditionalOutput_#(t), ConditionalOutput#(t))) provisos(Bits#(t, tSz));
  WireNormal#(t) dataLocal <- mkWireNormal;
  PulseNormal enLocal <- mkPulseNormal;
  PulseNormal validLocal <- mkPulseNormal;
  PulseNormal en_validLocal <- mkPulseNormal;
  PulseNormal consumedLocal <- mkPulseNormal;
  PulseNormal en_consumedLocal <- mkPulseNormal;

  return tuple2(
    interface ConditionalOutput_;
      method Action write(t x) if(g1);
        dataLocal.write(x);
        enLocal.send;
      endmethod

      method Action valid;
        validLocal.send;
      endmethod

      method Action en_valid;
        en_validLocal.send;
      endmethod

      method Bool consumed;
        return consumedLocal;
      endmethod

      method Bool en_consumed;
        return en_consumedLocal;
      endmethod
    endinterface,

    interface ConditionalOutput;
      method t _read if(g2);
        return dataLocal;
      endmethod

      method Bool en if(g2);
        return enLocal;
      endmethod

      method Bool valid;
        return validLocal;
      endmethod

      method Bool en_valid;
        return en_validLocal;
      endmethod

      method Action consumed;
        consumedLocal.send;
      endmethod

      method Action en_consumed;
        en_consumedLocal.send;
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

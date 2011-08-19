include Library;

include FifoNormal;

port InputFifo#(type t);
  Input#(t) in;
  Reverse FifoDeqNormal#(t) deq;
endport

partition InputFifo#(t) mkInputFifo provisos(Bits#(t, tSz));
  atomic a;
    deq.notEmpty := in.valid;
    if(in.valid && deq.deq)
    begin
      in.consumed;
      deq.first := in;
    end
  endatomic
endpartition

port OutputFifo#(numeric type n, type t);
  Output#(t) out;
  Reverse FifoEnqNormal#(t) enq;
endport

partition OutputFifo#(n, t) mkOutputGenericFifo#(function _m__#(FifoNormal#(n, t)) mkF) provisos(Bits#(t, tSz));
  FifoNormal#(n, t) f <- mkF;

  atomic a;
    if(f.deq.notEmpty)
    begin
      out := f.deq.first;
      out.valid;
    end
    if(out.consumed)
      f.deq.deq;
  endatomic

  mkConnection(enq, f.enq);
endpartition

partinst OutputFifo#(n, t) mkOutputFifo provisos(Bits#(t, tSz)) = mkOutputGenericFifo(mkFifoNormal);
partinst OutputFifo#(n, t) mkOutputLFifo provisos(Bits#(t, tSz)) = mkOutputGenericFifo(mkLFifoNormal);
partinst OutputFifo#(n, t) mkOutputBypassFifo provisos(Bits#(t, tSz)) = mkOutputGenericFifo(mkBypassFifoNormal);

portalias InputPulseFifo = InputFifo#(Bool);

partinst InputPulseFifo mkInputPulseFifo = mkInputFifo;

port OutputPulseFifo#(numeric type n);
  OutputPulse out;
  Reverse FifoEnqNormal#(Bool) enq;
endport

partition OutputPulseFifo#(n) mkOutputPulseGenericFifo#(function _m__#(FifoNormal#(n, Bool)) mkF);
  FifoNormal#(n, Bool) f <- mkF;

  atomic a;
    if(f.deq.notEmpty)
    begin
      if(f.deq.first)
        out;
      out.valid;
    end
    if(out.consumed)
      f.deq.deq;
  endatomic

  mkConnection(enq, f.enq);
endpartition

partinst OutputPulseFifo#(n) mkOutputPulseFifo = mkOutputPulseGenericFifo(mkFifoNormal);
partinst OutputPulseFifo#(n) mkOutputPulseLFifo = mkOutputPulseGenericFifo(mkLFifoNormal);
partinst OutputPulseFifo#(n) mkOutputPulseBypassFifo = mkOutputPulseGenericFifo(mkBypassFifoNormal);

port ConditionalInputFifo#(type t);
  ConditionalInput#(t) in;
  Reverse FifoDeqNormal#(Maybe#(t)) deq;
endport

partition ConditionalInputFifo#(t) mkConditionalInputFifo provisos(Bits#(t, tSz));
  atomic a;
    deq.notEmpty := in.valid && in.en_valid;
    if(in.valid && in.en_valid && deq.deq)
    begin
      in.consumed;
      in.en_consumed;
    end
    if((in.valid && in.en_valid) && deq.deq)
    begin
      if(in.en)
        deq.first := tagged Valid (in);
      else
        deq.first := tagged Invalid;
    end
  endatomic
endpartition

port ConditionalOutputFifo#(numeric type n, type t);
  ConditionalOutput#(t) out;
  Reverse FifoEnqNormal#(Maybe#(t)) enq;
endport

partition ConditionalOutputFifo#(n, t) mkConditionalOutputGenericFifo#(function _m__#(FifoNormal#(n, Maybe#(t))) mkF) provisos(Bits#(t, tSz));
  FifoNormal#(n, Maybe#(t)) f <- mkF;

  atomic a;
    if(f.deq.notEmpty)
    begin
      if(f.deq.first matches tagged Valid .x)
        out := x;
      out.valid;
      out.en_valid;
    end
    if(out.consumed && out.en_consumed)
      f.deq.deq;
  endatomic

  mkConnection(enq, f.enq);
endpartition

partinst ConditionalOutputFifo#(n, t) mkConditionalOutputFifo provisos(Bits#(t, tSz)) = mkConditionalOutputGenericFifo(mkFifoNormal);
partinst ConditionalOutputFifo#(n, t) mkConditionalOutputLFifo provisos(Bits#(t, tSz)) = mkConditionalOutputGenericFifo(mkLFifoNormal);
partinst ConditionalOutputFifo#(n, t) mkConditionalOutputBypassFifo provisos(Bits#(t, tSz)) = mkConditionalOutputGenericFifo(mkBypassFifoNormal);

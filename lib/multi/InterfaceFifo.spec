include Library;

include LibraryNormal;
include FifoNormal;

port InputFifo#(numeric type n, type t);
  Input#(t) in;
  Reverse FifoDeqNormal#(t) deq;
endport

partition InputFifo#(n, t) mkInputGenericFifo#(function _m__#(FifoNormal#(n, t)) mkF) provisos(Bits#(t, tSz));
  FifoNormal#(n, t) f <- mkF;

  atomic a;
    if((in.valid? f.enq.notFull: True))
      in.consumed;
    if(in.valid && f.enq.notFull)
      f.enq.enq := in;
  endatomic

  mkConnection(deq, f.deq);
endpartition

partinst InputFifo#(n, t) mkInputFifo provisos(Bits#(t, tSz)) = mkInputGenericFifo(mkFifoNormal);
partinst InputFifo#(n, t) mkInputLFifo provisos(Bits#(t, tSz)) = mkInputGenericFifo(mkLFifoNormal);
partinst InputFifo#(n, t) mkInputBypassFifo provisos(Bits#(t, tSz)) = mkInputGenericFifo(mkBypassFifoNormal);

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
    if(f.deq.notEmpty && out.consumed)
      f.deq.deq;
  endatomic

  mkConnection(enq, f.enq);
endpartition

partinst OutputFifo#(n, t) mkOutputFifo provisos(Bits#(t, tSz)) = mkOutputGenericFifo(mkFifoNormal);
partinst OutputFifo#(n, t) mkOutputLFifo provisos(Bits#(t, tSz)) = mkOutputGenericFifo(mkLFifoNormal);
partinst OutputFifo#(n, t) mkOutputBypassFifo provisos(Bits#(t, tSz)) = mkOutputGenericFifo(mkBypassFifoNormal);

portalias InputPulseFifo#(numeric type n) = InputFifo#(n, Bool);

partinst InputPulseFifo#(n) mkInputPulseGenericFifo#(function _m__#(FifoNormal#(n, Bool)) mkF) = mkInputGenericFifo(mkF);

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
    if(f.deq.notEmpty && out.consumed)
      f.deq.deq;
  endatomic

  mkConnection(enq, f.enq);
endpartition

partinst OutputPulseFifo#(n) mkOutputPulseFifo = mkOutputPulseGenericFifo(mkFifoNormal);
partinst OutputPulseFifo#(n) mkOutputPulseLFifo = mkOutputPulseGenericFifo(mkLFifoNormal);
partinst OutputPulseFifo#(n) mkOutputPulseBypassFifo = mkOutputPulseGenericFifo(mkBypassFifoNormal);

port ConditionalInputFifo#(numeric type n, type t);
  ConditionalInput#(t) in;
  Reverse FifoDeqNormal#(Maybe#(t)) deq;
endport

partition ConditionalInputFifo#(n, t) mkConditionalInputGenericFifo#(function _m__#(FifoNormal#(n, Maybe#(t))) mkF) provisos(Bits#(t, tSz));
  FifoNormal#(n, Maybe#(t)) f <- mkF;

  atomic a;
    if(((in.valid && in.en_valid)? f.enq.notFull: True))
    begin
      in.consumed;
      in.en_consumed;
    end
    if((in.valid && in.en_valid) && f.enq.notFull)
    begin
      if(in.en)
        f.enq.enq := tagged Valid (in);
      else
        f.enq.enq := tagged Invalid;
    end
  endatomic

  mkConnection(deq, f.deq);
endpartition

partinst ConditionalInputFifo#(n, t) mkConditionalInputFifo provisos(Bits#(t, tSz)) = mkConditionalInputGenericFifo(mkFifoNormal);
partinst ConditionalInputFifo#(n, t) mkConditionalInputLFifo provisos(Bits#(t, tSz)) = mkConditionalInputGenericFifo(mkLFifoNormal);
partinst ConditionalInputFifo#(n, t) mkConditionalInputBypassFifo provisos(Bits#(t, tSz)) = mkConditionalInputGenericFifo(mkBypassFifoNormal);

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
    if(f.deq.notEmpty && (out.consumed && out.en_consumed))
      f.deq.deq;
  endatomic

  mkConnection(enq, f.enq);
endpartition

partinst ConditionalOutputFifo#(n, t) mkConditionalOutputFifo provisos(Bits#(t, tSz)) = mkConditionalOutputGenericFifo(mkFifoNormal);
partinst ConditionalOutputFifo#(n, t) mkConditionalOutputLFifo provisos(Bits#(t, tSz)) = mkConditionalOutputGenericFifo(mkLFifoNormal);
partinst ConditionalOutputFifo#(n, t) mkConditionalOutputBypassFifo provisos(Bits#(t, tSz)) = mkConditionalOutputGenericFifo(mkBypassFifoNormal);

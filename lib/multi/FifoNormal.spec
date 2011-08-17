include MultiFifoNormal;

port FifoEnqNormal#(type t);
  InputNormal#(Bool) notFull;
  ConditionalOutputNormal#(t) enq WriteGuard(notFull);
endport

port FifoDeqNormal#(type t);
  InputNormal#(Bool) notEmpty;
  InputNormal#(t) first ReadGuard(notEmpty);
  OutputPulseNormal deq WriteGuard(notEmpty);
endport

port FifoNormal#(numeric type n, type t);
  Reverse FifoEnqNormal#(t) enq;
  Reverse FifoDeqNormal#(t) deq;
endport

partition FifoNormal#(n, t) mkGenericFifoNormal#(function _m__#(MultiFifoNormal#(n, 1, 1, t)) mkF) provisos(Bits#(t, tSz));
  MultiFifoNormal#(n, 1, 1, t) f <- mkF;

  atomic a;
    enq.notFull := f.enq.numFreeSlots > 0;
    deq.notEmpty := f.deq.numFilledSlots > 0;
    f.deq.numDeqs := deq.deq? 1: 0;
    deq.first := f.deq.data[0];
  endatomic

  mkConnection(f.enq.data[0], enq.enq);
endpartition

partinst FifoNormal#(n, t) mkLFifoNormal provisos(Bits#(t, tSz)) = mkGenericFifoNormal(mkMultiLFifoNormal);

partinst FifoNormal#(n, t) mkFifoNormal provisos(Bits#(t, tSz)) = mkGenericFifoNormal(mkMultiFifoNormal);

partinst FifoNormal#(n, t) mkBypassFifoNormal provisos(Bits#(t, tSz)) = mkGenericFifoNormal(mkMultiBypassFifoNormal);

port UgFifoEnqNormal#(type t);
  InputNormal#(Bool) notFull;
  ConditionalOutputNormal#(t) enq;
endport

port UgFifoDeqNormal#(type t);
  InputNormal#(Bool) notEmpty;
  InputNormal#(t) first;
  OutputPulseNormal deq;
endport

port UgFifoNormal#(numeric type n, type t);
  Reverse UgFifoEnqNormal#(t) enq;
  Reverse UgFifoDeqNormal#(t) deq;
endport

partition UgFifoNormal#(n, t) mkGenericUgFifoNormal#(function _m__#(MultiFifoNormal#(n, 1, 1, t)) mkF) provisos(Bits#(t, tSz));
  MultiFifoNormal#(n, 1, 1, t) f <- mkF;

  atomic a;
    enq.notFull := f.enq.numFreeSlots > 0;
    deq.notEmpty := f.deq.numFilledSlots > 0;
    f.deq.numDeqs := deq.deq? 1: 0;
    deq.first := f.deq.data[0];
  endatomic

  mkConnection(f.enq.data[0], enq.enq);
endpartition

partinst UgFifoNormal#(n, t) mkLUgFifoNormal provisos(Bits#(t, tSz)) = mkGenericUgFifoNormal(mkMultiLFifoNormal);

partinst UgFifoNormal#(n, t) mkUgFifoNormal provisos(Bits#(t, tSz)) = mkGenericUgFifoNormal(mkMultiFifoNormal);

partinst UgFifoNormal#(n, t) mkBypassUgFifoNormal provisos(Bits#(t, tSz)) = mkGenericUgFifoNormal(mkMultiBypassFifoNormal);


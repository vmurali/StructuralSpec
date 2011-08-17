include MultiFifo;

port FifoEnq#(type t);
  Input#(Bool) notFull;
  ConditionalOutput#(t) enq WriteGuard(notFull);
endport

port FifoDeq#(type t);
  Input#(Bool) notEmpty;
  Input#(t) first ReadGuard(notEmpty);
  OutputPulse deq WriteGuard(notEmpty);
endport

port Fifo#(numeric type n, type t);
  Reverse FifoEnq#(t) enq;
  Reverse FifoDeq#(t) deq;
endport

partition Fifo#(n, t) mkGenericFifo#(function _m__#(MultiFifo#(n, 1, 1, t)) mkF) provisos(Bits#(t, tSz));
  MultiFifo#(n, 1, 1, t) f <- mkF;

  atomic a;
    enq.notFull := f.enq.numFreeSlots > 0;
    deq.notEmpty := f.deq.numFilledSlots > 0;
    f.deq.numDeqs := deq.deq? 1: 0;
    deq.first := f.deq.data[0];
  endatomic

  mkConnection(f.enq.data[0], enq.enq);
endpartition

partinst Fifo#(n, t) mkLFifo provisos(Bits#(t, tSz)) = mkGenericFifo(mkMultiLFifo);

partinst Fifo#(n, t) mkFifo provisos(Bits#(t, tSz)) = mkGenericFifo(mkMultiFifo);

partinst Fifo#(n, t) mkBypassFifo provisos(Bits#(t, tSz)) = mkGenericFifo(mkMultiBypassFifo);

port UgFifoEnq#(type t);
  Input#(Bool) notFull;
  ConditionalOutput#(t) enq;
endport

port UgFifoDeq#(type t);
  Input#(Bool) notEmpty;
  Input#(t) first;
  OutputPulse deq;
endport

port UgFifo#(numeric type n, type t);
  Reverse UgFifoEnq#(t) enq;
  Reverse UgFifoDeq#(t) deq;
endport

partition UgFifo#(n, t) mkGenericUgFifo#(function _m__#(MultiFifo#(n, 1, 1, t)) mkF) provisos(Bits#(t, tSz));
  MultiUgFifo#(n, 1, 1, t) f <- mkF;

  atomic a;
    enq.notFull := f.enq.numFreeSlots > 0;
    deq.notEmpty := f.deq.numFilledSlots > 0;
    f.deq.numDeqs := deq.deq? 1: 0;
    deq.first := f.deq.data[0];
  endatomic

  mkConnection(f.enq.data[0], enq.enq);
endpartition

partinst UgFifo#(n, t) mkLUgFifo provisos(Bits#(t, tSz)) = mkGenericUgFifo(mkMultiLFifo);

partinst UgFifo#(n, t) mkUgFifo provisos(Bits#(t, tSz)) = mkGenericUgFifo(mkMultiFifo);

partinst UgFifo#(n, t) mkBypassUgFifo provisos(Bits#(t, tSz)) = mkGenericUgFifo(mkMultiBypassFifo);

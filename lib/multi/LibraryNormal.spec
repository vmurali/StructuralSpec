port FifoEnqNormal#(type t);
  InputNormal#(Bool) notFull;
  ConditionalOutputNormal#(t) enq WriteGuard(notFull);
endport

port FifoDeqNormal#(type t);
  InputNormal#(Bool) notEmpty;
  InputNormal#(t) first ReadGuard(notEmpty);
  OutputPulseNormal deq WriteGuard(notEmpty);
endport

port EmptyNormal;
endport

port FifoEnqNormal#(type t);
  InputNormal#(Bool) notFull;
  ConditionalOutputNormal#(t) enq Guard(notFull);
endport

port FifoDeqNormal#(type t);
  InputNormal#(Bool) notEmpty;
  InputNormal#(t) first Guard(notEmpty);
  OutputPulseNormal deq Guard(notEmpty);
endport

port EmptyNormal;
endport

port FifoEnq#(type t);
  Input#(Bool) notFull;
  ConditionalOutput#(t) enq Guard(notFull);
endport

port FifoDeq#(type t);
  Input#(Bool) notEmpty;
  Input#(t) first Guard(notEmpty);
  OutputPulse deq Guard(notEmpty);
endport

port Empty;
endport

typedef Bit#(TLog#(n)) Index#(type n);
typedef Bit#(TLog#(TAdd#(n, 1))) NumElems#(type n);

function Index#(n) moduloPlus(NumElems#(n) incr, Index#(n) orig) = truncate(zeroExtend(orig) + incr <= (fromInteger(valueOf(n) - 1))? zeroExtend(orig) + incr: (incr - (fromInteger(valueOf(n) - 1) - (zeroExtend(orig) - 1))));
function Index#(n) moduloIncr(Index#(n) orig) = moduloPlus(1, orig);


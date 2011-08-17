port Empty;
endport

typedef Bit#(TLog#(n)) Index#(type n);
typedef Bit#(TLog#(TAdd#(n, 1))) NumElems#(type n);

function Index#(n) moduloPlus(Integer size, NumElems#(n) incr, Index#(n) orig);
  NumElems#(n) origLocal = zeroExtend(orig);
  NumElems#(n) retLocal = origLocal + incr <= (fromInteger(size - 1))? origLocal + incr: (incr - (fromInteger(size - 1) - (origLocal - 1)));
  return truncate(retLocal);
endfunction

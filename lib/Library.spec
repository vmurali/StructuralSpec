port Empty;
endport

typedef Bit#(TLog#(n)) Index#(type n);
typedef Bit#(TLog#(TAdd#(n, 1))) NumElems#(type n);

function Index#(n) moduloPlus(Integer size, NumElems#(n) incr, Index#(n) orig) = truncate(zeroExtend(orig) + incr <= (fromInteger(size - 1))? zeroExtend(orig) + incr: (incr - (fromInteger(size - 1) - (zeroExtend(orig) - 1))));
function Index#(n) moduloIncr(Integer size, Index#(n) orig) = moduloPlus(size, 1, orig);


include Library;

include MultiFifo;

port FreeListAllocate#(numeric type n, numeric type allocs);
  Input#(NumElems#(allocs)) numFreeSlots;
  ConditionalInput#(Index#(n))[allocs] index;
  Output#(NumElems#(allocs)) allocateNum;
endport

port FreeList#(numeric type n, numeric type allocs, numeric type frees);
  Output#(Bool) initialized;
  Reverse FreeListAllocate#(n, allocs) allocate;
  ConditionalInput#(Index#(n))[frees] free;
endport

partition FreeList#(n, allocs, frees) mkFreeList#(NumElems#(n) allots);
  MultiFifo#(n, frees, allocs, Index#(n)) f <- mkMultiFifo;

  Reg#(NumElems#(n)) initializing <- mkReg(allots);

  Bool initializedLocal = initializing == fromInteger(valueOf(n));

  mkConnection(allocate.numFreeSlots, f.remove.numFilledSlots);
  mkConnection(allocate.index, f.remove.data);
  mkConnection(allocate.allocateNum, f.remove.numDeqs);

  mkConnection(free, f.fill.data);

  atomic a0;
    initialized := initializedLocal;
  endatomic
endpartition

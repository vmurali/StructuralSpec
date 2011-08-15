include Library;

include LibraryNormal;

include MultiFifoNormal;

port FreeListAllocateNormal#(numeric type n, numeric type allocs);
  InputNormal#(NumElems#(allocs)) numFreeSlots;
  ConditionalInputNormal#(Index#(n))[allocs] index;
  OutputNormal#(NumElems#(allocs)) allocateNum;
endport

port FreeListNormal#(numeric type n, numeric type allocs, numeric type frees);
  OutputNormal#(Bool) initialized;
  Reverse FreeListAllocateNormal#(n, allocs) allocate;
  ConditionalInputNormal#(Index#(n))[frees] free;
endport

partition FreeListNormal#(n, allocs, frees) mkFreeListNormal#(NumElems#(n) allots);
  MultiFifoNormal#(n, frees, allocs, Index#(n)) f <- mkMultiFifoNormal;

  RegNormal#(NumElems#(n)) initializing <- mkRegNormal(allots);

  Bool initializedLocal = initializing == fromInteger(valueOf(n));

  mkConnection(allocate.numFreeSlots, f.remove.numFilledSlots);
  mkConnection(allocate.index, f.remove.data);
  mkConnection(allocate.allocateNum, f.remove.numDeqs);

  mkConnection(free, f.fill.data);

  atomic a0;
    initialized := initializedLocal;
  endatomic
endpartition

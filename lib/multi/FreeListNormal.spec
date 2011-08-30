include Library;

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

partition FreeList#(n, allocs, frees) mkFreeList#(NumElems#(n) allots);
  MultiFifoNormal#(n, frees, allocs, Index#(n)) f <- mkMultiFifoNormal;

  RegNormal#(NumElems#(n)) initializing <- mkRegNormal(allots);

  Bool initializedLocal = initializing == fromInteger(valueOf(n));

  atomic a0(!initializedLocal);
    initializing <= initializing + 1;
    f.enq.data[0] := truncate(initializing);
  endatomic

  atomic a1(initializedLocal);
    allocate.numFreeSlots := f.deq.numFilledSlots;
    for(Integer i = 0; i < valueOf(allocs); i = i + 1)
      if(f.deq.data[i].en)
        allocate.index[i] := f.deq.data[i];
    f.deq.numDeqs := allocate.allocateNum;

    for(Integer i = 0; i < valueOf(frees); i = i + 1)
      if(free[i].en)
        f.enq.data[i] := free[i];
  endatomic

  atomic a2;
    initialized := initializedLocal;
  endatomic
endpartition

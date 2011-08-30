include Library;

include MultiFifo;

port FreeListAllocate#(numeric type n, numeric type allocs);
  Input#(NumElems#(n)) numFreeSlots;
  ConditionalInput#(Index#(n))[allocs] index;
  Output#(NumElems#(n)) allocateNum;
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

include Library;
include Types;
include Fifo;
include RegFile;

port Writeback;
  Reverse GuardedAction#(Wb) wb;
  OutputEn#(RegIndex) wbIndex;
  FifoDeq#(Data) dataQ;
  RegWrite#(RegIndexSz, Data) regWrite;
endport

partition mkWriteback implements Writeback;
  Fifo#(1, Wb) wbQ <- mkLFifo;

  mkConnection(asIfc(wb), wbQ.enq);

  rule r1;
    if(wbQ.deq.rdy)
      wbIndex.data := wbQ.deq.first.index;
   else
      wbIndex.data.justFinish;
  endrule

  rule r2;
    if(wbQ.deq.rdy)
    begin
      if(wbQ.deq.first.data matches tagged Valid .d)
        regWrite.write.data := tuple2(wbQ.deq.first.index, d);
      else if(dataQ.rdy)
        regWrite.write.data := tuple2(wbQ.deq.first.index, dataQ.first);
      else
        regWrite.write.data.justFinish;
    end
    else
      regWrite.write.data.justFinish;
  endrule

  rule r3;
    if(wbQ.deq.rdy)
    begin
      if(wbQ.deq.first.data matches tagged Valid .*)
        wbQ.deq.deq;
      else if(dataQ.rdy)
        wbQ.deq.deq;
      else
        wbQ.deq.deq.justFinish;
    end
    else
      wbQ.deq.deq.justFinish;
  endrule

  rule r4;
    if(wbQ.deq.rdy && !isValid(wbQ.deq.first.data) && dataQ.rdy)
      dataQ.deq;
    else
      dataQ.deq.justFinish;
  endrule

  rule r5;
    specCycleDone;
  endrule
endpartition

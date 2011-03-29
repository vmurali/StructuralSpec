include Library;
include Types;
include Fifo;
include RegFile;
include WritebackPort;

(* synthesize *)
partition mkWriteback implements Writeback;
  Fifo#(1, Wb) wbQ <- mkLFifo;

  mkConnection(wb, wbQ.enq);

  rule r1;
    if(wbQ.deq.rdy)
      wbIndex.data := wbQ.deq.first.index;
    else
      wbIndex.data.justFinish;
  endrule

  let syncFiresR2 = wbQ.deq.rdy &&
                    (wbQ.deq.data matches tagged Valid .d?
                       wbQ.deq.rdy:
                       wbQ.deq.rdy && dataQ.rdy);

  rule r2;
    if(syncFiresR2)
    begin
      if(wbQ.deq.first.data matches tagged Valid .d)
        regWrite.data := tuple2(wbQ.deq.first.index, d);
      else
        regWrite.data := tuple2(wbQ.deq.first.index, dataQ.first);
    end
    else
      regWrite.data.justFinish;
  endrule

  rule r3;
    if(syncFiresR2)
    begin
      if(wbQ.deq.first.data matches tagged Valid .d)
        wbQ.deq.deq;
      else
        wbQ.deq.deq;
    end
    else
      wbQ.deq.deq.justFinish;
  endrule

  rule r4;
    if(syncFiresR2)
    begin
      if(wbQ.deq.first.data matches tagged Valid .d)
        dataQ.deq.justFinish;
      else
        dataQ.deq;
    end
    else
      dataQ.deq.justFinish;
  endrule

  rule r5;
    specCycleDone;
  endrule
endpartition

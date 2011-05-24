include Library;
include Types;
include Fifo;
include RegFile;
include WritebackPort;

(* synthesize *)
partition mkWriteback implements Writeback;
  Fifo#(1, Wb) wbQ <- mkLFifo;

  mkConnection(wb, wbQ.enq);

  atomic r1;
    wbIndex.data := wbQ.deq.index;
  endatomic

  atomic r2;
    if(wbQ.deq.data matches tagged Valid .d)
    begin
      regWrite.data := tuple2(wbQ.deq.index, d);
      wbQ.deq.deq;
    end
    else
    begin
      regWrite.data := tuple2(wbQ.deq.index, dataQ.first);
      wbQ.deq.deq;
      dataQ.deq;
    end
  endatomic
endpartition

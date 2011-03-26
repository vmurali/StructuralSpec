include Library;
include Types;
include Fifo;
include RegFile;
include WritebackPort;

(* synthesize *)
partition mkWriteback implements Writeback;
  Fifo#(1, Wb) wbQ <- mkLFifo;

  mkConnection(asIfc(wb), wbQ.enq);

  rule r1;
    wbIndex.data := wbQ.deq.index;
    if(wbQ.deq.data matches tagged Valid .d)
    begin
      regWrite := tuple2(wbQ.deq.index, d);
      wbQ.deq.deq;
    end
    else
    begin
      regWrite := tuple2(wbQ.deq.index, dataQ.first);
      wbQ.deq.deq;
      dataQ.deq;
    end
  endrule
endpartition

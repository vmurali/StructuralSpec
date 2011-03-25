include Library;
include Types;
include Fifo;
include RegFile;

port Writeback;
  Reverse GuardedAction#(Wb) wb;
  OutputEn#(RegIndex) wbIndex;
  FifoDeq#(Data) data;
  RegWrite#(TLog#(SizeOf#(RegIndex)), Data) regWrite;
endport

partition mkWriteback implements Writeback;
  Fifo#(1, Wb) wbQ <- mkLFifo;

  mkConnection(asIfc(wb), wbQ.enq);

  rule r1;
    wbIndex := wbQ.deq.index;
    if(wbQ.deq.data matches tagged Valid .d)
    begin
      regWrite := tuple2(wbQ.deq.index, d);
      wbQ.deq.en;
    end
  endrule

  rule r2;
    if(wbQ.deq.data matches tagged Invalid)
    begin
      regWrite := tuple2(wb.deq.index, data);
      wbQ.deq.en;
      data.deq;
    end
  endrule

  rule r3;
    specCycleDone;
  endrule
endpartition

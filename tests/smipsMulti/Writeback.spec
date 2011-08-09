include Library;
include Types;
include Fifo;

port Writeback;
  Reverse FifoEnq#(Wb) wb;
  ConditionalOutput#(RegIndex) wbIndex;
  FifoDeq#(Data) dataQ;
  ConditionalOutput#(RegWrite) regWrite;
endport

(* synthesize *)
partition Writeback mkWriteback;
  Fifo#(1, Wb) wbQ <- mkLFifo;

  mkConnection(wb, wbQ.enq);

  atomic r1;
    wbIndex := wbQ.deq.first.index;
  endatomic

  atomic r2;
    if(wbQ.deq.first.data matches tagged Valid .d)
    begin
      regWrite := RegWrite{index: wbQ.deq.first.index, data: d};
      wbQ.deq.deq;
    end
    else
    begin
      regWrite := RegWrite{index: wbQ.deq.first.index, data: dataQ.first};
      wbQ.deq.deq;
      dataQ.deq;
    end
  endatomic
endpartition

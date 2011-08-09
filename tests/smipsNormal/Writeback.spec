include LibraryNormal;
include Types;
include FifoNormal;

port Writeback;
  Reverse FifoEnqNormal#(Wb) wb;
  ConditionalOutputNormal#(RegIndex) wbIndex;
  FifoDeqNormal#(Data) dataQ;
  ConditionalOutputNormal#(RegWrite) regWrite;
endport

(* synthesize *)
partition Writeback mkWriteback;
  FifoNormal#(1, Wb) wbQ <- mkLFifoNormal;

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

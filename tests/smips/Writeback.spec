include Library;
include Types;
include Fifo;

port Writeback;
  Reverse FifoEnq#(Wb) wb;
  ConditionalOutput#(RegIndex) wbIndex;
  FifoDeq#(Data) dataQ;
  ConditionalOutput#(Pair#(RegIndex, Data)) regWrite;
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
      regWrite := Pair{fst: wbQ.deq.first.index, snd: d};
      wbQ.deq.deq;
    end
    else
    begin
      regWrite := Pair{fst: wbQ.deq.first.index, snd: dataQ.first};
      wbQ.deq.deq;
      dataQ.deq;
    end
  endatomic
endpartition

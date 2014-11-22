include Library;
include Types;
include Fifo;

port Writeback;
  Reverse FifoEnq#(Pair#(RegIndex, Maybe#(Data))) wb;
  ConditionalOutput#(RegIndex) wbIndex;
  FifoDeq#(Data) dataQ;
  ConditionalOutput#(Pair#(RegIndex, Data)) regWrite;
endport

(* synthesize *)
partition Writeback mkWriteback;
  Fifo#(1, Pair#(RegIndex, Maybe#(Data))) wbQ <- mkBramLFifo;

  mkConnection(wb, wbQ.enq);

  atomic r1;
    wbIndex := wbQ.deq.first.fst;
  endatomic

  atomic r2;
    if(wbQ.deq.first.snd matches tagged Valid .d)
    begin
      regWrite := Pair{fst: wbQ.deq.first.fst, snd: d};
      wbQ.deq.deq;
    end
    else
    begin
      regWrite := Pair{fst: wbQ.deq.first.fst, snd: dataQ.first};
      wbQ.deq.deq;
      dataQ.deq;
    end
  endatomic
endpartition

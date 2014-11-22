include BramFifo;
include Fifo;

(* synthesize *)
partition Empty mkTestFifo;
  Reg#(Bit#(32)) v <- mkReg(23);
  Fifo#(2, Bit#(32)) f <- mkBramFifo;
  Reg#(Bit#(32)) count <- mkReg(0);

  atomic a0;
    count <= count + 1;
  endatomic

  atomic a;
    f.enq.enq := v;
    $display("%d enq: %d", $time, v);
    v <= v + 1;
  endatomic

  atomic b (count >= 0);
    f.deq.deq;
    $display("%d deq: %d", $time, f.deq.first);
  endatomic
endpartition

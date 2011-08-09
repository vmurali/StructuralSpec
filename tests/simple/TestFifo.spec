include Fifo;

(* synthesize *)
partition Empty mkTestFifo;
  Reg#(Bit#(32)) v <- mkReg(0);
  Fifo#(2, Bit#(32)) f <- mkFifo;

  atomic a;
    f.enq.enq := v;
    $display("enq: %d", v);
    v <= v + 1;
  endatomic

  atomic b;
    f.deq.deq;
    $display("deq: %d", f.deq.first);
  endatomic
endpartition

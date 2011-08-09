include Library;
include RegFile;

(* synthesize *)
partition Empty mkTestReg;
  Reg#(Bit#(32)) x <- mkReg(24);
  Reg#(Bit#(32)) y <- mkReg(45);

  Reg#(Bit#(32)) cycle <- mkReg(0);

  atomic b;
    cycle <= cycle + 1;
  endatomic

  atomic a if(cycle[0] == 0);
    $display("%d %d %d", cycle, x, y);
    x <= y;
    y <= x;
  endatomic
endpartition

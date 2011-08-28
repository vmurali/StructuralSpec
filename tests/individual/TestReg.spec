include Library;

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

//port Test;
//  Input#(Bit#(32)) in;
//  Output#(Bit#(32)) out;
//endport
//
//(* synthesize *)
//partition Test mkTestReg;
//  atomic a;
//    if(in == 46)
//      out := 24;
//    else
//      out := 36;
//  endatomic
//endpartition

//(* synthesize *)
//partition Empty mkTestReg;
//  Wire#(Bit#(32)) x <- mkWire;
//  Wire#(Bit#(32)) y <- mkWire;
//
//  Reg#(Bit#(32)) cycle <- mkReg(0);
//
//  atomic b;
//    cycle <= cycle + 1;
//  endatomic
//
//  atomic a if(cycle[0] == 0);
//    $display("%d %d %d", cycle, x, y);
//    x := y;
//  endatomic
//
//  atomic c;
//    y := x;
//  endatomic
//endpartition

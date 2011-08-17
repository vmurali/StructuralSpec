include Library;
include RegFile;

(* synthesize *)
partition Empty mkTestRegFile;
  Reg#(Bit#(5)) x <- mkReg(0);
  RegFile#(1, 1, 32, Bit#(5)) rf <- mkRegFileU;

  atomic a;
    rf.write[0] := Pair{fst: x, snd: x + 1};
    $display("write: %d %d", x, x + 1);
    rf.read[0].req := x;
    $display("read: %d %d", x, x, rf.read[0].resp);
    x <= x + 1;
  endatomic
endpartition

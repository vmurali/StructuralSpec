include Library;


port Client;
  ConditionalInput#(Bit#(1))  start;
endport



(* synthesize *)
partition Client mkClient;

   atomic cyclethru (start.en);
     $display("------ received signal  ------   %d",  start);
   endatomic

endpartition


port Wires;
endport


(* synthesize *)
partition Wires mkWires;
   Client c <-  mkClient;
   Reg#(Bit#(2)) state <- mkReg(0);

   atomic go  (state == 0);
     c.start := 1;
     state <= 1;
   endatomic

endpartition

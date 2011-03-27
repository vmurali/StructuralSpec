include Library;
include Types;

port Cop;
  Reverse OutputEn#(Data) write;
  Output#(Data) read;
endport

(* synthesize *)
partition mkCop implements Cop;
  Reg#(Maybe#(Data)) error <- mkRegU;

  rule r1;
    if(write.en)
    begin
      if(write.data == 1)
        $display("Passed");
      else
        $display("Failed");
      error <= tagged Valid (write.data);
    end
  endrule

  rule r2;
    if(error matches tagged Valid .e)
      $finish(truncate(e));
  endrule
endpartition

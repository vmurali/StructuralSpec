include Library;
include Types;

port Cop;
  OutputEn#(Data) write;
  Input#(Data) read;
endport

port RevCop;
  Reverse OutputEn#(Data) write;
  Output#(Data) read;
endport

(* synthesize *)
partition mkCop implements RevCop;
  rule r1;
    if(write.en)
    begin
      if(write.data == 1)
        $display("Passed\n");
      else
        $display("Failed\n");
    end
  endrule
endpartition

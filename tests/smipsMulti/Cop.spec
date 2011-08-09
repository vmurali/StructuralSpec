include Library;
include Types;

port Cop;
  ConditionalInput#(RegWrite) write;
  Output#(Data) read;
endport

(* synthesize *)
partition Cop mkCop;
  Reg#(Data) finishReg <- mkReg(0);
  Reg#(Data)  statsReg <- mkReg(0);

  atomic r1;
    if(write.en)
    begin
      if(write.index == 21)
      begin
        if(write.data == 1)
          $display("Passed");
        else
          $display("Failed");
        finishReg <= write.data;
      end
      else
        statsReg <= write.data;
    end
  endatomic

  atomic r2;
    read := statsReg;
  endatomic

  atomic r3;
    if(finishReg != 0)
      $finish(truncate(finishReg));
  endatomic
endpartition

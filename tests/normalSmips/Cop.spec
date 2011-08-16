include Library;
include Types;

port Cop;
  ConditionalInputNormal#(RegWrite) write;
  OutputNormal#(Data) read;
endport

(* synthesize *)
partition Cop mkCop;
  RegNormal#(Data) finishReg <- mkRegNormal(0);
  RegNormal#(Data)  statsReg <- mkRegNormal(0);

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

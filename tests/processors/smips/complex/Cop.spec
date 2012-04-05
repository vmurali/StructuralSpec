include Library;
include Types;

port Cop;
  ConditionalInput#(Pair#(RegIndex, Data)) write;
  Output#(Data) read;
endport

(* synthesize *)
partition Cop mkCop;
  Reg#(Data) finishReg <- mkReg(0);
  Reg#(Data)  statsReg <- mkReg(0);

  atomic r1;
    if(write.en)
    begin
      if(write.fst == 21)
      begin
        if(write.snd == 1)
          $display("Passed");
        else
          $display("Failed");
        finishReg <= write.snd;
      end
      else
        statsReg <= write.snd;
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

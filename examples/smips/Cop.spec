include Library;
include Types;

port Cop;
  Reverse OutputEn#(Tuple2#(RegIndex, Data)) write;
  Output#(Data) read;
endport

(* synthesize *)
partition mkCop implements Cop;
  Reg#(Data) finishReg <- mkReg(0);
  Reg#(Data)  statsReg <- mkReg(0);

  rule r1;
    if(write.en)
    begin
      if(tpl_1(write.data) == 21)
      begin
        if(tpl_2(write.data) == 1)
          $display("Passed");
        else
          $display("Failed");
        finishReg <= tpl_2(write.data);
      end
      else
        statsReg <= tpl_2(write.data);
    end
  endrule

  rule r2;
    read := statsReg;
  endrule

  rule r3;
    if(finishReg != 0)
      $finish(truncate(finishReg));
  endrule

  rule r4;
    specCycleDone;
  endrule
endpartition

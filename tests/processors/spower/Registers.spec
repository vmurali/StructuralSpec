include Library;
include Types;
include RegsFile;

port Registers;
  Reverse RegFileRead#(NumRegs, Data)[2] read;
  ConditionalInput#(Pair#(RegIndex, Data)) write;
endport

(* synthesize *)
partition Registers mkRegisters;
  RegFile#(2, 1, NumRegs, Data) regs <- mkRegFileU;

  atomic r1;
    for(Integer i = 0; i < 2; i = i + 1)
    begin
      if(read[i].req.en)
      begin
        regs.read[i].req := read[i].req;

        read[i].resp := read[i].req == 0?
                          0:
                          write.en && write.fst == read[i].req?
                            write.snd:
                            regs.read[i].resp;
      end
    end

    if(write.en && write.fst != 0)
    begin
      regs.write[0] := write;
      if(write.fst == 31)
      begin
        if(write.snd == 'haaa)
        begin
          $display("Passed");
          $finish(0);
        end
        else if(write.snd == 'hfff)
        begin
          $display("Failed");
          $finish(1);
        end
      end
    end
  endatomic
endpartition

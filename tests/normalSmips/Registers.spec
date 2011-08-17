include Library;
include Types;
include RegFileNormal;

port Registers;
  Reverse RegFileReadNormal#(NumRegs, Data)[2] read;
  ConditionalInputNormal#(Pair#(RegIndex, Data)) write;
endport

(* synthesize *)
partition Registers mkRegisters;
  RegFileNormal#(2, 1, NumRegs, Data) regs <- mkRegFileUNormal;

  atomic r1;
    for(Integer i = 0; i < 2; i = i + 1)
    begin
      regs.read[i].req := read[i].req;

      read[i].resp := read[i].req == 0?
                        0:
                        write.en && write.fst == read[i].req?
                          write.snd:
                          regs.read[i].resp;
    end

    if(write.en && write.fst != 0)
      regs.write[0] := write;
  endatomic
endpartition

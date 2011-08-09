include LibraryNormal;
include Types;
include RegFileNormal;

port Registers;
  Reverse RegFileReadNormal#(NumRegs, Data)[2] read;
  ConditionalInputNormal#(RegWrite) write;
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
                        write.en && write.index == read[i].req?
                          write.data:
                          regs.read[i].resp;
    end

    if(write.en && write.index != 0)
      regs.write[0] := write;
  endatomic
endpartition

include Library;
include Types;
include RegFile;
include RegistersPort;

(* synthesize *)
partition mkRegisters implements Registers;
  RegFile#(2, 1, RegIndexSz, Data) regs <- mkRegFile(0);

  atomic r1;
    for(Integer i = 0; i < 2; i = i + 1)
    begin
      regs.read[i].req := read[i].req;

      read[i].resp := read[i].req == 0?
                        0:
                        write.en && tpl_1(write.data) == read[i].req?
                          tpl_2(write.data):
                          regs.read[i].resp;
    end

    if(write.en && tpl_1(write.data) != 0)
      regs.write[0].data := write.data;
  endatomic
endpartition

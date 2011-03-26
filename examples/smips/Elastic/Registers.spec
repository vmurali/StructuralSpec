include Library;
include Types;
include RegFile;
include RegistersPort;

(* synthesize *)
partition mkRegisters implements Registers;
  RegFile#(2, 1, RegIndexSz, Data) regs <- mkRegFile(0);

  for(Integer i = 0; i < 2; i = i + 1)
  begin
    rule r1;
      regs.read[i].req := read[i].req;
    endrule

    rule r2;
      read[i].resp := tpl_1(write.data) == read[i].req?
                        tpl_2(write.data):
                        regs.read[i].resp;
    endrule
  end

  rule r3;
    if(write.en && tpl_1(write.data) != 0)
      regs.write[0].data := write.data;
    else
      regs.write[0].data.justFinish;
  endrule

  rule r4;
    specCycleDone;
  endrule
endpartition

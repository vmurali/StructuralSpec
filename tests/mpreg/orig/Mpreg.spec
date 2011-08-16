include Library;
include Types;

port Mpreg#(numeric type n, numeric type wp, numeric type rp,numeric type dw);
  ConditionalInput#(AddrT#(n)) [rp] readReq;
  Output#(DataT#(dw)) [rp] readResp;
  ConditionalInput#(WriteReq#(n,dw)) [wp] writeReq;
endport

partition Mpreg#(n,wp,rp,dw)  mkMpreg;

  Reg#(Vector#(n,DataT#(dw))) regs <- mkReg(replicate(0));

  atomic everyCycle;
    Vector#(n,DataT#(dw)) newregs  = regs;

    for(Integer i=0; i < valueof(wp); i=i+1)
      if (writeReq[i].en)
      begin
        WriteReq#(n,dw) w = writeReq[i];
        newregs[w.regnum] = w.regContent;
      end

    for(Integer i=0; i < valueof(rp); i=i+1)
      if (readReq[i].en)
      begin
        AddrT#(n) regnum = readReq[i];
        readResp[i] := newregs[regnum];
      end

    regs <= newregs;
  endatomic
endpartition


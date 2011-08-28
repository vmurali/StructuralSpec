include Library;
include Types;

include Interface;

port Mpreg#(numeric type n, numeric type wp, numeric type rp,numeric type dw);
  ConditionalInput#(AddrT#(n)) [rp] readReq;
  Output#(DataT#(dw)) [rp] readResp;
  ConditionalInput#(WriteReq#(n,dw)) [wp] writeReq;
endport

partition Mpreg#(n,wp,rp,dw)  mkMpreg provisos (Add#(wp, rp, np));
  RegNormal#(Vector#(n,DataT#(dw))) regs <- mkRegNormal(replicate(0));
  RegNormal#(Bit#(8)) localCycle <- mkRegNormal(0);

  RegNormal#(Vector#(wp, Bool)) done <- mkRegNormal(replicate(False));

  Vector#(rp, ConditionalInputFifo#(AddrT#(n))) readReqFifo = map(getConditionalInputFifo, readReq);
  Vector#(wp, ConditionalInputFifo#(WriteReq#(n, dw))) writeReqFifo = map(getConditionalInputFifo, writeReq);

  atomic a;
    $display("localCycle: %d", localCycle);
    localCycle <= localCycle + 1;
  endatomic

  atomic processReqs;
    Vector#(wp, Bool) newDone = done;
    Vector#(n, DataT#(dw)) newRegs = regs;

    $write("Beginning done: ");
    for(Integer i = 0; i < valueOf(wp); i = i + 1)
      $write("%b", done[i]);
    for(Integer i = 0; i < valueOf(rp); i = i + 1)
      $write("%b", readResp[i].consumed);
    $write("\n");

    Bool busy = False;

    for(Integer i = 0; i < valueOf(wp); i = i + 1)
      if(!busy && !done[i] && writeReqFifo[i].notEmpty)
      begin
        writeReqFifo[i].deq;
        $write("Write: %d", i);
        if(writeReqFifo[i].first matches tagged Valid .req)
        begin
          busy = True;
          newDone[i] = True;
          newRegs[req.regnum] = req.regContent;
          $write(" %d %d", req.regnum, req.regContent);
        end
        else
          newDone[i] = True;
        $write("\n");
      end

    Bool writesDone = True;
    for(Integer i = 0; i < valueOf(wp); i = i + 1)
      writesDone = writesDone && newDone[i];

    for(Integer i = 0; i < valueOf(rp); i = i + 1)
      if(!busy && writesDone && readReqFifo[i].notEmpty && readResp[i].notFull && !readResp[i].consumedBefore)
      begin
        readReqFifo[i].deq;
        $write("Read: %d", i);
        if(readReqFifo[i].first matches tagged Valid .req)
        begin
          busy = True;
          $write(" %d", req);
          readResp[i] := regs[req];
        end
        else
          readResp[i] := ?;
        $write("\n");
      end

    Bool allDone = writesDone;
    for(Integer i = 0; i < valueOf(rp); i = i + 1)
      allDone = allDone && readResp[i].consumed;

    done <= allDone? replicate(False): newDone;
    if(allDone)
      for(Integer i = 0; i < valueOf(rp); i = i + 1)
        readResp[i].reset;

    $write("End done: ");
    for(Integer i = 0; i < valueOf(wp); i = i + 1)
      $write("%b", allDone? False: newDone[i]);
    for(Integer i = 0; i < valueOf(rp); i = i + 1)
      $write("%b", allDone? False: readResp[i].consumed);
    $write("\n");

    regs <= newRegs;
  endatomic
endpartition

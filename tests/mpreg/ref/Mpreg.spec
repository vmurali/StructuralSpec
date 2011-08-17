include Library;
include Types;

include InterfaceFifo;

port Mpreg#(numeric type n, numeric type wp, numeric type rp,numeric type dw);
  ConditionalInput#(AddrT#(n)) [rp] readReq;
  Output#(DataT#(dw)) [rp] readResp;
  ConditionalInput#(WriteReq#(n,dw)) [wp] writeReq;
endport

partition Mpreg#(n,wp,rp,dw)  mkMpreg provisos (Add#(wp, rp, np));
  RegNormal#(Vector#(n,DataT#(dw))) regs <- mkRegNormal(replicate(0));
  RegNormal#(Bit#(8)) localCycle <- mkRegNormal(0);

  Vector#(rp, ConditionalInputFifo#(AddrT#(n)))         readReqFifo <- replicateM(mkConditionalInputFifo);
  Vector#(rp, OutputFifo#(1, DataT#(dw)))              readRespFifo <- replicateM(mkOutputBypassFifo);
  Vector#(wp, ConditionalInputFifo#(WriteReq#(n, dw))) writeReqFifo <- replicateM(mkConditionalInputFifo);

  RegNormal#(Vector#(TAdd#(rp, wp), Bool)) done <- mkRegNormal(replicate(False));

  atomic a;
    $display("localCycle: %d", localCycle);
    localCycle <= localCycle + 1;
  endatomic

  atomic processReqs;
    Vector#(TAdd#(rp, wp), Bool) newDone = done;
    Vector#(n, DataT#(dw)) newRegs = regs;

    $write("Beginning done: ");
    for(Integer i = 0; i < valueOf(rp) + valueOf(wp); i = i + 1)
      $write("%b", done[i]);
    $write("\n");

    Bool busy = False;

    for(Integer i = 0; i < valueOf(wp); i = i + 1)
      if(!busy && !done[i] && writeReqFifo[i].deq.notEmpty)
      begin
        writeReqFifo[i].deq.deq;
        $write("Write: %d", i);
        if(writeReqFifo[i].deq.first matches tagged Valid .req)
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

    for(Integer i = valueOf(wp); i < valueOf(wp) + valueOf(rp); i = i + 1)
      if(!busy && writesDone && !done[i] && readReqFifo[i - valueOf(wp)].deq.notEmpty && readRespFifo[i - valueOf(wp)].enq.notFull)
      begin
        readReqFifo[i - valueOf(wp)].deq.deq;
        $write("Read: %d", i);
        if(readReqFifo[i - valueOf(wp)].deq.first matches tagged Valid .req)
        begin
          busy = True;
          readRespFifo[i - valueOf(wp)].enq.enq := regs[req];
          newDone[i] = True;
          $write(" %d", req);
        end
        else
        begin
          readRespFifo[i - valueOf(wp)].enq.enq := ?;
          newDone[i] = True;
        end
        $write("\n");
      end

    Bool allDone = True;
    for(Integer i = 0; i < valueOf(rp) + valueOf(wp); i = i + 1)
      allDone = allDone && newDone[i];

    done <= allDone? replicate(False): newDone;

    $write("End done: ");
    for(Integer i = 0; i < valueOf(rp) + valueOf(wp); i = i + 1)
      $write("%b", allDone? False: newDone[i]);
    $write("\n");

    regs <= newRegs;
  endatomic

  for(Integer i = 0; i < valueOf(rp); i = i + 1)
  begin
    mkConnection(readReqFifo[i].in, readReq[i]);
    mkConnection(readRespFifo[i].out, readResp[i]);
  end

  for(Integer i = 0; i < valueOf(wp); i = i + 1)
    mkConnection(writeReqFifo[i].in, writeReq[i]);
endpartition

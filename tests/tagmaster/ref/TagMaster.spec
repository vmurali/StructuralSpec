include Library;

include InterfaceFifo;

port TagMaster#(numeric type nx, numeric type nports);
  InputPulse[nports] reqTag;
  Output#(Maybe#(Index#(nx)))[nports] getFreeTag;
  ConditionalInput#(Index#(nx))[nports] returnTag;
endport

typedef Vector#(nx, Bool) BVec#(type nx);

partition TagMaster#(nx, nports) mkTagMaster;

  //a bitvector tracks which tags are in use; initially all are set to be free
  RegNormal#(BVec#(nx)) tagsInUse <- mkRegNormal(replicate(False));

  RegNormal#(Bit#(32)) localCycle <- mkRegNormal(0);

  //Fifos to aid in receiving and sending data, according to the latency-tolerant protocol
  Vector#(nports, InputPulseFifo) reqTagFifo <- replicateM(mkInputPulseFifo);
  Vector#(nports, OutputFifo#(1, Maybe#(Index#(nx)))) getFreeTagFifo <- replicateM(mkOutputBypassFifo);
  Vector#(nports, ConditionalInputFifo#(Index#(nx))) returnTagFifo <- replicateM(mkConditionalInputFifo);

  //Making sure that we have received all the returnTags
  Vector#(nports, RegNormal#(Bool)) writeDones <- replicateM(mkRegNormal(False));

  //Making sure that we have sent all the reqTags
  Vector#(nports, RegNormal#(Bool)) readDones <- replicateM(mkRegNormal(False));

  atomic a;
    $display("-------------------------------------------------------------localCycle: %d", localCycle);
    localCycle <= localCycle + 1;
  endatomic

  function Bool allWriteDones;
    Bool ret = True;
    for(Integer i = 0; i < valueOf(nports); i = i + 1)
      ret = ret && writeDones[i];
    return ret;
  endfunction

  function Bool allReadDones;
    Bool ret = True;
    for(Integer i = 0; i < valueOf(nports); i = i + 1)
      ret = ret && readDones[i];
    return ret;
  endfunction

  for(Integer i = 0; i < valueOf(nports); i = i + 1)
  begin
    atomic b(!writeDones[i]);
      writeDones[i] <= True;
      returnTagFifo[i].deq.deq;
      let newTagsInUse = tagsInUse;
      if(returnTagFifo[i].deq.first matches tagged Valid .idx)
        newTagsInUse[idx] = False;
      tagsInUse <= newTagsInUse;
    endatomic

    atomic c(!readDones[i]);
      readDones[i] <= True;
      reqTagFifo[i].deq.deq;
      if(reqTagFifo[i].deq.first)
      begin
        if(allWriteDones)
        begin
          Maybe#(UInt#(TLog#(nx))) ret = findElem(False, tagsInUse);
          if(ret matches tagged Valid .j)
          begin
            let newTagsInUse = tagsInUse;
            newTagsInUse[j] = True;
            getFreeTagFifo[i].enq.enq := tagged Valid (pack(j));
            tagsInUse <= newTagsInUse;
          end
          else
            getFreeTagFifo[i].enq.enq := tagged Invalid;
        end
      end
      else
        getFreeTagFifo[i].enq.enq := tagged Invalid;
    endatomic
  end

  atomic d(allWriteDones && allReadDones);
    for(Integer i = 0; i < valueOf(nports); i = i + 1)
    begin
      writeDones[i] <= False;
      readDones[i] <= False;
    end
  endatomic

  for(Integer i = 0; i < valueOf(nports); i = i + 1)
  begin
    mkConnection(reqTag[i], reqTagFifo[i].in);
    mkConnection(getFreeTag[i], getFreeTagFifo[i].out);
    mkConnection(returnTag[i], returnTagFifo[i].in);
  end
endpartition

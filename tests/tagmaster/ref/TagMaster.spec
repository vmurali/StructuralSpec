include Library;

include Interface;

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

  Vector#(nports, ConditionalInputFifo#(Index#(nx))) returnTagFifo = map(getConditionalInputFifo, returnTag);

  //Making sure that we have received all the returnTags
  Vector#(nports, RegNormal#(Bool)) writeDones <- replicateM(mkRegNormal(False));

//  atomic a;
//    $display("-------------------------------------------------------------localCycle: %d", localCycle);
//    localCycle <= localCycle + 1;
//  endatomic

  function Bool allWriteDones;
    Bool ret = True;
    for(Integer i = 0; i < valueOf(nports); i = i + 1)
      ret = ret && writeDones[i];
    return ret;
  endfunction

  function Bool allReadDones;
    Bool ret = True;
    for(Integer i = 0; i < valueOf(nports); i = i + 1)
      ret = ret && getFreeTag[i].consumed;
    return ret;
  endfunction

  for(Integer i = 0; i < valueOf(nports); i = i + 1)
  begin
    atomic b(!writeDones[i]);
      writeDones[i] <= True;
      returnTagFifo[i].deq;
      let newTagsInUse = tagsInUse;
      if(returnTagFifo[i].first matches tagged Valid .idx)
        newTagsInUse[idx] = False;
      tagsInUse <= newTagsInUse;
    endatomic

    atomic c(!getFreeTag[i].consumedBefore);
      reqTag[i].deq;
      if(reqTag[i])
      begin
        if(allWriteDones)
        begin
          Maybe#(UInt#(TLog#(nx))) ret = findElem(False, tagsInUse);
          if(ret matches tagged Valid .j)
          begin
            let newTagsInUse = tagsInUse;
            newTagsInUse[j] = True;
            getFreeTag[i] := tagged Valid (pack(j));
            tagsInUse <= newTagsInUse;
          end
          else
            getFreeTag[i] := tagged Invalid;
        end
      end
      else
        getFreeTag[i] := tagged Invalid;
    endatomic
  end

  atomic d(allWriteDones && allReadDones);
    for(Integer i = 0; i < valueOf(nports); i = i + 1)
    begin
      writeDones[i] <= False;
      getFreeTag[i].reset;
    end
  endatomic
endpartition

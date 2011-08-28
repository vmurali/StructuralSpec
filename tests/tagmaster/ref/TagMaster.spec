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
  RegNormal#(BVec#(nports)) writeDones <- mkRegNormal(replicate(False));

  atomic a;
    $display("                                                             localCycle: %d", localCycle);
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
      ret = ret && getFreeTag[i].consumed;
    return ret;
  endfunction

  atomic b;
    Bool busy = False;
    BVec#(nx) newTagsInUse = tagsInUse;
    BVec#(nports) newWriteDones = writeDones;

    for(Integer i = 0; i < valueOf(nports); i = i + 1)
      if(!writeDones[i] && returnTagFifo[i].notEmpty)
      begin
        newWriteDones[i] = True;
        returnTagFifo[i].deq;
        if(returnTagFifo[i].first matches tagged Valid .idx)
        begin
          newTagsInUse[idx] = False;
          busy = True;
        end
      end

    for(Integer i = 0; i < valueOf(nports); i = i + 1)
      if(!getFreeTag[i].consumedBefore && reqTag[i].notEmpty && getFreeTag[i].notFull)
      begin
        if(reqTag[i])
        begin
          if(allWriteDones)
          begin
            reqTag[i].deq;
            Maybe#(UInt#(TLog#(nx))) ret = findElem(False, tagsInUse);
            if(ret matches tagged Valid .j)
            begin
              newTagsInUse[j] = True;
              getFreeTag[i] := tagged Valid (pack(j));
              busy = True;
            end
            else
              getFreeTag[i] := tagged Invalid;
          end
        end
        else
        begin
          reqTag[i].deq;
          getFreeTag[i] := tagged Invalid;
        end
      end

    if(allWriteDones && allReadDones)
      for(Integer i = 0; i < valueOf(nports); i = i + 1)
      begin
        newWriteDones[i] = False;
        getFreeTag[i].reset;
      end

    tagsInUse <= newTagsInUse;
    writeDones <= newWriteDones;
  endatomic
endpartition

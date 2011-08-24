include Library;

port TagMaster#(numeric type nx, numeric type nports);
  InputPulse[nports] reqTag;
  Output#(Maybe#(Index#(nx)))[nports] getFreeTag;
  ConditionalInput#(Index#(nx))[nports] returnTag;
endport

typedef Vector#(nx, Bool) BVec#(type nx);

partition TagMaster#(nx, nports) mkTagMaster;

  //a bitvector tracks which tags are in use; initially all are set to be free
  Reg#(BVec#(nx)) tagsInUse <- mkReg(replicate(False));

  //in each cycle
  atomic a;
    //first collect all returned tags if any and compose the new list of free tags
    BVec#(nx) newTagsInUse = tagsInUse;

    for(Integer i = 0; i < valueOf(nports); i = i + 1)
      if(returnTag[i].en)
        newTagsInUse[returnTag[i]] = False;

    //if there is a request, see if you can give a free tag
    for(Integer i = 0; i < valueOf(nports); i = i + 1)
      if(reqTag[i])
      begin
        Maybe#(UInt#(TLog#(nx))) ret = findElem(False, newTagsInUse);
        if(ret matches tagged Valid .j)
        begin
          newTagsInUse[j] = True;
          getFreeTag[i] := tagged Valid (pack(j));
        end
      end

    //finally update the free tags
    tagsInUse <= newTagsInUse;
  endatomic
endpartition

include Library;

include TagMaster;

include Rand;

include Fifo;

typedef 20 TagSize;

portalias TagMasterInst = TagMaster#(TagSize, 1);

(* synthesize *)
partinst TagMasterInst mkTagMasterInst = mkTagMaster;

(* synthesize *)
partition Empty mkTest;
  TagMasterInst tm <- mkTagMasterInst;

  Rand#(Bit#(32)) myrand <- mkRand(35423);

  Wire#(Bit#(32)) r <- mkWire;

  Reg#(Bit#(8)) cycle <- mkReg(0);

  //Fifo to keep track of collected tags
  Fifo#(10, Index#(TagSize)) tagq <- mkFifo;

  //get a random number and put it on a wire
  atomic getrand;
    myrand.req;
    Bit#(32) y = myrand.resp;
    r := y;
  endatomic

  //display cycles
  atomic dispCycle;
    $display("[%d] -----", cycle);
    cycle <= cycle + 1;
  endatomic

  //get a new tag and enqueue into FIFO
  atomic gettag(r[1:0] == 1);
    tm.reqTag[0];
    if(tm.getFreeTag[0] matches tagged Valid .a1)
    begin
      tagq.enq.enq := a1;
      $display("obtained tag = %d", a1);
    end
  endatomic

  //return a tag by dequeuing from Fifo
  atomic rettag(r[2] == 1);
    let tag = tagq.deq.first;
    tagq.deq.deq;
    $display("return tag = %d", tag);
    tm.returnTag[0] := tag;
  endatomic
endpartition

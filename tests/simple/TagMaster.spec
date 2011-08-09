
include Library;
//include Stypes;
include Rand;
include Fifo;

port TagMaster#(numeric type nt, numeric type nu, numeric type nx, numeric type nports);
  ConditionalInput#(Bool) reqTag;
  Output#(Maybe#(Index#(nx))) getFreeTag;
  ConditionalInput#(Index#(nx)) [nports] returnTag;
endport


partition TagMaster#(nt, nu, nx, nports)  mkTagMasterGen;

   //a bitvector tracks which tags are free; initially all are set to be free
   Reg#(Bit#(nx)) freeTags <- mkReg(~(0));
   Reg#(Index#(nx)) lastAllocTag <- mkReg(0);

   function Index#(nx) advance(Index#(nx) x); 
      return ( (x == fromInteger(valueof(nx)-1)) ? 0 : x+1);
   endfunction

   //in each cycle
   atomic everycycle;

      //first collect all returned tags if any and compose the new list of free tags
      Bit#(nx) newFreeTags = freeTags;
      for(Integer i=0; i< valueof(nports); i=i+1)
        if (returnTag[i].en) newFreeTags[returnTag[i]] = 1;

      //if there is a request, see if you can give a free tag
      if (reqTag.en) begin
         Maybe#(Index#(nx)) ret = tagged Invalid;
         if (newFreeTags != 0) begin

            Bool found = False;
     	    Index#(nx) fi = advance(lastAllocTag);

	    for(Integer i=0; i< valueof(nx); i=i+1)  if (!found) begin
                if (newFreeTags[fi] == 1) found = True; else fi = advance(fi);
            end

	    if (found) begin
               newFreeTags = newFreeTags & ~(1 << fi);
               lastAllocTag <= fi;
               ret = tagged Valid fi;
            end
         end

         getFreeTag := ret;
      end
      
      //finally, update the free tags
      freeTags <= newFreeTags;

   endatomic

endpartition


port TestTagMaster;
endport

typedef 4  Nt;
typedef 4  Nu;
typedef 8  Nx;
typedef 2  Nports;


(* synthesize *)
partition  TestTagMaster mkTagMaster;
   Rand#(Bit#(32)) myrand <- mkRand(35079);
   Reg#(Bit#(32)) r <- mkReg(467083);

   
   TagMaster#(Nt, Nu, Nx, Nports) tm <-  mkTagMasterGen;
   Reg#(Bit#(8)) cycle <- mkReg(0);

   Vector#(2, Fifo#(10, Index#(Nx))) tagq  <- replicateM(mkFifo());


   atomic getrand;
     myrand.req;
     Bit#(32) y = myrand.resp;
     r <= y;
   endatomic


   atomic my;
      cycle <= cycle + 1;
      $display("[%d] -----", cycle);
   endatomic


   for (Integer i=0; i<2; i=i+1)
   atomic gettag ((r[0] == 1) && (r[1] == fromInteger(i)));
      tm.reqTag := True;
      let aa = tm.getFreeTag;
      if (aa matches tagged Valid .a1) begin 
        tagq[i].enq.enq := a1;
	$display(" obtained tag = %d  by agent %d", a1, i);
      end
      else $display("??");
   endatomic


   for (Integer i=0; i<2; i=i+1)   
   atomic rettag ((r[2+i] == 1) && tagq[i].deq.notEmpty) ;
      Index#(Nx) tag = tagq[i].deq.first;
      tagq[i].deq.deq;
      $display("                                      returned tag  = %d by agent %d", tag, i);
      tm.returnTag[i] := tag;
   endatomic



endpartition



					   

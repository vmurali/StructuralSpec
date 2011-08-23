include Library;
include Types;
include Mpreg;



port GenericDriver#(numeric type n, numeric type wp, numeric type rp,numeric type dw);
endport

partition GenericDriver#(n, wp, rp, dw) mkGenericDriver#(function _m__#(Mpreg#(n, wp, rp, dw)) mkMpreg);

   Reg#(Bit#(32)) cycle <- mkReg(0);
   Mpreg#(n,wp,rp,dw) rf <- mkMpreg();

   atomic runclk;
     cycle <= cycle +1;
     $display("                                              cycle %d ------------------", cycle);
   endatomic

   atomic c0 (cycle == 0);
     rf.writeReq[0] := WriteReq{regnum : 2, regContent : 27};
     rf.writeReq[1] := WriteReq{regnum : 4, regContent : 47};
     rf.readReq[0] := 2;
     rf.readReq[1] := 4;
     rf.readReq[2] := 6;
     $display("                                              r2=%d, r4=%d, r6=%d", rf.readResp[0],rf.readResp[1],rf.readResp[2]);
   endatomic


   atomic c2 (cycle == 2);
     rf.writeReq[0] := WriteReq{regnum : 6, regContent : 67};
     rf.readReq[0] := 2;
     rf.readReq[1] := 4;
     rf.readReq[2] := 6;
     $display("                                              r2=%d, r4=%d, r6=%d", rf.readResp[0],rf.readResp[1],rf.readResp[2]);
   endatomic

endpartition




typedef 16 N;
typedef 3  Rp;
typedef 2  Wp;
typedef 32 Dw;

(* synthesize *)
partinst Mpreg#(N,Wp,Rp,Dw)  mkMpregA = mkMpreg;


(* synthesize *)
partinst GenericDriver#(N, Wp, Rp, Dw) mkDriver =  mkGenericDriver(mkMpregA);

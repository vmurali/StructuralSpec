import RandVerilog::*;

port Rand#(type t);
  InputPulse req;
  ConditionalOutput#(t) resp;
endport

partition Rand#(t) mkRand#(t seed) provisos(Bits#(t, tSz), Div#(tSz, 32, n));
  Vector#(n, Bit#(32)) seeds = unpack(zeroExtend(pack(seed)));
  function mkRandSeed(i) = mkRand32(seeds[i]);

  Vector#(n, Rand32) rs <- genWithM(mkRandSeed);

  function Bit#(32) getResp(Rand32 r) = r.resp;

  atomic a;
    if(req)
      for(Integer i = 0; i < valueOf(n); i = i + 1)
        rs[i].req;

    if(req)
      resp := unpack(truncate(pack(map(getResp, rs))));
  endatomic
endpartition

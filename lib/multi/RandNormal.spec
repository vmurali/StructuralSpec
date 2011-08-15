include Library;

import RandVerilog::*;

port RandNormal#(t);
  InputPulseNormal req;
  ConditionalOutputNormal#(t) resp;
endport

partition RandNormal#(t) mkRandNormal#(t seed) provisos(Bits#(t, tSz), Div#(tSz, 32, n));
  Vector#(n, Bit#(32)) seeds = unpack(zeroExtend(pack(seed)));
  function mkRandSeed(Integer i) = mkRand32Normal(seeds[i]);

  Vector#(n, Rand32Normal) rs <- genWithM(mkRandSeed);

  function Bit#(32) getResp(Rand32Normal r) = r.resp;

  atomic a;
    if(req)
      for(Integer i = 0; i < valueOf(n); i = i + 1)
        rs[i].req;

    if(req)
      resp := unpack(truncate(pack(map(getResp, rs))));
  endatomic
endpartition

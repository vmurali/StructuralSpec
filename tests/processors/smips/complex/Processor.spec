include Core;
include Memory;

(* synthesize *)
partition Empty mkProcessor;
  let core <- mkCore;
  let  mem <- mkMemory;

  mkConnection(core.mem, mem);
endpartition

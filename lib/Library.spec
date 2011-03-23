port OutputEn#(type t);
  OutputPulse en;
  Default Output#(t) data En(en);
endport

port ActionCall#(type t);
  OutputPulse en;
  Input#(Bool) guard;
  Default Output#(t) data En(en) Guard(guard);
endport

port GuardedInput#(type t);
  Input#(Bool) guard;
  Default Input#(t) data Guard(guard);
endport

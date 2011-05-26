port OutputEn#(type t);
  OutputPulse en;
  Default Output#(t) data En(en);
endport

port ConditionalOutput#(type t);
  OutputPulse en;
  Default Output#(t) data En(en);
endport

port GuardedAction#(type t);
  OutputPulse en;
  Input#(Bool) rdy;
  Default Output#(t) data En(en) Guard(rdy);
endport

port GuardedInput#(type t);
  Input#(Bool) guard;
  Default Input#(t) data Guard(guard);
endport

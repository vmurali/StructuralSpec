interface OutputEn;
  OutputPulse en;
  Default Output#(Bool) data En(en);
endinterface

interface ActionCall#(type t);
  OutputPulse en;
  Input#(Bool) guard;
  Default Output#(t) data En(en) Guard(guard);
endinterface

interface GuardedInput#(type t);
  Input#(Bool) guard;
  Default Input#(t) data Guard(guard);
endinterface

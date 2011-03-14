interface ActionCall#(type t);
  Enable en;
  Default Output#(t) data En(en);
endinterface

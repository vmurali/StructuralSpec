import Vector::*;
import Library::*;

interface Dfe;
  interface Input#(Bool) x ["r3","ej"][][]["efdk","eg"];
endinterface

interface Dfe_;
  interface Input_#(Bool) x ["r3","ej"][][]["efdk","eg"];
endinterface

module _Dfe(Tuple2#(Dfe,Dfe_));
  Input#(Bool) x ["r3","ej"][][]["efdk","eg"]_ <- _x;
  return(
    interface Dfe_i;
      interface x = tpl_1(x_);
    endinterface,
    interface Dfe_ i_;
      interface x = tpl_2(x_);
    endinterface)
endmodule


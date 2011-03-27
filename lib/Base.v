module mkWire(in, out, en);
  parameter width = 1;
  input [width-1:0] in;
  output [width-1:0] out;
  input en;

  assign out = in;
endmodule

module mkPulse(out, en);
  output out;
  input en;

  assign out = en;
endmodule

module mkReg(clk, rst_n, in, out, en);
  parameter width = 1;
  parameter init = 0;
  input [width-1:0] in;
  output reg [width-1:0] out;
  input en, clk, rst_n;

  initial
    out = init;

  always @(posedge clk)
  begin
    if(!rst_n)
      out <= init;
    else
      if(en)
        out <= in;
  end
endmodule

module mkRegU(clk, in, out, en);
  parameter width = 1;
  input [width-1:0] in;
  output reg [width-1:0] out;
  input en, clk;

  initial
    out = {((width+1))/2{2'b10}};

  always @(posedge clk)
    if(en)
      out <= in;
endmodule

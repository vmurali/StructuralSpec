module mkWire(in, out, en);
  parameter Width = 1;
  input [Width-1:0] in;
  output reg [Width-1:0] out;
  input en;

  initial
    out = {((Width+1))/2{2'b10}};

  always@(*)
    out = in;
endmodule

module mkReg(clk, rst_n, in, out, en);
  parameter Width = 1;
  parameter Init = 0;
  input [Width-1:0] in;
  output reg [Width-1:0] out;
  input en, clk, rst_n;

  initial
    out = {((Width+1))/2{2'b10}};

  always @(posedge clk)
  begin
    if(!rst_n)
      out <= Init;
    else
      if(en)
        out <= in;
  end
endmodule

module mkRegU(clk, in, out, en);
  parameter Width = 1;
  input [Width-1:0] in;
  output reg [Width-1:0] out;
  input en, clk;

  initial
    out = {((Width+1))/2{2'b10}};

  always @(posedge clk)
    if(en)
      out <= in;
endmodule

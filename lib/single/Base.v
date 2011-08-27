module mkWire(IN_WRITE, IN_EN_WRITE, OUT_READ);
  parameter width = 1;
  input [((width == 0)? 0: width-1):0] IN_WRITE;
  output [((width == 0)? 0: width-1):0] OUT_READ;
  input IN_EN_WRITE;

  assign OUT_READ = IN_WRITE;
endmodule

module mkPulse(IN_EN_WRITE, OUT_READ);
  output OUT_READ;
  input IN_EN_WRITE;

  assign OUT_READ = IN_EN_WRITE;
endmodule

module mkReg(CLK, RST_N, IN_WRITE, OUT_READ, IN_EN_WRITE);
  parameter width = 1;
  parameter init = 0;
  input [((width == 0)? 0: width-1):0] IN_WRITE;
  output reg [((width == 0)? 0: width-1):0] OUT_READ;
  input IN_EN_WRITE, CLK, RST_N;

  initial
    OUT_READ = init;

  always @(posedge CLK)
  begin
    if(!RST_N)
      OUT_READ <= init;
    else
      if(IN_EN_WRITE)
        OUT_READ <= IN_WRITE;
  end
endmodule

module mkRegU(CLK, RST_N, IN_WRITE, OUT_READ, IN_EN_WRITE);
  parameter width = 1;
  input [((width == 0)? 0: width-1):0] IN_WRITE;
  output reg [((width == 0)? 0: width-1):0] OUT_READ;
  input IN_EN_WRITE, CLK, RST_N;

  initial
    OUT_READ = {((width+1))/2{2'b10}};

  always @(posedge CLK)
    if(IN_EN_WRITE)
      OUT_READ <= IN_WRITE;
endmodule

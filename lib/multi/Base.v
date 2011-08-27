module mkWire_NO_FIFO(IN_WRITE, IN_WRITE_VALID, IN_EN_WRITE, IN_EN_WRITE_VALID, OUT_READ, OUT_READ_VALID, DONE, RESET);
  parameter width = 1;
  input [((width == 0)? 0: width-1):0] IN_WRITE;
  input IN_WRITE_VALID;
  input IN_EN_WRITE;
  input IN_EN_WRITE_VALID;
  output [((width == 0)? 0: width-1):0] OUT_READ;
  output OUT_READ_VALID;
  output DONE;
  input RESET;

  assign OUT_READ = IN_WRITE;
  assign OUT_READ_VALID = (width == 0)? 1: IN_WRITE_VALID;

  assign DONE = 1;
endmodule

module mkPulse_NO_FIFO(IN_EN_WRITE, IN_EN_WRITE_VALID, OUT_READ, OUT_READ_VALID, DONE, RESET);
  input IN_EN_WRITE;
  input IN_EN_WRITE_VALID;
  output OUT_READ;
  output OUT_READ_VALID;
  output DONE;
  input RESET;

  assign OUT_READ = IN_EN_WRITE;
  assign OUT_READ_VALID = IN_EN_WRITE_VALID;

  assign DONE = 1;
endmodule

module mkReg_NO_FIFO(CLK, RST_N, IN_WRITE, IN_WRITE_VALID, IN_EN_WRITE, IN_EN_WRITE_VALID, OUT_READ, OUT_READ_VALID, DONE, RESET);
  parameter width = 1;
  parameter init = 0;
  input CLK, RST_N;
  input [((width == 0)? 0: width-1):0] IN_WRITE;
  input IN_WRITE_VALID;
  input IN_EN_WRITE;
  input IN_EN_WRITE_VALID;
  output [((width == 0)? 0: width-1):0] OUT_READ;
  output OUT_READ_VALID;
  output DONE;
  input RESET;

  reg [((width == 0)? 0: width-1):0] data;

  initial
    data = init;

  assign OUT_READ_VALID = 1;
  assign DONE = 1;

  always@(posedge CLK)
  begin
    if(!RST_N)
      data <= init;
    else if(RESET && IN_EN_WRITE)
        data <= IN_WRITE;
  end
endmodule

module mkRegU_NO_FIFO(CLK, RST_N, IN_WRITE, IN_WRITE_VALID, IN_EN_WRITE, IN_EN_WRITE_VALID, OUT_READ, OUT_READ_VALID, DONE, RESET);
  parameter width = 1;
  parameter init = 0;
  input CLK, RST_N;
  input [((width == 0)? 0: width-1):0] IN_WRITE;
  input IN_WRITE_VALID;
  input IN_EN_WRITE;
  input IN_EN_WRITE_VALID;
  output [((width == 0)? 0: width-1):0] OUT_READ;
  output OUT_READ_VALID;
  output DONE;
  input RESET;

  reg [((width == 0)? 0: width-1):0] data;

  initial
    data = {((width+1))/2{2'b10}};

  assign OUT_READ_VALID = 1;
  assign DONE = 1;

  always@(posedge CLK)
  begin
    if(!RST_N)
      data <= {((width+1))/2{2'b10}};
    else if(RESET && IN_EN_WRITE)
        data <= IN_WRITE;
  end
endmodule

module mkWireNormal(IN_WRITE, IN_EN_WRITE, OUT_READ);
  parameter width = 1;
  input [((width == 0)? 0: width-1):0] IN_WRITE;
  input IN_EN_WRITE;
  output [((width == 0)? 0: width-1):0] OUT_READ;

  assign OUT_READ = IN_WRITE;
endmodule

module mkPulseNormal(IN_EN_WRITE, OUT_READ);
  input IN_EN_WRITE;
  output OUT_READ;

  assign OUT_READ = IN_EN_WRITE;
endmodule

module mkRegNormal(CLK, RST_N, IN_WRITE, IN_EN_WRITE, OUT_READ);
  parameter width = 1;
  parameter init = 0;
  input [((width == 0)? 0: width-1):0] IN_WRITE;
  input IN_EN_WRITE;
  output reg [((width == 0)? 0: width-1):0] OUT_READ;
  input CLK, RST_N;

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

module mkRegUNormal(CLK, IN_WRITE, IN_EN_WRITE, OUT_READ);
  parameter width = 1;
  input [((width == 0)? 0: width-1):0] IN_WRITE;
  input IN_EN_WRITE;
  output reg [((width == 0)? 0: width-1):0] OUT_READ;
  input CLK;

  initial
    OUT_READ = {((width+1))/2{2'b10}};

  always @(posedge CLK)
    if(IN_EN_WRITE)
      OUT_READ <= IN_WRITE;
endmodule

module BYPASS_FIFO(CLK, RST_N, ENQ, ENQ_VALUE, NOT_FULL, CONSUMED_BEFORE, RESET, CONSUMED, NOT_EMPTY, DEQ_VALUE, DEQ);
  parameter width = 0;
  input ENQ;
  input [width:0] ENQ_VALUE;
  output NOT_FULL;
  output CONSUMED_BEFORE;
  input RESET;
  output CONSUMED;
  output NOT_EMPTY;
  output [width:0] DEQ_VALUE;
  input DEQ;
  input CLK, RST_N;

  reg valid;
  reg consumed;
  reg [width:0] data;

  assign NOT_FULL = !valid;
  assign CONSUMED_BEFORE = consumed;
  assign CONSUMED = ENQ || consumed;
  assign NOT_EMPTY = valid || ENQ;
  assign DEQ_VALUE = ENQ? ENQ_VALUE: data;

  initial
  begin
    valid = 0;
    consumed = 0;
  end

  always@(posedge CLK)
  begin
    if(!RST_N)
    begin
      valid <= 0;
      consumed <= 0;
    end
    else
    begin
      if(RESET)
        consumed <= 0;
      else if(ENQ)
        consumed <= 1;

      if(DEQ)
        valid <= 0;
      else if(ENQ)
        valid <= 1;
    end
  end
endmodule

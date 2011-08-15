module mkWire(IN_WRITE, IN_WRITE_VALID, IN_WRITE_CONSUMED, IN_EN_WRITE, IN_EN_WRITE_VALID, IN_EN_WRITE_CONSUMED, OUT_READ, OUT_READ_VALID, OUT_READ_CONSUMED);
  parameter width = 1;
  input [((width == 0)? 0: width-1):0] IN_WRITE;
  input IN_WRITE_VALID;
  output IN_WRITE_CONSUMED;
  output [((width == 0)? 0: width-1):0] OUT_READ;
  output OUT_READ_VALID;
  input OUT_READ_CONSUMED;
  input IN_EN_WRITE;
  input IN_EN_WRITE_VALID;
  output IN_EN_WRITE_CONSUMED;

  assign OUT_READ = IN_WRITE;
  assign OUT_READ_VALID = (width == 0)? 1: IN_WRITE_VALID;
  assign IN_WRITE_CONSUMED = (width == 0)? 1: OUT_READ_CONSUMED;

  assign IN_EN_WRITE_CONSUMED = 1;
endmodule

module mkPulse(IN_EN_WRITE, IN_EN_WRITE_VALID, IN_EN_WRITE_CONSUMED, OUT_READ, OUT_READ_VALID, OUT_READ_CONSUMED);
  output OUT_READ;
  output OUT_READ_VALID;
  input OUT_READ_CONSUMED;
  input IN_EN_WRITE;
  input IN_EN_WRITE_VALID;
  output IN_EN_WRITE_CONSUMED;

  assign OUT_READ = IN_EN_WRITE;
  assign OUT_READ_VALID = IN_EN_WRITE_VALID;
  assign IN_EN_WRITE_CONSUMED = OUT_READ_CONSUMED;
endmodule

module mkReg(CLK, RST_N, IN_WRITE, IN_WRITE_VALID, IN_WRITE_CONSUMED, OUT_READ, OUT_READ_VALID, OUT_READ_CONSUMED, IN_EN_WRITE, IN_EN_WRITE_VALID, IN_EN_WRITE_CONSUMED);
  parameter width = 1;
  parameter init = 0;
  input [((width == 0)? 0: width-1):0] IN_WRITE;
  input IN_WRITE_VALID;
  output IN_WRITE_CONSUMED;
  output [((width == 0)? 0: width-1):0] OUT_READ;
  output OUT_READ_VALID;
  input OUT_READ_CONSUMED;
  input IN_EN_WRITE, CLK, RST_N;
  input IN_EN_WRITE_VALID;
  output IN_EN_WRITE_CONSUMED;

  reg [((width == 0)? 0: width-1):0] data0;
  reg [((width == 0)? 0: width-1):0] data1;
  reg valid0, done;
  wire enqCond, consumedCond, inpValid;

  initial
  begin
    valid0 = 0;
    data1 = init;
    done = 0;
  end

  assign OUT_READ_VALID = 1;
  assign OUT_READ = data1;
  assign inpValid = ((width == 0)? 1: IN_WRITE_VALID) && IN_EN_WRITE_VALID;
  assign consumedCond = inpValid && !valid0;
  assign enqCond = consumedCond && IN_EN_WRITE;
  assign IN_WRITE_CONSUMED = inpValid? !valid0: done;
  assign IN_EN_WRITE_CONSUMED = inpValid? !valid0: done;

  always@(posedge CLK)
  begin
    if(!RST_N)
    begin
      valid0 <= 0;
      data1 <= init;
      done <= 0;
    end
    else
    begin
      if(!done && consumedCond)
        done <= 1;
      if(enqCond && !((width == 0)? 1: OUT_READ_CONSUMED))
      begin
        valid0 <= 1;
        data0 <= IN_WRITE;
      end
      else if(enqCond && ((width == 0)? 1: OUT_READ_CONSUMED))
        data1 <= IN_WRITE;
      else if(!enqCond && ((width == 0)? 1: OUT_READ_CONSUMED) && valid0)
      begin
        valid0 <= 0;
        data1 <= data0;
      end
    end
  end
endmodule

module mkRegU(CLK, RST_N, IN_WRITE, IN_WRITE_VALID, IN_WRITE_CONSUMED, OUT_READ, OUT_READ_VALID, OUT_READ_CONSUMED, IN_EN_WRITE, IN_EN_WRITE_VALID, IN_EN_WRITE_CONSUMED);
  parameter width = 1;
  input [((width == 0)? 0: width-1):0] IN_WRITE;
  input IN_WRITE_VALID;
  output IN_WRITE_CONSUMED;
  output [((width == 0)? 0: width-1):0] OUT_READ;
  output OUT_READ_VALID;
  input OUT_READ_CONSUMED;
  input IN_EN_WRITE, CLK, RST_N;
  input IN_EN_WRITE_VALID;
  output IN_EN_WRITE_CONSUMED;

  reg [((width == 0)? 0: width-1):0] data0;
  reg [((width == 0)? 0: width-1):0] data1;
  reg valid0, done;
  wire enqCond, consumedCond, inpValid;

  initial
  begin
    valid0 = 0;
    done = 0;
    data1 = {((width+1))/2{2'b10}};
  end

  assign OUT_READ_VALID = 1;
  assign OUT_READ = data1;
  assign inpValid = ((width == 0)? 1: IN_WRITE_VALID) && IN_EN_WRITE_VALID;
  assign consumedCond = inpValid && !valid0;
  assign enqCond = consumedCond && IN_EN_WRITE;
  assign IN_WRITE_CONSUMED = inpValid? !valid0: done;
  assign IN_EN_WRITE_CONSUMED = inpValid? !valid0: done;

  always@(posedge CLK)
  begin
    if(!RST_N)
    begin
      valid0 <= 0;
      done <= 0;
    end
    else
    begin
      if(!done && consumedCond)
        done <= 1;
      if(enqCond && !((width == 0)? 1: OUT_READ_CONSUMED))
      begin
        valid0 <= 1;
        data0 <= IN_WRITE;
      end
      else if(enqCond && ((width == 0)? 1: OUT_READ_CONSUMED))
        data1 <= IN_WRITE;
      else if(!enqCond && ((width == 0)? 1: OUT_READ_CONSUMED) && valid0)
      begin
        valid0 <= 0;
        data1 <= data0;
      end
    end
  end
endmodule

module mkWireNormal(IN_WRITE, IN_WRITE_VALID, IN_WRITE_CONSUMED, IN_EN_WRITE, IN_EN_WRITE_VALID, IN_EN_WRITE_CONSUMED, OUT_READ, OUT_READ_VALID, OUT_READ_CONSUMED);
  parameter width = 1;
  input [((width == 0)? 0: width-1):0] IN_WRITE;
  input IN_WRITE_VALID;
  output IN_WRITE_CONSUMED;
  input IN_EN_WRITE;
  input IN_EN_WRITE_VALID;
  output IN_EN_WRITE_CONSUMED;
  output [((width == 0)? 0: width-1):0] OUT_READ;
  output OUT_READ_VALID;
  input OUT_READ_CONSUMED;

  assign IN_WRITE_CONSUMED = 1'b1;
  assign OUT_READ_VALID = 1'b1;
  assign OUT_READ = IN_WRITE;
endmodule

module mkPulseNormal(IN_EN_WRITE, IN_EN_WRITE_VALID, IN_EN_WRITE_CONSUMED, OUT_READ, OUT_READ_VALID, OUT_READ_CONSUMED);
  input IN_EN_WRITE;
  input IN_EN_WRITE_VALID;
  output IN_EN_WRITE_CONSUMED;
  output OUT_READ;
  output OUT_READ_VALID;
  input OUT_READ_CONSUMED;

  assign OUT_READ_VALID = 1'b1;
  assign IN_EN_WRITE_CONSUMED = 1'b1;

  assign OUT_READ = IN_EN_WRITE;
endmodule

module mkRegNormal(CLK, RST_N, IN_WRITE, IN_WRITE_VALID, IN_WRITE_CONSUMED, IN_EN_WRITE, IN_EN_WRITE_VALID, IN_EN_WRITE_CONSUMED, OUT_READ, OUT_READ_VALID, OUT_READ_CONSUMED);
  parameter width = 1;
  parameter init = 0;
  input [((width == 0)? 0: width-1):0] IN_WRITE;
  input IN_WRITE_VALID;
  output IN_WRITE_CONSUMED;
  input IN_EN_WRITE;
  input IN_EN_WRITE_VALID;
  output IN_EN_WRITE_CONSUMED;
  output reg [((width == 0)? 0: width-1):0] OUT_READ;
  output OUT_READ_VALID;
  input OUT_READ_CONSUMED;
  input CLK, RST_N;

  assign IN_WRITE_CONSUMED = 1'b1;
  assign IN_EN_WRITE_CONSUMED = 1'b1;
  assign OUT_READ_VALID = 1'b1;

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

module mkRegUNormal(CLK, IN_WRITE, IN_WRITE_VALID, IN_WRITE_CONSUMED, IN_EN_WRITE, IN_EN_WRITE_VALID, IN_EN_WRITE_CONSUMED, OUT_READ, OUT_READ_VALID, OUT_READ_CONSUMED);
  parameter width = 1;
  input [((width == 0)? 0: width-1):0] IN_WRITE;
  input IN_WRITE_VALID;
  output IN_WRITE_CONSUMED;
  input IN_EN_WRITE;
  input IN_EN_WRITE_VALID;
  output IN_EN_WRITE_CONSUMED;
  output reg [((width == 0)? 0: width-1):0] OUT_READ;
  output OUT_READ_VALID;
  input OUT_READ_CONSUMED;
  input CLK;

  assign IN_WRITE_CONSUMED = 1'b1;
  assign IN_EN_WRITE_CONSUMED = 1'b1;
  assign OUT_READ_CONSUMED = 1'b1;

  initial
    OUT_READ = {((width+1))/2{2'b10}};

  always @(posedge CLK)
    if(IN_EN_WRITE)
      OUT_READ <= IN_WRITE;
endmodule

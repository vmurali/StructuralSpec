module mkRand32_FIFO_OUTER_NOT_EXPOSED(CLK, REQ_WRITE, REQ_WRITE_VALID, RESP_READ, RESP_READ_VALID, DONE, RESET);
  parameter seed = 0;

  input CLK;
  input REQ_WRITE;
  input REQ_WRITE_VALID;
  output reg [31:0] RESP_READ;
  output RESP_READ_VALID;
  output DONE;
  input RESET;

  integer randomseed;
  initial
  begin
    randomseed = seed;
    RESP_READ = $random(randomseed);
  end

  assign DONE = 1;

  assign RESP_READ_VALID = REQ_WRITE_VALID;

  always@(posedge CLK)
  begin
    if(RESET && REQ_WRITE)
      RESP_READ <= $random(randomseed);
  end
endmodule

module mkRand32Normal(CLK, REQ_WRITE, RESP_READ);
  parameter seed = 0;

  input CLK;
  input REQ_WRITE;
  output reg [31:0] RESP_READ;

  integer randomseed;
  initial
  begin
    randomseed = seed;
    RESP_READ = $random(randomseed);
  end

  always@(posedge CLK)
  begin
    if(REQ_WRITE)
      RESP_READ <= $random(randomseed);
  end
endmodule

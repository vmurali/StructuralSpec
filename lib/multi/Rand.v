module mkRand32(CLK, REQ_WRITE, REQ_WRITE_VALID, REQ_WRITE_CONSUMED, RESP_READ, RESP_READ_VALID, RESP_READ_CONSUMED);
  parameter seed = 0;

  input CLK;
  input REQ_WRITE;
  input REQ_WRITE_VALID;
  output REQ_WRITE_CONSUMED;
  output reg [31:0] RESP_READ;
  output RESP_READ_VALID;
  input RESP_READ_CONSUMED;

  integer randomseed;
  initial
    randomseed = seed;

  assign RESP_READ_VALID = REQ_WRITE_VALID;
  assign RESP_READ_CONSUMED = REQ_WRITE_CONSUMED;

  always@(posedge CLK)
  begin
    if(REQ_WRITE)
      RESP_READ = $random(randomseed);
  end
endmodule

module mkRand32Normal(CLK, REQ_WRITE, RESP_READ);
  parameter seed = 0;

  input CLK;
  input REQ_WRITE;
  output reg [31:0] RESP_READ;

  integer randomseed;
  initial
    randomseed = seed;

  always@(posedge CLK)
  begin
    if(REQ_WRITE)
      RESP_READ = $random(randomseed);
  end
endmodule

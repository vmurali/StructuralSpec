module mkRand32(CLK, REQ_WRITE, RESP_READ);
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

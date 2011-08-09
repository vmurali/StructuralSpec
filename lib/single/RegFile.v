module mkRegFileVerilogLoad(CLK, RST_N, READ_REQ_WRITE, READ_RESP_READ, WRITE_EN_WRITE, WRITE_INDEX_WRITE, WRITE_DATA_WRITE);
  parameter width = 32;
  parameter n = 5;
  parameter size = 32;
  parameter file = "memory.vmh";
  parameter mode = 0;

  input CLK, RST_N;
  input [((n == 0)? 0: n-1):0] READ_REQ_WRITE;
  output [((width == 0)? 0: width-1):0] READ_RESP_READ;
  input WRITE_EN_WRITE;
  input [((n == 0)? 0: n-1):0] WRITE_INDEX_WRITE;
  input [((width == 0)? 0: width-1):0] WRITE_DATA_WRITE;

  reg [((width == 0)? 0: width-1):0] arr[0:size-1];

  initial
  begin
    if(mode == 1)
      $readmemb(file, arr, 0, size-1);
    else if(mode == 2)
      $readmemh(file, arr, 0, size-1);
  end

  assign READ_RESP_READ = arr[(size == 1)? 0: READ_REQ_WRITE];

  always@(posedge CLK)
  begin
    if(!RST_N)
    begin: allif
      if(mode == 1)
        $readmemb(file, arr, 0, size-1);
      else if(mode ==2)
        $readmemh(file, arr, 0, size-1);
    end
    else if(WRITE_EN_WRITE)
        arr[(size == 1)? 0: WRITE_INDEX_WRITE] <= WRITE_DATA_WRITE;
  end
endmodule

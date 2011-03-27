module mkRegFileLoadVerilog_(clk, rst_n, readReq, readResp, writeEn, writeIndex, writeData, dummy);
  parameter reads = 2;
  parameter writes = 1;
  parameter width = 32;
  parameter n = 5;
  parameter size = 32;
  parameter file = "memory.vmh";
  parameter binary = 0;

  input clk, rst_n, dummy;
  input [reads*n-1:0] readReq;
  output reg [reads*width-1:0] readResp;
  input [writes-1:0] writeEn;
  input [writes*n-1:0] writeIndex;
  input [writes*width-1:0] writeData;

  reg [width-1:0] arr[0:size-1];

  initial
  begin: init
    integer i;
    if(binary)
      $readmemb(file, arr, 0, size-1);
    else
      $readmemh(file, arr, 0, size-1);
  end

  always@(clk or readReq)
  begin: star
    integer i;
    for(i = 0; i < reads; i=i+1)
    begin
      readResp[((i+1)*width-1)-:width] = arr[readReq[((i+1)*n-1)-:n]];
    end
  end

  always@(posedge clk)
  begin: all
    if(!rst_n)
    begin: allif
      integer i;
      if(binary)
        $readmemb(file, arr, 0, size-1);
      else
        $readmemh(file, arr, 0, size-1);
    end
    else
    begin: allelse
      integer i;
      for(i = 0; i < writes; i=i+1)
        if(writeEn[i])
          arr[writeIndex[i]] <= writeData[((i+1)*width)-:width];
    end
  end
endmodule

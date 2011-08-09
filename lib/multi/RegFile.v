module mkRegFileVerilogLoad(CLK, RST_N,
                            READ_REQ_WRITE, READ_REQ_WRITE_VALID, READ_REQ_WRITE_CONSUMED,
                            READ_RESP_READ, READ_RESP_READ_VALID, READ_RESP_READ_CONSUMED,
                            WRITE_EN_WRITE, WRITE_EN_WRITE_VALID, WRITE_EN_WRITE_CONSUMED,
                            WRITE_INDEX_WRITE, WRITE_INDEX_WRITE_VALID, WRITE_INDEX_WRITE_CONSUMED,
                            WRITE_DATA_WRITE, WRITE_DATA_WRITE_VALID, WRITE_DATA_WRITE_CONSUMED);
  parameter width = 32;
  parameter n = 5;
  parameter size = 32;
  parameter file = "memory.vmh";
  parameter mode = 0;

  input CLK, RST_N;
  input [((n == 0)? 0: n-1):0] READ_REQ_WRITE;
  input READ_REQ_WRITE_VALID;
  output READ_REQ_WRITE_CONSUMED;
  output [((width == 0)? 0: width-1):0] READ_RESP_READ;
  output READ_RESP_READ_VALID;
  input READ_RESP_READ_CONSUMED;
  input WRITE_EN_WRITE;
  input WRITE_EN_WRITE_VALID;
  output WRITE_EN_WRITE_CONSUMED;
  input [((n == 0)? 0: n-1):0] WRITE_INDEX_WRITE;
  input WRITE_INDEX_WRITE_VALID;
  output WRITE_INDEX_WRITE_CONSUMED;
  input [((width == 0)? 0: width-1):0] WRITE_DATA_WRITE;
  input WRITE_DATA_WRITE_VALID;
  output WRITE_DATA_WRITE_CONSUMED;

  reg [((width == 0)? 0: width-1):0] arr[0:size-1];

  wire enqCond, notFull, deqEn, notEmpty;
  wire writeEn;
  wire [((n == 0)? 0: n-1):0] writeIndex;
  wire [((width == 0)? 0: width-1):0] writeData;
  wire consumedCond;

  reg en0;
  reg [((n == 0)? 0: n-1):0] index0;
  reg [((width == 0)? 0: width-1):0] data0;
  reg valid0;
  reg en1;
  reg [((n == 0)? 0: n-1):0] index1;
  reg [((width == 0)? 0: width-1):0] data1;
  reg valid1;

  assign notFull = !valid0;
  assign notEmpty = valid1;
  assign writeEn = en1;
  assign writeIndex = index1;
  assign writeData = data1;

  assign consumedCond = ((n == 0)? 1: WRITE_INDEX_WRITE_VALID) && ((width == 0)? 1: WRITE_DATA_WRITE_VALID) && notFull;
  assign enqCond = consumedCond && WRITE_EN_WRITE_VALID;
  assign WRITE_EN_WRITE_CONSUMED = consumedCond;
  assign WRITE_INDEX_WRITE_CONSUMED = consumedCond;
  assign WRITE_DATA_WRITE_CONSUMED = consumedCond;

  assign READ_RESP_READ = (writeEn && ((n == 0)? 1: writeIndex == READ_REQ_WRITE))? writeData: arr[(n == 0)? 0: READ_REQ_WRITE];
  assign READ_RESP_READ_VALID = notEmpty && ((n == 0)? 1: READ_REQ_WRITE_VALID);
  assign READ_REQ_WRITE_CONSUMED = ((width == 0)? 1: READ_RESP_READ_CONSUMED);
  assign deqEn = ((width == 0)? 1: READ_RESP_READ_CONSUMED);

  initial
  begin
    if(mode == 1)
      $readmemb(file, arr, 0, size-1);
    else if(mode == 2)
      $readmemh(file, arr, 0, size-1);
    valid1 = 1;
    en1 = 1;
    index1 = 0;
    data1 = arr[0];
    valid0 = 0;
  end

  always@(posedge CLK)
  begin
    if(!RST_N)
    begin: allif
      if(mode == 1)
        $readmemb(file, arr, 0, size-1);
      else if(mode ==2)
        $readmemh(file, arr, 0, size-1);
      valid1 <= 1;
      en1 <= 1;
      index1 <= 0;
      data1 <= arr[0];
      valid0 <= 0;
    end
    else
    begin
      if(deqEn)
        if(writeEn)
          arr[(n == 0)? 0: writeIndex] <= writeData;
      if(enqCond)
      begin
        if(deqEn || !valid1)
        begin
          valid1 <= 1;
          en1 <= WRITE_EN_WRITE;
          index1 <= WRITE_INDEX_WRITE;
          data1 <= WRITE_DATA_WRITE;
        end
        else
        begin
          valid0 <= 1;
          en0 <= WRITE_EN_WRITE;
          index0 <= WRITE_INDEX_WRITE;
          data0 <= WRITE_DATA_WRITE;
        end
      end
      else if(deqEn)
      begin
        valid0 <= 0;
        valid1 <= valid0;
        en1 <= en0;
        index1 <= index0;
        data1 <= data0;
      end
    end
  end
endmodule

module mkRegFileVerilogLoadNormal(CLK, RST_N, READ_REQ_WRITE, READ_RESP_READ, WRITE_EN_WRITE, WRITE_INDEX_WRITE, WRITE_DATA_WRITE);
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

  assign READ_RESP_READ = arr[(n == 0)? 0: READ_REQ_WRITE];

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
        arr[(n == 0)? 0: WRITE_INDEX_WRITE] <= WRITE_DATA_WRITE;
  end
endmodule

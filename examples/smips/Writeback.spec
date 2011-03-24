include Library;
include Types;

port Writeback;
  FifoDeq#(Wb) wb;
  FifoDeq#(Data) data;
  RegWrite regWrite;
endport

partition mkWriteback implements Writeback;
  rule r1;
    case (wb.data) matches
      tagged Valid .d:
        begin
          regWrite := tuple2(wb.index, d);
          wb.deq;
        end
      tagged Invalid:
        begin
          regWrite := tuple2(wb.index, data);
          wb.deq;
          data.deq;
        end
    endcase
  endrule

  rule r2;
    specCycleDone;
  endrule
endpartition

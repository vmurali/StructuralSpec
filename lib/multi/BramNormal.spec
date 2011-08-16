include Library;
include RegFileNormal;

/*****  BRAM ABSTRACTION:   
1. Bram#(R, W, N, Type)  rf <- mkBramLoad(mode, file)  -- this creates a bank of N registers, initialized or uninitialized depending on the mode, which must belong to the given type "Type".
2. mode = 0, it creates an uninitialized Bram
   mode = 1, it reads a binary file to initialize the Bram during configuration. On reset, it becomes uninitialized
   mode = 2, it reads a text hex file (VMH) to initialize the Bram during configuration. On reset, it becomes uninitialized
3. In each cycle, the module accepts
   R read requests  (i=0..R-1) of the form:  rf.read[i].req := ri,        where 0 <= ri < N, and
   W write requests (j=0..W-1) of the form:  rf.write[j].data := <rj,vj>, where 0 <= rj < N  and  all vj are of type "Type"
4. In each cycle, all read requests are processed first and then all write requests are processed. 
5. The use of expression of the form:    ....rf.read[i].resp ....,  in any cycle t,  returns the contents of register r,
   if a read request was issued for register r in cycle t-1 (which is the latest value written into that register 
   prior to cycle t-1 or else the initValue); If such a read was not issued in cycle t-1, the value returned
   is undeterminable.
6. Each write request replaces the speified register with the specified contents and the effect is visible by
   reads in the next cycle (or later cycles until it is written again).
7. When multiple writes are specified for the same register in the same cycle, the resulting value of that register
   is undeterminable.
******/

portalias BramNormal = RegFileNormal;

partition BramNormal#(1, 1, size, t) mkBramLoadSingleNormal#(Integer mode, String file) provisos(Bits#(t, tSz));
  RegFileNormal#(1, 1, size, t) rf <- mkRegFileLoadSingleNormal(mode, file);

  RegNormal#(t) valueReadReg <- mkRegNormal(?);

  atomic a;
    t valueRead = valueReadReg;
    read[0].resp := valueReadReg;
    rf.read[0].req := read[0].req;
    valueRead = rf.read[0].resp;
    if(write[0].en)
      rf.write[0] := write[0];
    valueReadReg <= valueRead;
  endatomic
endpartition

partinst BramNormal#(reads, writes, size, t) mkBramUNormal provisos(Bits#(t, tSz)) = mkMultiplePortsNormal(mkBramLoadSingleNormal(0, ""));

partinst BramNormal#(reads, writes, size, t) mkBramBinaryNormal#(String file) provisos(Bits#(t, tSz)) = mkMultiplePortsNormal(mkBramLoadSingleNormal(1, file));

partinst BramNormal#(reads, writes, size, t) mkBramVmhNormal#(String file) provisos(Bits#(t, tSz)) = mkMultiplePortsNormal(mkBramLoadSingleNormal(2, file));

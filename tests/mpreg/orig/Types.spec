
typedef Bit#(TLog#(n)) AddrT#(numeric type n);
typedef Bit#(dw) DataT#(numeric type dw);
typedef Bit#(TLog#(wp)) WritePortNum#(numeric type wp);
typedef Bit#(TLog#(rp)) ReadPortNum#(numeric type rp);
typedef struct {
   AddrT#(n) regnum;
   DataT#(dw) regContent;
} WriteReq#(numeric type n, numeric type dw)  deriving (Eq, Bits);








signature WritebackPort where {
import ¶Inout®¶;
	       
import ¶List®¶;
	      
import ¶ListN®¶;
	       
import ¶Monad®¶;
	       
import ¶PrimArray®¶;
		   
import ¶Vector®¶;
		
import Base;
	   
import ¶Connectable®¶;
		     
import HaskellLib;
		 
import Primitive;
		
import Library;
	      
import Fifo;
	   
import RegFileLoadVerilog;
			 
import RegFile;
	      
import Types;
	    
interface (WritebackPort.Writeback_ :: *) = {
    WritebackPort.wb :: Library.GuardedAction Types.Wb;
    WritebackPort.wbIndex :: Library.OutputEn_ Types.RegIndex;
    WritebackPort.dataQ :: Fifo.FifoDeq_ Types.Data;
    WritebackPort.regWrite :: Library.OutputEn_ (¶Prelude®¶.¶Tuple2®¶ Types.RegIndex Types.Data);
    WritebackPort.specCycleInputDone :: ¶Prelude®¶.¶Action®¶ {-# arg_names = [] #-};
    WritebackPort.specCycleOutputDone :: ¶Prelude®¶.¶Action®¶ {-# arg_names = [] #-};
    WritebackPort.isSupplied :: ¶Prelude®¶.¶Bool®¶ {-# arg_names = [] #-}
};
 
instance WritebackPort ¶Prelude®¶.¶PrimMakeUndefined®¶ WritebackPort.Writeback_;
									       
instance WritebackPort ¶Prelude®¶.¶PrimDeepSeqCond®¶ WritebackPort.Writeback_;
									     
instance WritebackPort ¶Prelude®¶.¶PrimMakeUninitialized®¶ WritebackPort.Writeback_;
										   
interface (WritebackPort.Writeback :: *) = {
    WritebackPort.wb :: Library.GuardedAction_ Types.Wb;
    WritebackPort.wbIndex :: Library.OutputEn Types.RegIndex;
    WritebackPort.dataQ :: Fifo.FifoDeq Types.Data;
    WritebackPort.regWrite :: Library.OutputEn (¶Prelude®¶.¶Tuple2®¶ Types.RegIndex Types.Data);
    WritebackPort.specCycleInputDone :: ¶Prelude®¶.¶Action®¶ {-# arg_names = [] #-};
    WritebackPort.specCycleOutputDone :: ¶Prelude®¶.¶Action®¶ {-# arg_names = [] #-};
    WritebackPort.isSupplied :: ¶Prelude®¶.¶Bool®¶ {-# arg_names = [] #-}
};
 
instance WritebackPort ¶Prelude®¶.¶PrimMakeUndefined®¶ WritebackPort.Writeback;
									      
instance WritebackPort ¶Prelude®¶.¶PrimDeepSeqCond®¶ WritebackPort.Writeback;
									    
instance WritebackPort ¶Prelude®¶.¶PrimMakeUninitialized®¶ WritebackPort.Writeback;
										  
WritebackPort._Writeback :: (¶Prelude®¶.¶IsModule®¶ _m__ _c__) =>
			    _m__ (¶Prelude®¶.¶Tuple2®¶ WritebackPort.Writeback_ WritebackPort.Writeback);
													
instance WritebackPort ¶Connectable®¶.¶Connectable®¶ WritebackPort.Writeback
		       WritebackPort.Writeback_;
					       
instance WritebackPort ¶Connectable®¶.¶Connectable®¶ WritebackPort.Writeback_
		       WritebackPort.Writeback;
					      
instance WritebackPort Base.Sync_ WritebackPort.Writeback;
							 
instance WritebackPort Base.Sync_ WritebackPort.Writeback_
}

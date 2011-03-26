signature MemoryPort where {
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
	   
import Types;
	    
interface (MemoryPort.Memory_ :: *) = {
    MemoryPort.instReqQ :: Fifo.FifoDeq_ Types.VAddr;
    MemoryPort.instQ :: Library.GuardedAction_ Types.Inst;
    MemoryPort.dataReqQ :: Fifo.FifoDeq_ (¶Prelude®¶.¶Tuple2®¶ Types.VAddr Types.Data);
    MemoryPort.dataQ :: Library.GuardedAction_ Types.Inst;
    MemoryPort.specCycleInputDone :: ¶Prelude®¶.¶Action®¶ {-# arg_names = [] #-};
    MemoryPort.specCycleOutputDone :: ¶Prelude®¶.¶Action®¶ {-# arg_names = [] #-};
    MemoryPort.isSupplied :: ¶Prelude®¶.¶Bool®¶ {-# arg_names = [] #-}
};
 
instance MemoryPort ¶Prelude®¶.¶PrimMakeUndefined®¶ MemoryPort.Memory_;
								      
instance MemoryPort ¶Prelude®¶.¶PrimDeepSeqCond®¶ MemoryPort.Memory_;
								    
instance MemoryPort ¶Prelude®¶.¶PrimMakeUninitialized®¶ MemoryPort.Memory_;
									  
interface (MemoryPort.Memory :: *) = {
    MemoryPort.instReqQ :: Fifo.FifoDeq Types.VAddr;
    MemoryPort.instQ :: Library.GuardedAction Types.Inst;
    MemoryPort.dataReqQ :: Fifo.FifoDeq (¶Prelude®¶.¶Tuple2®¶ Types.VAddr Types.Data);
    MemoryPort.dataQ :: Library.GuardedAction Types.Inst;
    MemoryPort.specCycleInputDone :: ¶Prelude®¶.¶Action®¶ {-# arg_names = [] #-};
    MemoryPort.specCycleOutputDone :: ¶Prelude®¶.¶Action®¶ {-# arg_names = [] #-};
    MemoryPort.isSupplied :: ¶Prelude®¶.¶Bool®¶ {-# arg_names = [] #-}
};
 
instance MemoryPort ¶Prelude®¶.¶PrimMakeUndefined®¶ MemoryPort.Memory;
								     
instance MemoryPort ¶Prelude®¶.¶PrimDeepSeqCond®¶ MemoryPort.Memory;
								   
instance MemoryPort ¶Prelude®¶.¶PrimMakeUninitialized®¶ MemoryPort.Memory;
									 
MemoryPort._Memory :: (¶Prelude®¶.¶IsModule®¶ _m__ _c__) =>
		      _m__ (¶Prelude®¶.¶Tuple2®¶ MemoryPort.Memory_ MemoryPort.Memory);
										      
instance MemoryPort ¶Connectable®¶.¶Connectable®¶ MemoryPort.Memory MemoryPort.Memory_;
										      
instance MemoryPort ¶Connectable®¶.¶Connectable®¶ MemoryPort.Memory_ MemoryPort.Memory;
										      
instance MemoryPort Base.Sync_ MemoryPort.Memory;
						
instance MemoryPort Base.Sync_ MemoryPort.Memory_
}

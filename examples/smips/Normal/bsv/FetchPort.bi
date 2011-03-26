signature FetchPort where {
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
	      
import Types;
	    
interface (FetchPort.Fetch_ :: *) = {
    FetchPort.currEpoch :: Primitive.Output_ ¶Prelude®¶.¶Bool®¶;
    FetchPort.pcQ :: Library.GuardedAction_ (¶Prelude®¶.¶Tuple2®¶ Types.VAddr ¶Prelude®¶.¶Bool®¶);
    FetchPort.instReqQ :: Library.GuardedAction_ Types.VAddr;
    FetchPort.branchPc :: Library.OutputEn Types.VAddr;
    FetchPort.specCycleInputDone :: ¶Prelude®¶.¶Action®¶ {-# arg_names = [] #-};
    FetchPort.specCycleOutputDone :: ¶Prelude®¶.¶Action®¶ {-# arg_names = [] #-};
    FetchPort.isSupplied :: ¶Prelude®¶.¶Bool®¶ {-# arg_names = [] #-}
};
 
instance FetchPort ¶Prelude®¶.¶PrimMakeUndefined®¶ FetchPort.Fetch_;
								   
instance FetchPort ¶Prelude®¶.¶PrimDeepSeqCond®¶ FetchPort.Fetch_;
								 
instance FetchPort ¶Prelude®¶.¶PrimMakeUninitialized®¶ FetchPort.Fetch_;
								       
interface (FetchPort.Fetch :: *) = {
    FetchPort.currEpoch :: Primitive.Output ¶Prelude®¶.¶Bool®¶;
    FetchPort.pcQ :: Library.GuardedAction (¶Prelude®¶.¶Tuple2®¶ Types.VAddr ¶Prelude®¶.¶Bool®¶);
    FetchPort.instReqQ :: Library.GuardedAction Types.VAddr;
    FetchPort.branchPc :: Library.OutputEn_ Types.VAddr;
    FetchPort.specCycleInputDone :: ¶Prelude®¶.¶Action®¶ {-# arg_names = [] #-};
    FetchPort.specCycleOutputDone :: ¶Prelude®¶.¶Action®¶ {-# arg_names = [] #-};
    FetchPort.isSupplied :: ¶Prelude®¶.¶Bool®¶ {-# arg_names = [] #-}
};
 
instance FetchPort ¶Prelude®¶.¶PrimMakeUndefined®¶ FetchPort.Fetch;
								  
instance FetchPort ¶Prelude®¶.¶PrimDeepSeqCond®¶ FetchPort.Fetch;
								
instance FetchPort ¶Prelude®¶.¶PrimMakeUninitialized®¶ FetchPort.Fetch;
								      
FetchPort._Fetch :: (¶Prelude®¶.¶IsModule®¶ _m__ _c__) =>
		    _m__ (¶Prelude®¶.¶Tuple2®¶ FetchPort.Fetch_ FetchPort.Fetch);
										
instance FetchPort ¶Connectable®¶.¶Connectable®¶ FetchPort.Fetch FetchPort.Fetch_;
										 
instance FetchPort ¶Connectable®¶.¶Connectable®¶ FetchPort.Fetch_ FetchPort.Fetch;
										 
instance FetchPort Base.Sync_ FetchPort.Fetch;
					     
instance FetchPort Base.Sync_ FetchPort.Fetch_
}

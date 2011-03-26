signature Library where {
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
		
interface (Library.OutputEn_ :: * -> *) t = {
    Library.en :: Primitive.OutputPulse_;
    Library.¡data¡ :: Primitive.Output_ t;
    Library._write :: t -> ¶Prelude®¶.¶Action®¶ {-# arg_names = [x] #-};
    Library.specCycleInputDone :: ¶Prelude®¶.¶Action®¶ {-# arg_names = [] #-};
    Library.specCycleOutputDone :: ¶Prelude®¶.¶Action®¶ {-# arg_names = [] #-};
    Library.isSupplied :: ¶Prelude®¶.¶Bool®¶ {-# arg_names = [] #-}
};
 
instance Library ¶Prelude®¶.¶PrimMakeUndefined®¶ (Library.OutputEn_ t);
								      
instance Library ¶Prelude®¶.¶PrimDeepSeqCond®¶ (Library.OutputEn_ t);
								    
instance Library ¶Prelude®¶.¶PrimMakeUninitialized®¶ (Library.OutputEn_ t);
									  
interface (Library.OutputEn :: * -> *) t = {
    Library.en :: Primitive.OutputPulse;
    Library.¡data¡ :: Primitive.Output t;
    Library._read :: t {-# arg_names = [] #-};
    Library.specCycleInputDone :: ¶Prelude®¶.¶Action®¶ {-# arg_names = [] #-};
    Library.specCycleOutputDone :: ¶Prelude®¶.¶Action®¶ {-# arg_names = [] #-};
    Library.isSupplied :: ¶Prelude®¶.¶Bool®¶ {-# arg_names = [] #-}
};
 
instance Library (¶Prelude®¶.¶PrimMakeUndefined®¶ t) =>
		 ¶Prelude®¶.¶PrimMakeUndefined®¶ (Library.OutputEn t);
								     
instance Library (¶Prelude®¶.¶PrimDeepSeqCond®¶ t) =>
		 ¶Prelude®¶.¶PrimDeepSeqCond®¶ (Library.OutputEn t);
								   
instance Library (¶Prelude®¶.¶PrimMakeUninitialized®¶ t) =>
		 ¶Prelude®¶.¶PrimMakeUninitialized®¶ (Library.OutputEn t);
									 
Library._OutputEn :: (¶Prelude®¶.¶Bits®¶ t _sZt, ¶Prelude®¶.¶IsModule®¶ _m__ _c__) =>
		     _m__ (¶Prelude®¶.¶Tuple2®¶ (Library.OutputEn_ t) (Library.OutputEn t));
											   
instance Library (¶Prelude®¶.¶Bits®¶ t _sZt) =>
		 ¶Connectable®¶.¶Connectable®¶ (Library.OutputEn t) (Library.OutputEn_ t);
											 
instance Library (¶Prelude®¶.¶Bits®¶ t _sZt) =>
		 ¶Connectable®¶.¶Connectable®¶ (Library.OutputEn_ t) (Library.OutputEn t);
											 
instance Library Base.Sync_ (Library.OutputEn t);
						
instance Library Base.Sync_ (Library.OutputEn_ t);
						 
interface (Library.GuardedAction_ :: * -> *) t = {
    Library.en :: Primitive.OutputPulse_;
    Library.rdy :: Primitive.Output ¶Prelude®¶.¶Bool®¶;
    Library.¡data¡ :: Primitive.Output_ t;
    Library._write :: t -> ¶Prelude®¶.¶Action®¶ {-# arg_names = [x] #-};
    Library.specCycleInputDone :: ¶Prelude®¶.¶Action®¶ {-# arg_names = [] #-};
    Library.specCycleOutputDone :: ¶Prelude®¶.¶Action®¶ {-# arg_names = [] #-};
    Library.isSupplied :: ¶Prelude®¶.¶Bool®¶ {-# arg_names = [] #-}
};
 
instance Library ¶Prelude®¶.¶PrimMakeUndefined®¶ (Library.GuardedAction_ t);
									   
instance Library ¶Prelude®¶.¶PrimDeepSeqCond®¶ (Library.GuardedAction_ t);
									 
instance Library ¶Prelude®¶.¶PrimMakeUninitialized®¶ (Library.GuardedAction_ t);
									       
interface (Library.GuardedAction :: * -> *) t = {
    Library.en :: Primitive.OutputPulse;
    Library.rdy :: Primitive.Output_ ¶Prelude®¶.¶Bool®¶;
    Library.¡data¡ :: Primitive.Output t;
    Library._read :: t {-# arg_names = [] #-};
    Library.specCycleInputDone :: ¶Prelude®¶.¶Action®¶ {-# arg_names = [] #-};
    Library.specCycleOutputDone :: ¶Prelude®¶.¶Action®¶ {-# arg_names = [] #-};
    Library.isSupplied :: ¶Prelude®¶.¶Bool®¶ {-# arg_names = [] #-}
};
 
instance Library (¶Prelude®¶.¶PrimMakeUndefined®¶ t) =>
		 ¶Prelude®¶.¶PrimMakeUndefined®¶ (Library.GuardedAction t);
									  
instance Library (¶Prelude®¶.¶PrimDeepSeqCond®¶ t) =>
		 ¶Prelude®¶.¶PrimDeepSeqCond®¶ (Library.GuardedAction t);
									
instance Library (¶Prelude®¶.¶PrimMakeUninitialized®¶ t) =>
		 ¶Prelude®¶.¶PrimMakeUninitialized®¶ (Library.GuardedAction t);
									      
Library._GuardedAction :: (¶Prelude®¶.¶Bits®¶ t _sZt, ¶Prelude®¶.¶IsModule®¶ _m__ _c__) =>
			  _m__ (¶Prelude®¶.¶Tuple2®¶ (Library.GuardedAction_ t) (Library.GuardedAction t));
													  
instance Library (¶Prelude®¶.¶Bits®¶ t _sZt) =>
		 ¶Connectable®¶.¶Connectable®¶ (Library.GuardedAction t) (Library.GuardedAction_ t);
												   
instance Library (¶Prelude®¶.¶Bits®¶ t _sZt) =>
		 ¶Connectable®¶.¶Connectable®¶ (Library.GuardedAction_ t) (Library.GuardedAction t);
												   
instance Library Base.Sync_ (Library.GuardedAction t);
						     
instance Library Base.Sync_ (Library.GuardedAction_ t);
						      
interface (Library.GuardedInput_ :: * -> *) t = {
    Library.guard :: Primitive.Output ¶Prelude®¶.¶Bool®¶;
    Library.¡data¡ :: Primitive.Output t;
    Library._read :: t {-# arg_names = [] #-};
    Library.specCycleInputDone :: ¶Prelude®¶.¶Action®¶ {-# arg_names = [] #-};
    Library.specCycleOutputDone :: ¶Prelude®¶.¶Action®¶ {-# arg_names = [] #-};
    Library.isSupplied :: ¶Prelude®¶.¶Bool®¶ {-# arg_names = [] #-}
};
 
instance Library (¶Prelude®¶.¶PrimMakeUndefined®¶ t) =>
		 ¶Prelude®¶.¶PrimMakeUndefined®¶ (Library.GuardedInput_ t);
									  
instance Library (¶Prelude®¶.¶PrimDeepSeqCond®¶ t) =>
		 ¶Prelude®¶.¶PrimDeepSeqCond®¶ (Library.GuardedInput_ t);
									
instance Library (¶Prelude®¶.¶PrimMakeUninitialized®¶ t) =>
		 ¶Prelude®¶.¶PrimMakeUninitialized®¶ (Library.GuardedInput_ t);
									      
interface (Library.GuardedInput :: * -> *) t = {
    Library.guard :: Primitive.Output_ ¶Prelude®¶.¶Bool®¶;
    Library.¡data¡ :: Primitive.Output_ t;
    Library._write :: t -> ¶Prelude®¶.¶Action®¶ {-# arg_names = [x] #-};
    Library.specCycleInputDone :: ¶Prelude®¶.¶Action®¶ {-# arg_names = [] #-};
    Library.specCycleOutputDone :: ¶Prelude®¶.¶Action®¶ {-# arg_names = [] #-};
    Library.isSupplied :: ¶Prelude®¶.¶Bool®¶ {-# arg_names = [] #-}
};
 
instance Library ¶Prelude®¶.¶PrimMakeUndefined®¶ (Library.GuardedInput t);
									 
instance Library ¶Prelude®¶.¶PrimDeepSeqCond®¶ (Library.GuardedInput t);
								       
instance Library ¶Prelude®¶.¶PrimMakeUninitialized®¶ (Library.GuardedInput t);
									     
Library._GuardedInput :: (¶Prelude®¶.¶Bits®¶ t _sZt, ¶Prelude®¶.¶IsModule®¶ _m__ _c__) =>
			 _m__ (¶Prelude®¶.¶Tuple2®¶ (Library.GuardedInput_ t) (Library.GuardedInput t));
												       
instance Library (¶Prelude®¶.¶Bits®¶ t _sZt) =>
		 ¶Connectable®¶.¶Connectable®¶ (Library.GuardedInput t) (Library.GuardedInput_ t);
												 
instance Library (¶Prelude®¶.¶Bits®¶ t _sZt) =>
		 ¶Connectable®¶.¶Connectable®¶ (Library.GuardedInput_ t) (Library.GuardedInput t);
												 
instance Library Base.Sync_ (Library.GuardedInput t);
						    
instance Library Base.Sync_ (Library.GuardedInput_ t)
}

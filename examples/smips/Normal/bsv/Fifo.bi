signature Fifo where {
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
	      
interface (Fifo.FifoDeq_ :: * -> *) t = {
    Fifo.rdy :: Primitive.Output ¶Prelude®¶.¶Bool®¶;
    Fifo.first :: Primitive.Output t;
    Fifo._read :: t {-# arg_names = [] #-};
    Fifo.deq :: Primitive.OutputPulse_;
    Fifo.specCycleInputDone :: ¶Prelude®¶.¶Action®¶ {-# arg_names = [] #-};
    Fifo.specCycleOutputDone :: ¶Prelude®¶.¶Action®¶ {-# arg_names = [] #-};
    Fifo.isSupplied :: ¶Prelude®¶.¶Bool®¶ {-# arg_names = [] #-}
};
 
instance Fifo (¶Prelude®¶.¶PrimMakeUndefined®¶ t) =>
	      ¶Prelude®¶.¶PrimMakeUndefined®¶ (Fifo.FifoDeq_ t);
							       
instance Fifo (¶Prelude®¶.¶PrimDeepSeqCond®¶ t) => ¶Prelude®¶.¶PrimDeepSeqCond®¶ (Fifo.FifoDeq_ t);
												  
instance Fifo (¶Prelude®¶.¶PrimMakeUninitialized®¶ t) =>
	      ¶Prelude®¶.¶PrimMakeUninitialized®¶ (Fifo.FifoDeq_ t);
								   
interface (Fifo.FifoDeq :: * -> *) t = {
    Fifo.rdy :: Primitive.Output_ ¶Prelude®¶.¶Bool®¶;
    Fifo.first :: Primitive.Output_ t;
    Fifo._write :: t -> ¶Prelude®¶.¶Action®¶ {-# arg_names = [x] #-};
    Fifo.deq :: Primitive.OutputPulse;
    Fifo.specCycleInputDone :: ¶Prelude®¶.¶Action®¶ {-# arg_names = [] #-};
    Fifo.specCycleOutputDone :: ¶Prelude®¶.¶Action®¶ {-# arg_names = [] #-};
    Fifo.isSupplied :: ¶Prelude®¶.¶Bool®¶ {-# arg_names = [] #-}
};
 
instance Fifo ¶Prelude®¶.¶PrimMakeUndefined®¶ (Fifo.FifoDeq t);
							      
instance Fifo ¶Prelude®¶.¶PrimDeepSeqCond®¶ (Fifo.FifoDeq t);
							    
instance Fifo ¶Prelude®¶.¶PrimMakeUninitialized®¶ (Fifo.FifoDeq t);
								  
Fifo._FifoDeq :: (¶Prelude®¶.¶Bits®¶ t _sZt, ¶Prelude®¶.¶IsModule®¶ _m__ _c__) =>
		 _m__ (¶Prelude®¶.¶Tuple2®¶ (Fifo.FifoDeq_ t) (Fifo.FifoDeq t));
									       
instance Fifo (¶Prelude®¶.¶Bits®¶ t _sZt) =>
	      ¶Connectable®¶.¶Connectable®¶ (Fifo.FifoDeq t) (Fifo.FifoDeq_ t);
									      
instance Fifo (¶Prelude®¶.¶Bits®¶ t _sZt) =>
	      ¶Connectable®¶.¶Connectable®¶ (Fifo.FifoDeq_ t) (Fifo.FifoDeq t);
									      
instance Fifo Base.Sync_ (Fifo.FifoDeq t);
					 
instance Fifo Base.Sync_ (Fifo.FifoDeq_ t);
					  
interface (Fifo.Fifo_ :: # -> * -> *) n t = {
    Fifo.enq :: Library.GuardedAction t;
    Fifo.deq :: Fifo.FifoDeq t;
    Fifo.specCycleInputDone :: ¶Prelude®¶.¶Action®¶ {-# arg_names = [] #-};
    Fifo.specCycleOutputDone :: ¶Prelude®¶.¶Action®¶ {-# arg_names = [] #-};
    Fifo.isSupplied :: ¶Prelude®¶.¶Bool®¶ {-# arg_names = [] #-}
};
 
instance Fifo (¶Prelude®¶.¶PrimMakeUndefined®¶ t) =>
	      ¶Prelude®¶.¶PrimMakeUndefined®¶ (Fifo.Fifo_ n t);
							      
instance Fifo (¶Prelude®¶.¶PrimDeepSeqCond®¶ t) => ¶Prelude®¶.¶PrimDeepSeqCond®¶ (Fifo.Fifo_ n t);
												 
instance Fifo (¶Prelude®¶.¶PrimMakeUninitialized®¶ t) =>
	      ¶Prelude®¶.¶PrimMakeUninitialized®¶ (Fifo.Fifo_ n t);
								  
interface (Fifo.Fifo :: # -> * -> *) n t = {
    Fifo.enq :: Library.GuardedAction_ t;
    Fifo.deq :: Fifo.FifoDeq_ t;
    Fifo.specCycleInputDone :: ¶Prelude®¶.¶Action®¶ {-# arg_names = [] #-};
    Fifo.specCycleOutputDone :: ¶Prelude®¶.¶Action®¶ {-# arg_names = [] #-};
    Fifo.isSupplied :: ¶Prelude®¶.¶Bool®¶ {-# arg_names = [] #-}
};
 
instance Fifo (¶Prelude®¶.¶PrimMakeUndefined®¶ t) =>
	      ¶Prelude®¶.¶PrimMakeUndefined®¶ (Fifo.Fifo n t);
							     
instance Fifo (¶Prelude®¶.¶PrimDeepSeqCond®¶ t) => ¶Prelude®¶.¶PrimDeepSeqCond®¶ (Fifo.Fifo n t);
												
instance Fifo (¶Prelude®¶.¶PrimMakeUninitialized®¶ t) =>
	      ¶Prelude®¶.¶PrimMakeUninitialized®¶ (Fifo.Fifo n t);
								 
Fifo._Fifo :: (¶Prelude®¶.¶Bits®¶ t _sZt, ¶Prelude®¶.¶IsModule®¶ _m__ _c__) =>
	      _m__ (¶Prelude®¶.¶Tuple2®¶ (Fifo.Fifo_ n t) (Fifo.Fifo n t));
									  
instance Fifo (¶Prelude®¶.¶Bits®¶ t _sZt) =>
	      ¶Connectable®¶.¶Connectable®¶ (Fifo.Fifo n t) (Fifo.Fifo_ n t);
									    
instance Fifo (¶Prelude®¶.¶Bits®¶ t _sZt) =>
	      ¶Connectable®¶.¶Connectable®¶ (Fifo.Fifo_ n t) (Fifo.Fifo n t);
									    
instance Fifo Base.Sync_ (Fifo.Fifo n t);
					
instance Fifo Base.Sync_ (Fifo.Fifo_ n t);
					 
Fifo.mkLFifo :: (¶Prelude®¶.¶Bits®¶ t tSz, ¶Prelude®¶.¶IsModule®¶ _m__ _c__) => _m__ (Fifo.Fifo n t)
}

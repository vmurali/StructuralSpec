signature Cop where {
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
	    
interface (Cop.Cop_ :: *) = {
    Cop.write :: Library.OutputEn_ Types.Data;
    Cop.read :: Primitive.Output Types.Data;
    Cop.specCycleInputDone :: ¶Prelude®¶.¶Action®¶ {-# arg_names = [] #-};
    Cop.specCycleOutputDone :: ¶Prelude®¶.¶Action®¶ {-# arg_names = [] #-};
    Cop.isSupplied :: ¶Prelude®¶.¶Bool®¶ {-# arg_names = [] #-}
};
 
instance Cop ¶Prelude®¶.¶PrimMakeUndefined®¶ Cop.Cop_;
						     
instance Cop ¶Prelude®¶.¶PrimDeepSeqCond®¶ Cop.Cop_;
						   
instance Cop ¶Prelude®¶.¶PrimMakeUninitialized®¶ Cop.Cop_;
							 
interface (Cop.Cop :: *) = {
    Cop.write :: Library.OutputEn Types.Data;
    Cop.read :: Primitive.Output_ Types.Data;
    Cop.specCycleInputDone :: ¶Prelude®¶.¶Action®¶ {-# arg_names = [] #-};
    Cop.specCycleOutputDone :: ¶Prelude®¶.¶Action®¶ {-# arg_names = [] #-};
    Cop.isSupplied :: ¶Prelude®¶.¶Bool®¶ {-# arg_names = [] #-}
};
 
instance Cop ¶Prelude®¶.¶PrimMakeUndefined®¶ Cop.Cop;
						    
instance Cop ¶Prelude®¶.¶PrimDeepSeqCond®¶ Cop.Cop;
						  
instance Cop ¶Prelude®¶.¶PrimMakeUninitialized®¶ Cop.Cop;
							
Cop._Cop :: (¶Prelude®¶.¶IsModule®¶ _m__ _c__) => _m__ (¶Prelude®¶.¶Tuple2®¶ Cop.Cop_ Cop.Cop);
											      
instance Cop ¶Connectable®¶.¶Connectable®¶ Cop.Cop Cop.Cop_;
							   
instance Cop ¶Connectable®¶.¶Connectable®¶ Cop.Cop_ Cop.Cop;
							   
instance Cop Base.Sync_ Cop.Cop;
			       
instance Cop Base.Sync_ Cop.Cop_;
				
interface (Cop.RevCop_ :: *) = {
    Cop.write :: Library.OutputEn Types.Data;
    Cop.read :: Primitive.Output_ Types.Data;
    Cop.specCycleInputDone :: ¶Prelude®¶.¶Action®¶ {-# arg_names = [] #-};
    Cop.specCycleOutputDone :: ¶Prelude®¶.¶Action®¶ {-# arg_names = [] #-};
    Cop.isSupplied :: ¶Prelude®¶.¶Bool®¶ {-# arg_names = [] #-}
};
 
instance Cop ¶Prelude®¶.¶PrimMakeUndefined®¶ Cop.RevCop_;
							
instance Cop ¶Prelude®¶.¶PrimDeepSeqCond®¶ Cop.RevCop_;
						      
instance Cop ¶Prelude®¶.¶PrimMakeUninitialized®¶ Cop.RevCop_;
							    
interface (Cop.RevCop :: *) = {
    Cop.write :: Library.OutputEn_ Types.Data;
    Cop.read :: Primitive.Output Types.Data;
    Cop.specCycleInputDone :: ¶Prelude®¶.¶Action®¶ {-# arg_names = [] #-};
    Cop.specCycleOutputDone :: ¶Prelude®¶.¶Action®¶ {-# arg_names = [] #-};
    Cop.isSupplied :: ¶Prelude®¶.¶Bool®¶ {-# arg_names = [] #-}
};
 
instance Cop ¶Prelude®¶.¶PrimMakeUndefined®¶ Cop.RevCop;
						       
instance Cop ¶Prelude®¶.¶PrimDeepSeqCond®¶ Cop.RevCop;
						     
instance Cop ¶Prelude®¶.¶PrimMakeUninitialized®¶ Cop.RevCop;
							   
Cop._RevCop :: (¶Prelude®¶.¶IsModule®¶ _m__ _c__) =>
	       _m__ (¶Prelude®¶.¶Tuple2®¶ Cop.RevCop_ Cop.RevCop);
								 
instance Cop ¶Connectable®¶.¶Connectable®¶ Cop.RevCop Cop.RevCop_;
								 
instance Cop ¶Connectable®¶.¶Connectable®¶ Cop.RevCop_ Cop.RevCop;
								 
instance Cop Base.Sync_ Cop.RevCop;
				  
instance Cop Base.Sync_ Cop.RevCop_;
				   
Cop.mkCop :: (¶Prelude®¶.¶IsModule®¶ _m__ _c__) => _m__ Cop.RevCop
}

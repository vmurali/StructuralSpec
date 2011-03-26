signature Primitive where {
import ¶Inout®¶;
	       
import ¶List®¶;
	      
import ¶ListN®¶;
	       
import ¶PrimArray®¶;
		   
import ¶Vector®¶;
		
import Base;
	   
import ¶Connectable®¶;
		     
interface (Primitive.Output_ :: * -> *) t = {
    Primitive._write :: t -> ¶Prelude®¶.¶Action®¶ {-# arg_names = [x] #-}
};
 
instance Primitive ¶Prelude®¶.¶PrimMakeUndefined®¶ (Primitive.Output_ t);
									
instance Primitive ¶Prelude®¶.¶PrimDeepSeqCond®¶ (Primitive.Output_ t);
								      
instance Primitive ¶Prelude®¶.¶PrimMakeUninitialized®¶ (Primitive.Output_ t);
									    
interface (Primitive.Output :: * -> *) t = {
    Primitive._read :: t {-# arg_names = [] #-}
};
 
instance Primitive (¶Prelude®¶.¶PrimMakeUndefined®¶ t) =>
		   ¶Prelude®¶.¶PrimMakeUndefined®¶ (Primitive.Output t);
								       
instance Primitive (¶Prelude®¶.¶PrimDeepSeqCond®¶ t) =>
		   ¶Prelude®¶.¶PrimDeepSeqCond®¶ (Primitive.Output t);
								     
instance Primitive (¶Prelude®¶.¶PrimMakeUninitialized®¶ t) =>
		   ¶Prelude®¶.¶PrimMakeUninitialized®¶ (Primitive.Output t);
									   
instance Primitive Base.Sync_ (Primitive.Output t);
						  
instance Primitive Base.Sync_ (Primitive.Output_ t);
						   
instance Primitive ¶Connectable®¶.¶Connectable®¶ (Primitive.Output_ t) (Primitive.Output t);
											   
instance Primitive ¶Connectable®¶.¶Connectable®¶ (Primitive.Output t) (Primitive.Output_ t);
											   
Primitive._Output :: (¶Prelude®¶.¶Bits®¶ t tSz, ¶Prelude®¶.¶IsModule®¶ _m__ _c__) =>
		     ¶Prelude®¶.¶Bool®¶ ->
		     Primitive.OutputPulse_ ->
		     ¶Prelude®¶.¶Bool®¶ ->
		     ¶Prelude®¶.¶Bool®¶ -> _m__ (¶Prelude®¶.¶Tuple2®¶ (Primitive.Output_ t) (Primitive.Output t));
														 
interface (Primitive.OutputPulse_ :: *) = {
    Primitive._read :: ¶Prelude®¶.¶Action®¶ {-# arg_names = [] #-}
};
 
instance Primitive ¶Prelude®¶.¶PrimMakeUndefined®¶ Primitive.OutputPulse_;
									 
instance Primitive ¶Prelude®¶.¶PrimDeepSeqCond®¶ Primitive.OutputPulse_;
								       
instance Primitive ¶Prelude®¶.¶PrimMakeUninitialized®¶ Primitive.OutputPulse_;
									     
instance Primitive Base.Sync_ Primitive.OutputPulse_;
						    
type (Primitive.OutputPulse :: *) = Primitive.Output ¶Prelude®¶.¶Bool®¶;
								       
instance Primitive ¶Connectable®¶.¶Connectable®¶ Primitive.OutputPulse_ Primitive.OutputPulse;
											     
instance Primitive ¶Connectable®¶.¶Connectable®¶ Primitive.OutputPulse Primitive.OutputPulse_;
											     
Primitive._OutputPulse :: (¶Prelude®¶.¶IsModule®¶ _m__ _c__) =>
			  ¶Prelude®¶.¶Bool®¶ ->
			  Primitive.OutputPulse_ ->
			  ¶Prelude®¶.¶Bool®¶ ->
			  ¶Prelude®¶.¶Bool®¶ -> _m__ (¶Prelude®¶.¶Tuple2®¶ Primitive.OutputPulse_ Primitive.OutputPulse)
}

signature Types where {
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
		
type (Types.VAddr :: *) = ¶Prelude®¶.¶Bit®¶ 32;
					      
type (Types.Inst :: *) = ¶Prelude®¶.¶Bit®¶ 32;
					     
type (Types.Data :: *) = ¶Prelude®¶.¶Bit®¶ 32;
					     
type (Types.RegIndexSz :: #) = 5;
				
type (Types.RegIndex :: *) = ¶Prelude®¶.¶Bit®¶ Types.RegIndexSz;
							       
type (Types.SData :: *) = ¶Prelude®¶.¶Int®¶ 32;
					      
struct (Types.Wb :: *) = {
    Types.index :: Types.RegIndex;
    Types.¡data¡ :: ¶Prelude®¶.¶Maybe®¶ Types.Data
};
 
instance Types ¶Prelude®¶.¶PrimMakeUndefined®¶ Types.Wb;
						       
instance Types ¶Prelude®¶.¶PrimDeepSeqCond®¶ Types.Wb;
						     
instance Types ¶Prelude®¶.¶PrimMakeUninitialized®¶ Types.Wb;
							   
instance Types ¶Prelude®¶.¶Bits®¶ Types.Wb 38;
					     
instance Types ¶Prelude®¶.¶Eq®¶ Types.Wb;
					
data (Types.Mem :: *) =
    Types.Load Types.Data | Types.Store (¶Prelude®¶.¶Tuple2®¶ Types.VAddr Types.Data);
										     
instance Types ¶Prelude®¶.¶PrimMakeUndefined®¶ Types.Mem;
							
instance Types ¶Prelude®¶.¶PrimDeepSeqCond®¶ Types.Mem;
						      
instance Types ¶Prelude®¶.¶PrimMakeUninitialized®¶ Types.Mem;
							    
instance Types ¶Prelude®¶.¶Bits®¶ Types.Mem 65;
					      
instance Types ¶Prelude®¶.¶Eq®¶ Types.Mem
}

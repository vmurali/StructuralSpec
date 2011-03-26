signature RegFile where {
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
	      
import RegFileLoadVerilog;
			 
interface (RegFile.RegRead_ :: # -> * -> *) n t = {
    RegFile.req :: Primitive.Output_ (¶Prelude®¶.¶Bit®¶ n);
    RegFile.resp :: Primitive.Output t;
    RegFile.specCycleInputDone :: ¶Prelude®¶.¶Action®¶ {-# arg_names = [] #-};
    RegFile.specCycleOutputDone :: ¶Prelude®¶.¶Action®¶ {-# arg_names = [] #-};
    RegFile.isSupplied :: ¶Prelude®¶.¶Bool®¶ {-# arg_names = [] #-}
};
 
instance RegFile (¶Prelude®¶.¶PrimMakeUndefined®¶ t) =>
		 ¶Prelude®¶.¶PrimMakeUndefined®¶ (RegFile.RegRead_ n t);
								       
instance RegFile (¶Prelude®¶.¶PrimDeepSeqCond®¶ t) =>
		 ¶Prelude®¶.¶PrimDeepSeqCond®¶ (RegFile.RegRead_ n t);
								     
instance RegFile (¶Prelude®¶.¶PrimMakeUninitialized®¶ t) =>
		 ¶Prelude®¶.¶PrimMakeUninitialized®¶ (RegFile.RegRead_ n t);
									   
interface (RegFile.RegRead :: # -> * -> *) n t = {
    RegFile.req :: Primitive.Output (¶Prelude®¶.¶Bit®¶ n);
    RegFile.resp :: Primitive.Output_ t;
    RegFile.specCycleInputDone :: ¶Prelude®¶.¶Action®¶ {-# arg_names = [] #-};
    RegFile.specCycleOutputDone :: ¶Prelude®¶.¶Action®¶ {-# arg_names = [] #-};
    RegFile.isSupplied :: ¶Prelude®¶.¶Bool®¶ {-# arg_names = [] #-}
};
 
instance RegFile ¶Prelude®¶.¶PrimMakeUndefined®¶ (RegFile.RegRead n t);
								      
instance RegFile ¶Prelude®¶.¶PrimDeepSeqCond®¶ (RegFile.RegRead n t);
								    
instance RegFile ¶Prelude®¶.¶PrimMakeUninitialized®¶ (RegFile.RegRead n t);
									  
RegFile._RegRead :: (¶Prelude®¶.¶Bits®¶ t _sZt, ¶Prelude®¶.¶IsModule®¶ _m__ _c__) =>
		    _m__ (¶Prelude®¶.¶Tuple2®¶ (RegFile.RegRead_ n t) (RegFile.RegRead n t));
											    
instance RegFile (¶Prelude®¶.¶Bits®¶ t _sZt) =>
		 ¶Connectable®¶.¶Connectable®¶ (RegFile.RegRead n t) (RegFile.RegRead_ n t);
											   
instance RegFile (¶Prelude®¶.¶Bits®¶ t _sZt) =>
		 ¶Connectable®¶.¶Connectable®¶ (RegFile.RegRead_ n t) (RegFile.RegRead n t);
											   
instance RegFile Base.Sync_ (RegFile.RegRead n t);
						 
instance RegFile Base.Sync_ (RegFile.RegRead_ n t);
						  
interface (RegFile.RegWrite_ :: # -> * -> *) n t = {
    RegFile.write :: Library.OutputEn_ (¶Prelude®¶.¶Tuple2®¶ (¶Prelude®¶.¶Bit®¶ n) t);
    RegFile._write :: ¶Prelude®¶.¶Tuple2®¶ (¶Prelude®¶.¶Bit®¶ n) t ->
		      ¶Prelude®¶.¶Action®¶ {-# arg_names = [x] #-};
    RegFile.specCycleInputDone :: ¶Prelude®¶.¶Action®¶ {-# arg_names = [] #-};
    RegFile.specCycleOutputDone :: ¶Prelude®¶.¶Action®¶ {-# arg_names = [] #-};
    RegFile.isSupplied :: ¶Prelude®¶.¶Bool®¶ {-# arg_names = [] #-}
};
 
instance RegFile ¶Prelude®¶.¶PrimMakeUndefined®¶ (RegFile.RegWrite_ n t);
									
instance RegFile ¶Prelude®¶.¶PrimDeepSeqCond®¶ (RegFile.RegWrite_ n t);
								      
instance RegFile ¶Prelude®¶.¶PrimMakeUninitialized®¶ (RegFile.RegWrite_ n t);
									    
interface (RegFile.RegWrite :: # -> * -> *) n t = {
    RegFile.write :: Library.OutputEn (¶Prelude®¶.¶Tuple2®¶ (¶Prelude®¶.¶Bit®¶ n) t);
    RegFile._read :: ¶Prelude®¶.¶Tuple2®¶ (¶Prelude®¶.¶Bit®¶ n) t {-# arg_names = [] #-};
    RegFile.specCycleInputDone :: ¶Prelude®¶.¶Action®¶ {-# arg_names = [] #-};
    RegFile.specCycleOutputDone :: ¶Prelude®¶.¶Action®¶ {-# arg_names = [] #-};
    RegFile.isSupplied :: ¶Prelude®¶.¶Bool®¶ {-# arg_names = [] #-}
};
 
instance RegFile (¶Prelude®¶.¶PrimMakeUndefined®¶ t) =>
		 ¶Prelude®¶.¶PrimMakeUndefined®¶ (RegFile.RegWrite n t);
								       
instance RegFile (¶Prelude®¶.¶PrimDeepSeqCond®¶ t) =>
		 ¶Prelude®¶.¶PrimDeepSeqCond®¶ (RegFile.RegWrite n t);
								     
instance RegFile (¶Prelude®¶.¶PrimMakeUninitialized®¶ t) =>
		 ¶Prelude®¶.¶PrimMakeUninitialized®¶ (RegFile.RegWrite n t);
									   
RegFile._RegWrite :: (¶Prelude®¶.¶Bits®¶ t _sZt, ¶Prelude®¶.¶IsModule®¶ _m__ _c__) =>
		     _m__ (¶Prelude®¶.¶Tuple2®¶ (RegFile.RegWrite_ n t) (RegFile.RegWrite n t));
											       
instance RegFile (¶Prelude®¶.¶Bits®¶ t _sZt) =>
		 ¶Connectable®¶.¶Connectable®¶ (RegFile.RegWrite n t) (RegFile.RegWrite_ n t);
											     
instance RegFile (¶Prelude®¶.¶Bits®¶ t _sZt) =>
		 ¶Connectable®¶.¶Connectable®¶ (RegFile.RegWrite_ n t) (RegFile.RegWrite n t);
											     
instance RegFile Base.Sync_ (RegFile.RegWrite n t);
						  
instance RegFile Base.Sync_ (RegFile.RegWrite_ n t);
						   
interface (RegFile.RegFile_ :: # -> # -> # -> * -> *) reads writes n t = {
    RegFile.read :: ¶Vector®¶.¶Vector®¶ reads (RegFile.RegRead n t);
    RegFile.write :: ¶Vector®¶.¶Vector®¶ writes
		     (Library.OutputEn (¶Prelude®¶.¶Tuple2®¶ (¶Prelude®¶.¶Bit®¶ n) t));
    RegFile.specCycleInputDone :: ¶Prelude®¶.¶Action®¶ {-# arg_names = [] #-};
    RegFile.specCycleOutputDone :: ¶Prelude®¶.¶Action®¶ {-# arg_names = [] #-};
    RegFile.isSupplied :: ¶Prelude®¶.¶Bool®¶ {-# arg_names = [] #-}
};
 
instance RegFile (¶Prelude®¶.¶PrimMakeUndefined®¶ t) =>
		 ¶Prelude®¶.¶PrimMakeUndefined®¶ (RegFile.RegFile_ reads writes n t);
										    
instance RegFile (¶Prelude®¶.¶PrimDeepSeqCond®¶ t) =>
		 ¶Prelude®¶.¶PrimDeepSeqCond®¶ (RegFile.RegFile_ reads writes n t);
										  
instance RegFile (¶Prelude®¶.¶PrimMakeUninitialized®¶ t) =>
		 ¶Prelude®¶.¶PrimMakeUninitialized®¶ (RegFile.RegFile_ reads writes n t);
											
interface (RegFile.RegFile :: # -> # -> # -> * -> *) reads writes n t = {
    RegFile.read :: ¶Vector®¶.¶Vector®¶ reads (RegFile.RegRead_ n t);
    RegFile.write :: ¶Vector®¶.¶Vector®¶ writes
		     (Library.OutputEn_ (¶Prelude®¶.¶Tuple2®¶ (¶Prelude®¶.¶Bit®¶ n) t));
    RegFile.specCycleInputDone :: ¶Prelude®¶.¶Action®¶ {-# arg_names = [] #-};
    RegFile.specCycleOutputDone :: ¶Prelude®¶.¶Action®¶ {-# arg_names = [] #-};
    RegFile.isSupplied :: ¶Prelude®¶.¶Bool®¶ {-# arg_names = [] #-}
};
 
instance RegFile (¶Prelude®¶.¶PrimMakeUndefined®¶ t) =>
		 ¶Prelude®¶.¶PrimMakeUndefined®¶ (RegFile.RegFile reads writes n t);
										   
instance RegFile (¶Prelude®¶.¶PrimDeepSeqCond®¶ t) =>
		 ¶Prelude®¶.¶PrimDeepSeqCond®¶ (RegFile.RegFile reads writes n t);
										 
instance RegFile (¶Prelude®¶.¶PrimMakeUninitialized®¶ t) =>
		 ¶Prelude®¶.¶PrimMakeUninitialized®¶ (RegFile.RegFile reads writes n t);
										       
RegFile._RegFile :: (¶Prelude®¶.¶Bits®¶ t _sZt, ¶Prelude®¶.¶IsModule®¶ _m__ _c__) =>
		    _m__ (¶Prelude®¶.¶Tuple2®¶ (RegFile.RegFile_ reads writes n t) (RegFile.RegFile reads writes n t));
														      
instance RegFile (¶Prelude®¶.¶Bits®¶ t _sZt) =>
		 ¶Connectable®¶.¶Connectable®¶ (RegFile.RegFile reads writes n t)
		 (RegFile.RegFile_ reads writes n t);
						    
instance RegFile (¶Prelude®¶.¶Bits®¶ t _sZt) =>
		 ¶Connectable®¶.¶Connectable®¶ (RegFile.RegFile_ reads writes n t)
		 (RegFile.RegFile reads writes n t);
						   
instance RegFile Base.Sync_ (RegFile.RegFile reads writes n t);
							      
instance RegFile Base.Sync_ (RegFile.RegFile_ reads writes n t);
							       
RegFile.mkRegFileLoad :: (¶Prelude®¶.¶Bits®¶ t tSz, ¶Prelude®¶.¶IsModule®¶ _m__ _c__) =>
			 ¶Prelude®¶.¶String®¶ -> ¶Prelude®¶.¶Bool®¶ -> _m__ (RegFile.RegFile reads writes n t);
													      
RegFile.mkRegFile :: (¶Prelude®¶.¶Bits®¶ t tSz, ¶Prelude®¶.¶IsModule®¶ _m__ _c__) =>
		     t -> _m__ (RegFile.RegFile reads writes n t)
}

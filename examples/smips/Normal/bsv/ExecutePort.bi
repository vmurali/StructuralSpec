signature ExecutePort where {
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
	    
import Cop;
	  
interface (ExecutePort.Execute_ :: *) = {
    ExecutePort.pcQ :: Fifo.FifoDeq_ (¶Prelude®¶.¶Tuple2®¶ Types.VAddr ¶Prelude®¶.¶Bool®¶);
    ExecutePort.instQ :: Fifo.FifoDeq_ Types.Inst;
    ExecutePort.dataReqQ :: Library.GuardedAction_ Types.Mem;
    ExecutePort.regRead :: ¶Vector®¶.¶Vector®¶ 2 (RegFile.RegRead_ Types.RegIndexSz Types.Data);
    ExecutePort.wbQ :: Library.GuardedAction_ Types.Wb;
    ExecutePort.wbIndex :: Library.OutputEn Types.RegIndex;
    ExecutePort.currEpoch :: Primitive.Output ¶Prelude®¶.¶Bool®¶;
    ExecutePort.branchPc :: Library.OutputEn_ Types.VAddr;
    ExecutePort.cop :: Cop.Cop_;
    ExecutePort.specCycleInputDone :: ¶Prelude®¶.¶Action®¶ {-# arg_names = [] #-};
    ExecutePort.specCycleOutputDone :: ¶Prelude®¶.¶Action®¶ {-# arg_names = [] #-};
    ExecutePort.isSupplied :: ¶Prelude®¶.¶Bool®¶ {-# arg_names = [] #-}
};
 
instance ExecutePort ¶Prelude®¶.¶PrimMakeUndefined®¶ ExecutePort.Execute_;
									 
instance ExecutePort ¶Prelude®¶.¶PrimDeepSeqCond®¶ ExecutePort.Execute_;
								       
instance ExecutePort ¶Prelude®¶.¶PrimMakeUninitialized®¶ ExecutePort.Execute_;
									     
interface (ExecutePort.Execute :: *) = {
    ExecutePort.pcQ :: Fifo.FifoDeq (¶Prelude®¶.¶Tuple2®¶ Types.VAddr ¶Prelude®¶.¶Bool®¶);
    ExecutePort.instQ :: Fifo.FifoDeq Types.Inst;
    ExecutePort.dataReqQ :: Library.GuardedAction Types.Mem;
    ExecutePort.regRead :: ¶Vector®¶.¶Vector®¶ 2 (RegFile.RegRead Types.RegIndexSz Types.Data);
    ExecutePort.wbQ :: Library.GuardedAction Types.Wb;
    ExecutePort.wbIndex :: Library.OutputEn_ Types.RegIndex;
    ExecutePort.currEpoch :: Primitive.Output_ ¶Prelude®¶.¶Bool®¶;
    ExecutePort.branchPc :: Library.OutputEn Types.VAddr;
    ExecutePort.cop :: Cop.Cop;
    ExecutePort.specCycleInputDone :: ¶Prelude®¶.¶Action®¶ {-# arg_names = [] #-};
    ExecutePort.specCycleOutputDone :: ¶Prelude®¶.¶Action®¶ {-# arg_names = [] #-};
    ExecutePort.isSupplied :: ¶Prelude®¶.¶Bool®¶ {-# arg_names = [] #-}
};
 
instance ExecutePort ¶Prelude®¶.¶PrimMakeUndefined®¶ ExecutePort.Execute;
									
instance ExecutePort ¶Prelude®¶.¶PrimDeepSeqCond®¶ ExecutePort.Execute;
								      
instance ExecutePort ¶Prelude®¶.¶PrimMakeUninitialized®¶ ExecutePort.Execute;
									    
ExecutePort._Execute :: (¶Prelude®¶.¶IsModule®¶ _m__ _c__) =>
			_m__ (¶Prelude®¶.¶Tuple2®¶ ExecutePort.Execute_ ExecutePort.Execute);
											    
instance ExecutePort ¶Connectable®¶.¶Connectable®¶ ExecutePort.Execute ExecutePort.Execute_;
											   
instance ExecutePort ¶Connectable®¶.¶Connectable®¶ ExecutePort.Execute_ ExecutePort.Execute;
											   
instance ExecutePort Base.Sync_ ExecutePort.Execute;
						   
instance ExecutePort Base.Sync_ ExecutePort.Execute_;
						    
ExecutePort.isSrcValid :: Types.Inst -> ¶Vector®¶.¶Vector®¶ 2 ¶Prelude®¶.¶Bool®¶;
										
ExecutePort.getDest :: Types.Inst -> Types.RegIndex;
						   
ExecutePort.isRegWrite :: Types.Inst -> ¶Prelude®¶.¶Bool®¶;
							  
ExecutePort.isLoad :: Types.Inst -> ¶Prelude®¶.¶Bool®¶;
						      
ExecutePort.isStore :: Types.Inst -> ¶Prelude®¶.¶Bool®¶;
						       
ExecutePort.isBranch :: Types.Inst -> ¶Prelude®¶.¶Bool®¶;
							
ExecutePort.copRead :: Types.Inst -> ¶Prelude®¶.¶Bool®¶;
						       
ExecutePort.copWrite :: Types.Inst -> ¶Prelude®¶.¶Bool®¶;
							
ExecutePort.aluDataAddr :: Types.Inst -> Types.Data -> Types.Data -> Types.Data;
									       
ExecutePort.branch :: Types.Inst ->
		      Types.Data ->
		      Types.Data ->
		      Types.VAddr -> ¶Prelude®¶.¶Tuple3®¶ ¶Prelude®¶.¶Bool®¶ Types.VAddr ¶Prelude®¶.¶Bool®¶
}

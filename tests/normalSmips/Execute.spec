include Library;
include Types;
include RegFileNormal;
include Cop;
include FifoNormal;

port Execute;
  FifoDeqNormal#(PcQ) pcQ;
  FifoDeqNormal#(Inst) instQ;
  FifoEnqNormal#(Mem) dataReqQ;
  RegFileReadNormal#(NumRegs, Data)[2] regRead;
  FifoEnqNormal#(Wb) wbQ;
  ConditionalInputNormal#(RegIndex) wbIndex;
  InputNormal#(Bool) currEpoch;
  ConditionalOutputNormal#(VAddr) branchPc;
  Reverse Cop cop;
endport

(* synthesize *)
partition Execute mkExecute;
  let inst = instQ.first;
  let reg1 = inst[25:21];
  let reg2 = inst[20:16];
  match tagged PcQ{epoch:.epoch, pc:.pcPlus4} = pcQ.first;

  atomic r1;
    regRead[0].req := reg1;
    regRead[1].req := reg2;
  endatomic

  atomic r2;
    if(epoch != currEpoch)
    begin
      pcQ.deq;
      instQ.deq;
    end
    else
    begin
      let stall = (isSrcValid(inst)[0] && wbIndex.en && wbIndex == reg1 || isSrcValid(inst)[1] && wbIndex.en && wbIndex == reg2);
      if(!stall)
      begin
        pcQ.deq;
        instQ.deq;
        let dest = getDest(inst);
        if(isBranch(inst))
        begin
          match {.taken, .branchTarget, .isLinked} = branch(inst, regRead[0].resp, regRead[1].resp, pcPlus4);
          if(taken)
            branchPc := branchTarget;
          if(isLinked)
            wbQ.enq := Wb{index: dest, data: tagged Valid pcPlus4};
        end
        else
        begin
          let res = aluDataAddr(inst, regRead[0].resp, regRead[1].resp);
          if(isLoad(inst))
          begin
            wbQ.enq := Wb{index: dest, data: tagged Invalid};
            dataReqQ.enq := tagged Load aluDataAddr(inst, regRead[0].resp, regRead[1].resp);
          end
          else if(isStore(inst))
          begin
            dataReqQ.enq := tagged Store (MemWrite{index: res, data: regRead[1].resp});
          end
          else if(copRead(inst))
          begin
            wbQ.enq := Wb{index: dest, data: tagged Valid (cop.read)};
          end
          else if(copWrite(inst))
          begin
            cop.write := RegWrite{index: copReg(inst), data: regRead[1].resp};
          end
          else
          begin
            wbQ.enq := Wb{index: dest, data: tagged Valid res};
          end
        end
      end
    end
  endatomic
endpartition

function Vector#(2, Bool) isSrcValid(Inst inst);
  Vector#(2, Bool) ret = newVector;
  ret[0] = inst[25:21] != 0;
  ret[1] = inst[20:16] != 0;
  case (inst[31:26])
    'b100011, 'b001001, 'b001010, 'b001011, 'b001100, 'b001101, 'b001110:
      ret[1] = False;
    'b001111, 'b000010, 'b000011:
      ret = replicate(False);
    'b000000:
      case (inst[5:0])
        'b000000, 'b000010, 'b000011:
          ret[0] = False;
        'b001000, 'b001001:
          ret[1] = False;
      endcase
    'b000110, 'b000111, 'b000001:
      ret[1] = False;
    'b010000:
      case (inst[25:21])
        'b00000:
          ret = replicate(False);
        'b00100:
          ret[0] = False;
      endcase
  endcase
  return ret;
endfunction

function RegIndex getDest(Inst inst);
  RegIndex ret = inst[15:11];
  case (inst[31:26])
    'b100011, 'b001001, 'b001010, 'b001011, 'b001100, 'b001101, 'b001110, 'b001111, 'b010000:
      ret = inst[20:16];
    'b000011:
      ret = 31;
  endcase
  return ret;
endfunction

function Bool isRegWrite(Inst inst);
  Bool ret = True;
  case (inst[31:26])
    'b101011, 'b000010, 'b000100, 'b000101, 'b000110, 'b000111, 'b000001:
      ret = False;
    'b000000:
      case (inst[5:0])
        'b001000: ret = False;
      endcase
    'b010000:
      case (inst[25:21])
        'b00100:
          ret = False;
      endcase
  endcase
  return ret;
endfunction

function Bool isLoad(Inst inst)  = inst[31:26] == 'b100011;
function Bool isStore(Inst inst) = inst[31:26] == 'b101011;

function Bool isBranch(Inst inst);
  Bool ret = False;
  case (inst[31:26])
    'b000010, 'b000011, 'b000100, 'b000101, 'b000110, 'b000111, 'b000001:
      ret = True;
    'b000000:
      case (inst[5:0])
        'b001000, 'b001001:
          ret = True;
      endcase
  endcase
  return ret;
endfunction

function Bool copRead(Inst inst)  = inst[31:26] == 'b010000 && inst[25:21] == 'b00000;
function Bool copWrite(Inst inst) = inst[31:26] == 'b010000 && inst[25:21] == 'b00100;

function RegIndex copReg(Inst inst) = inst[15:11];

function Data aluDataAddr(Inst inst, Data src1, Data src2);
  SData ssrc1 = unpack(src1);
  SData ssrc2 = unpack(src2);
  Data  imm = signExtend(inst[15:0]);
  Data zimm = zeroExtend(inst[15:0]);
  SData simm = unpack(imm);
  Int#(5) shamt = unpack(inst[10:6]);
  Int#(5) rshamt = unpack(src1[4:0]);
  Data ret = ?;
  case (inst[31:26])
    'b100011, 'b101011:
      ret = src1 + imm;
    'b001001:
      ret = src1 + imm;
    'b001010:
      ret = (ssrc1 < simm)? 1: 0;
    'b001011:
      ret = (src1 < imm)? 1: 0;
    'b001100:
      ret = src1 & zimm;
    'b001101:
      ret = src1 | zimm;
    'b001110:
      ret = src1 ^ zimm;
    'b001111:
      ret = {inst[15:0], 16'b0};
    'b000000:
      case (inst[5:0])
        'b000000:
          ret = src2 << shamt;
        'b000010:
          ret = src2 >> shamt;
        'b000011:
          ret = pack(ssrc2 >> shamt);
        'b000100:
          ret = src2 << rshamt;
        'b000110:
          ret = src2 >> rshamt;
        'b000111:
          ret = pack(ssrc2 >> rshamt);
        'b100001:
          ret = src1 + src2;
        'b100011:
          ret = src1 - src2;
        'b100100:
          ret = src1 & src2;
        'b100101:
          ret = src1 | src2;
        'b100110:
          ret = src1 ^ src2;
        'b100111:
          ret = ~(src1 | src2);
        'b101010:
          ret = (ssrc1 < ssrc2)? 1: 0;
        'b101011:
          ret = (src1 < src2)? 1: 0;
      endcase
  endcase
  return ret;
endfunction

function Tuple3#(Bool /* taken */, VAddr /* branchPc */, Bool /* link */) branch(Inst inst, Data src1, Data src2, VAddr pcPlus4);
  Data  imm = signExtend(inst[15:0]);
  SData ssrc1 = unpack(src1);
  VAddr branchTarget = pcPlus4 + (imm << 2);
  Tuple3#(Bool, VAddr, Bool) ret = tuple3(?, ?, ?);
  case (inst[31:26])
    'b000000:
      case (inst[5:0])
        'b001000:
          ret = tuple3(True, src1, False);
        'b001001:
          ret = tuple3(True, src1, True);
      endcase
    'b000010:
      ret = tuple3(True, {pcPlus4[31:28], inst[25:0], 2'b0}, False);
    'b000011:
      ret = tuple3(True, {pcPlus4[31:28], inst[25:0], 2'b0}, True);
    'b000100:
      ret = tuple3(src1 == src2, (src1 == src2)? branchTarget: pcPlus4, False);
    'b000101:
      ret = tuple3(src1 != src2, (src1 != src2)? branchTarget: pcPlus4, False);
    'b000110:
      ret = tuple3(src1 == 0 || src1[31] == 1, (src1 == 0 || src1[31] == 1)? branchTarget: pcPlus4, False);
    'b000111:
      ret = tuple3(ssrc1 > 0, (ssrc1 > 0)? branchTarget: pcPlus4, False);
    'b000001:
      case (inst[20:16])
        'b00000:
          ret = tuple3(src1[31] == 1, (src1[31] == 1)? branchTarget: pcPlus4, False);
        'b00001:
          ret = tuple3(src1[31] == 0, (src1[31] == 0)? branchTarget: pcPlus4, False);
      endcase
  endcase
  return ret;
endfunction

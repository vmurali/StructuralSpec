include Types;

function Vector#(2, Bool) isSrcsValid(Inst inst);
  Vector#(2, Bool) ret = replicate(False);
  case (inst[31:26])
    14:
      ret[0] = True;
    28, 26, 24:
      ret[0] = True;
    31:
      case (inst[10:1])
        266, 40:
        begin
          ret[0] = True;
          ret[1] = True;
        end
        28, 316, 124, 444:
        begin
          ret[0] = True;
          ret[1] = True;
        end
        24, 792, 536:
        begin
          ret[0] = True;
          ret[1] = True;
        end
      endcase
    32:
      ret[0] = True;
    36:
    begin
      ret[0] = True;
      ret[1] = True;
    end
    11:
      ret[0] = True;
  endcase
  return ret;
endfunction

function Vector#(2, RegIndex) getSrcsInst(Inst inst);
  Vector#(2, RegIndex) ret = newVector;
  ret[1] = inst[15:11];
  case (inst[31:26])
    14:
      ret[0] = inst[20:16];
    28, 26, 24:
      ret[0] = inst[25:21];
    31:
      case (inst[10:1])
        266, 40:
          ret[0] = inst[20:16];
        28, 316, 124, 444:
          ret[0] = inst[25:21];
        24, 792, 536:
          ret[0] = inst[25:21];
      endcase
    32:
      ret[0] = inst[20:16];
    36:
    begin
      ret[0] = inst[20:16];
      ret[1] = inst[25:21];
    end
    11:
      ret[0] = inst[20:16];
  endcase
  return ret;
endfunction

function Bool isReadCr0(Inst inst) = inst[31:26] == 16;

function Bool isReadLr(Inst inst);
  Bool ret = False;
  case(inst[31:26])
    19:
      ret = True;
    31:
      case(inst[10:1])
        339:
          ret = True;
      endcase
  endcase
  return ret;
endfunction

function Bool isDestValid(Inst inst);
  Bool ret = False;
  Bool src1 = inst[25:21] != 0;
  Bool src2 = inst[20:16] != 0;
  case (inst[31:26])
    14:
      ret = src1;
    28, 26, 24:
      ret = src2;
    31:
      case (inst[10:1])
        266, 40:
          ret = src1;
        28, 316, 124, 444:
          ret = src2;
        24, 792, 536:
          ret = src2;
        339:
          ret = src1;
      endcase
    32:
      ret = src1;
  endcase
  return ret;
endfunction

function RegIndex getDest(Inst inst);
  RegIndex ret = ?;
  case (inst[31:26])
    14:
      ret = inst[25:21];
    28, 26, 24:
      ret = inst[20:16];
    31:
      case (inst[10:1])
        266, 40:
          ret = inst[25:21];
        28, 316, 124, 444:
          ret = inst[20:16];
        24, 792, 536:
          ret = inst[20:16];
        339:
          ret = inst[25:21];
      endcase
    32:
      ret = inst[25:21];
  endcase
  return ret;
endfunction

function Bool isWriteCr0(Inst inst) = inst[31:26] == 11;

function Bit#(4) condWrite(Inst inst, Data src1);
  Data imm = signExtend(inst[15:0]);
  SData simm = unpack(imm);
  SData ssrc1 = unpack(src1);
  if(ssrc1 < simm)
    return 4'b0001;
  else if(ssrc1 > simm)
    return 4'b0010;
  else return 4'b0100;
endfunction

function Bool isWriteLr(Inst inst) = inst[31:26] == 18 && inst[0] == 1;

function Data aluResultMemAddr(Inst inst, Vector#(2, Data) srcs, VAddr lr);
  Data src1 = srcs[0];
  Data src2 = srcs[1];
  SData ssrc1 = unpack(src1);
  Data  imm = signExtend(inst[15:0]);
  Data zimm = zeroExtend(inst[15:0]);
  Data ret = ?;
  case (inst[31:26])
    14:
      ret = src1 + imm;
    28:
      ret = src1 & zimm;
    26:
      ret = src1 ^ zimm;
    24:
      ret = src1 | zimm;
    31:
      case (inst[10:1])
        266:
          ret = src1 + src2;
        40:
          ret = src2 - src1;

        28:
          ret = src1 & src2;
        316:
          ret = src1 ^ src2;
        124:
          ret = ~(src1 | src2);
        444:
          ret = src1 | src2;

        24:
          ret = src1 << src2[5:0];
        536:
          ret = src1 >> src2[5:0];
        792:
          ret = pack(ssrc1 >> src2[5:0]);

        339:
          ret = lr;
      endcase
    32, 36:
      ret = src1 + imm;
  endcase
  return ret;
endfunction

function Tuple2#(Bool /* taken */, VAddr /* branchPc */) branch(Inst inst, Data src1, Data linkReg, Bit#(4) cr, VAddr pc);
  Data imm = signExtend({inst[15:2], 2'b0});
  VAddr branchTarget = pc + imm;
  Bool trueCond = cr[inst[17:16]] == 1;
  Tuple2#(Bool, VAddr) ret = tuple2(?, ?);
  case (inst[31:26])
    18:
      ret = tuple2(True, branchTarget);
    19:
      ret = tuple2(True, linkReg);
    16:
      case(inst[25:21])
        12:
          ret = tuple2(trueCond, branchTarget);
        4:
          ret = tuple2(!trueCond, branchTarget);
      endcase
  endcase
  return ret;
endfunction

function Bool isLoad(Inst inst)  = inst[31:26] == 32;
function Bool isStore(Inst inst) = inst[31:26] == 36;

function Bool isBranch(Inst inst);
  Bool ret = False;
  case (inst[31:26])
    18, 19, 16:
      ret = True;
  endcase
  return ret;
endfunction

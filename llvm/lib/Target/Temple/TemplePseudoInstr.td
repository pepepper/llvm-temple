include "TempleInstrInfo.td"

def brtarget : Operand<OtherVT> {
  let EncoderMethod = "getBREncoding";
}

multiclass ALUI<SDNode ops> {
  def r : Pseudo<(outs GPR:$ra), // $ra = $rb ops $rc
                 (ins GPR:$rb, GPR:$rc),
                 [(set GPR:$ra, (ops GPR:$rb, GPR:$rc))]>;
}

let Defs = [FLAG], mayStore = 0, mayLoad = 0, hasSideEffects = 0 in {
  /// Arithmetic Instructions
  // IR "add" defined in include/llvm/Target/TargetSelectionDAG.td, line 315
  // (def add).
  defm ADD : ALUI<add>;
  defm SUB : ALUI<sub>;
  /// Bitwise Instructions
  defm SRL : ALUI<srl>; // $ra = $rb >> $rc (unsigned)
  defm SLL : ALUI<shl>;// $ra = $rb << $rc
  defm SRA : ALUI<sra>; // $ra = $rb >> $rc (signed)             

  defm AND : ALUI<and>;
  defm OR : ALUI<or>;
  defm XOR : ALUI<xor>;

  def MOV : Pseudo<(outs GPR:$ra),
                  (ins i16imm:$imm16),
                  [(set GPR:$ra,imm:$imm16)]>;
}

def STORE : Pseudo<(outs), // *($rb) = $ra
                   (ins GPR:$ra, GPR:$rb),
                   [(store GPR:$ra, GPR:$rb)]>;
def LOAD : Pseudo<(outs GPR:$rb), // $rb = *($ra)
                  (ins GPR:$ra),
                  [(set GPR:$rb, (load GPR:$ra))]>;

/// Branch Instructions
let mayStore = 0, mayLoad = 0, hasSideEffects = 0 in {
  let isBranch = 1, isBarrier = 1, isTerminator = 1 in {
    let  isIndirectBranch = 1 in {
    def BRIND : Pseudo<(outs),
                        (ins GPR:$ra),
                        [(brind GPR:$ra)]>,
                 PseudoInstExpansion<(JL ZERO, 7, GPR:$ra)>;
    }
    def BR : Pseudo<(outs),
                        (ins brtarget:$ra),
                        [(br bb:$ra)]>;
  }
}

let isReturn = 1, isTerminator = 1, isBarrier = 1 in {
  def RET : Pseudo<(outs), (ins), [(Templeret)]>,
            PseudoInstExpansion<(JL ZERO, 7, RA)>;
}

let isCall = 1, isBarrier = 1,hasSideEffects = 0,
    mayStore = 0, mayLoad = 0 in{
  def CALL : Pseudo<(outs), (ins GPR:$ra), [(Templecall GPR:$ra)]>,
             PseudoInstExpansion<(JL RA, 7, GPR:$ra)>;
}

let isBranch = 1, isIndirectBranch = 1, isTerminator = 1, hasSideEffects = 0, mayLoad = 0,
    mayStore = 0 in {
  def BEQ : Pseudo<(outs),
                   (ins brtarget:$rc, GPR:$ra, GPR:$rb),
                   []>; // branch $rc if $ra=$rb
  def BNE : Pseudo<(outs),
                   (ins brtarget:$rc, GPR:$ra, GPR:$rb),
                   []>; // branch $rc if $ra!=$rb
  def BLT : Pseudo<(outs),
                   (ins brtarget:$rc, GPR:$ra, GPR:$rb),
                   []>; // branch $rc if $ra<$rb
  def BGE : Pseudo<(outs),
                   (ins brtarget:$rc, GPR:$ra, GPR:$rb),
                   []>; // branch $rc if $ra>=$rb
  def BLTU : Pseudo<(outs),
                    (ins brtarget:$rc, GPR:$ra, GPR:$rb),
                    []>; // branch $rc if $ra<$rb
  def BGEU : Pseudo<(outs),
                    (ins brtarget:$rc, GPR:$ra, GPR:$rb),
                    []>; // branch $rc if $ra>=$rb
}

multiclass SETCC<CondCode cond> {
  def r : Pseudo<(outs GPR:$ra), // $ra = $rb ops $rc
                 (ins GPR:$rb, GPR:$rc),
                 [(set GPR:$ra,(setcc GPR:$rb,GPR:$rc,cond))]>;
}

let hasSideEffects = 0, mayLoad = 0, mayStore = 0 in {
  def SELECT : Pseudo<(outs GPR:$ra),
                     (ins GPR:$rb, GPR:$rc, GPR:$rd),
                     []>; // $ra = $rb ? $rc : $rd
  defm SETEQ : SETCC<SETEQ>;
  defm SETNE : SETCC<SETNE>;
  defm SETLT : SETCC<SETLT>;
  defm SETGE : SETCC<SETGE>;
  defm SETULT : SETCC<SETULT>;
  defm SETUGE : SETCC<SETUGE>;

}

def : Pat<(select GPR:$rb, GPR:$rc, GPR:$rd), (SELECT GPR:$rb, GPR:$rc, GPR:$rd)>;

// Pessimistically assume the stack pointer will be clobbered
let Defs = [SP], Uses = [SP] in {
  def ADJCALLSTACKDOWN : Pseudo<(outs),
                                (ins i16imm:$amt1, i16imm:$amt2),
                                [(CallSeqStart timm:$amt1, timm:$amt2)]>;
  def ADJCALLSTACKUP : Pseudo<(outs),
                              (ins i16imm:$amt1, i16imm:$amt2),
                              [(CallSeqEnd timm:$amt1, timm:$amt2)]>;
} // Defs = [SP], Uses = [SP]

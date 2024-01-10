include "TempleInstrInfo.td"

multiclass ALUI<SDNode ops> {
  def r : Pseudo<(outs GPR:$ra), // $ra = $rb ops $rc
                 (ins GPR:$rb, GPR:$rc),
                 [(set GPR:$ra, (ops GPR:$rb, GPR:$rc))]>;

  def i : Pseudo<(outs GPR:$ra), // $ra = $rb ops imm16
                 (ins GPR:$rb, i16imm:$imm16),
                 [(set GPR:$ra, (ops GPR:$rb, imm:$imm16))]>;
}

let Defs = [FLAG], mayStore = 0, mayLoad = 0, hasSideEffects = 0 in {
  /// Arithmetic Instructions
  // IR "add" defined in include/llvm/Target/TargetSelectionDAG.td, line 315
  // (def add).
  defm ADD : ALUI<add>;
  defm SUB : ALUI<sub>;
  /// Bitwise Instructions
  // 即値右シフト以外のシフトはLLVMが展開してくれることを祈る
  def SRLi : Pseudo<(outs GPR:$ra), // $ra = $rb >> $rc
                    (ins GPR:$rb, i16imm:$imm16),
                    [(set GPR:$ra, (srl GPR:$rb, imm:$imm16))]>;

  defm AND : ALUI<and>;
  defm OR : ALUI<or>;
  defm XOR : ALUI<xor>;
}

def STORE : Pseudo<(outs), // *($rb) = $ra
                   (ins GPR:$ra, GPR:$rb),
                   [(store GPR:$ra, GPR:$rb)]>;
def LOAD : Pseudo<(outs GPR:$rb), // $rb = *($ra)
                  (ins GPR:$ra),
                  [(set GPR:$rb, (load GPR:$ra))]>;

/// Branch Instructions
let isIndirectBranch = 1, mayStore = 0, mayLoad = 0, hasSideEffects = 0 in {
  let Uses = [FLAG] in {
    def JLI : Pseudo<(outs), (ins cond3:$cond, GPR:$rb)>,
              PseudoInstExpansion<(JL ALLONE, cond3:$cond, GPR:$rb)>;
    def JLZero : Pseudo<(outs GPR:$ra),
                        (ins GPR:$rb)>,
                 PseudoInstExpansion<(JL GPR:$ra, 4, GPR:$rb)>;
    def JLNeg : Pseudo<(outs GPR:$ra),
                       (ins GPR:$rb)>,
                PseudoInstExpansion<(JL GPR:$ra, 2, GPR:$rb)>;
    def JLCarry : Pseudo<(outs GPR:$ra),
                         (ins GPR:$rb)>,
                  PseudoInstExpansion<(JL GPR:$ra, 1, GPR:$rb)>;
  }
  let isBarrier = 1, isTerminator = 1 in {
    def JLAlways : Pseudo<(outs GPR:$ra),
                          (ins GPR:$rb)>,
                   PseudoInstExpansion<(JL GPR:$ra, 7, GPR:$rb)>;
    def branch : Pseudo<(outs),
                        (ins GPR:$ra),
                        [(brind GPR:$ra)]>,
                 PseudoInstExpansion<(JL ZERO, 7, GPR:$ra)>;
  }
  def JLNever : Pseudo<(outs GPR:$ra),
                       (ins)>,
                PseudoInstExpansion<(JL GPR:$ra, 0, ZERO)>;
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

let isIndirectBranch = 1, isTerminator = 1, hasSideEffects = 0, mayLoad = 0,
    mayStore = 0 in {
  def BEQ : Pseudo<(outs),
                   (ins GPR:$ra, GPR:$rb, GPR:$rc),
                   []>; // branch $rc if $ra=$rb
  def BNE : Pseudo<(outs),
                   (ins GPR:$ra, GPR:$rb, GPR:$rc),
                   []>; // branch $rc if $ra!=$rb
  def BLT : Pseudo<(outs),
                   (ins GPR:$ra, GPR:$rb, GPR:$rc),
                   []>; // branch $rc if $ra<$rb
  def BGE : Pseudo<(outs),
                   (ins GPR:$ra, GPR:$rb, GPR:$rc),
                   []>; // branch $rc if $ra>=$rb
  def BLTU : Pseudo<(outs),
                    (ins GPR:$ra, GPR:$rb, GPR:$rc),
                    []>; // branch $rc if $ra<$rb
  def BGEU : Pseudo<(outs),
                    (ins GPR:$ra, GPR:$rb, GPR:$rc),
                    []>; // branch $rc if $ra>=$rb
}

let hasSideEffects = 0, mayLoad = 0, mayStore = 0 in {
  def SELECT : Pseudo<(outs GPR:$ra),
                     (ins GPR:$rb, GPR:$rc, GPR:$rd),
                     []>; // $ra = $rb ? $rc : $rd
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

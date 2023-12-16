//===- TempleInstrInfo.td - Target Description for Temple Target -*- tablegen
//-*-=//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains the Temple implementation of the TargetInstrInfo class.
//
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// Temple profiles and nodes
//===----------------------------------------------------------------------===//
def SDT_TempleCall : SDTypeProfile<0, -1, [SDTCisVT<0, i16>]>;
def SDT_TempleCp : SDTypeProfile<0, 2, [SDTCisSameAs<0, 1>]>;
def SDT_TempleSelectCC
    : SDTypeProfile<1, 3,
                    [SDTCisSameAs<0, 1>, SDTCisSameAs<1, 2>, SDTCisVT<3, i16>]>;
def SDT_TempleBrCC
    : SDTypeProfile<0, 2, [SDTCisVT<0, OtherVT>, SDTCisVT<1, i16>]>;
    
def Templeret : SDNode<"TempleISD::RET", SDTNone,
                       [SDNPHasChain]>;

def Templeadd : SDNode<"TempleISD::ADD", SDTIntBinOp, [SDNPOutGlue]>;
def Templenor : SDNode<"TempleISD::NOR", SDTIntBinOp, [SDNPOutGlue]>;

def Templeselectcc
    : SDNode<"TempleISD::SELECT_CC", SDT_TempleSelectCC, [SDNPInGlue]>;
def Templebrcc
    : SDNode<"TempleISD::BR_CC", SDT_TempleBrCC, [SDNPHasChain, SDNPInGlue]>;
def Templecall
    : SDNode<"TempleISD::CALL", SDT_TempleCall,
             [SDNPHasChain, SDNPOptInGlue, SDNPOutGlue, SDNPVariadic]>;
//===----------------------------------------------------------------------===//
// Instruction format superclass
//===----------------------------------------------------------------------===//

include "TempleInstrFormats.td"

//===----------------------------------------------------------------------===//
// Temple Operand, Complex Patterns and Transformations Definitions.
//===----------------------------------------------------------------------===//
// Instruction operand types
class ImmAsmOperand<string prefix, int width, string suffix> : AsmOperandClass {
  let Name = prefix #"Imm" #width #suffix;
  let RenderMethod = "addImmOperands";
  let DiagnosticType = "Invalid" #Name;
}

class SImmAsmOperand<int width, string suffix = "">
    : ImmAsmOperand<"S", width, suffix> {
}

class UImmAsmOperand<int width, string suffix = "">
    : ImmAsmOperand<"U", width, suffix> {
}
// Signed Operand
def simm16 : Operand<i32> {
  let ParserMatchClass = SImmAsmOperand<16>;
  let DecoderMethod = "decodeSimm16";
}

def cond3 : Operand<i32> {
  let ParserMatchClass = UImmAsmOperand<3>;
  let DecoderMethod = "decodeCond";
}

// Node immediate fits as 16-bit sign extended on target immediate.
// e.g. addi, andi
def immSExt16 : PatLeaf<(imm), [{ return isInt<16>(N->getSExtValue()); }]>;

def immcond3 : PatLeaf<(imm), [{ return isInt<3>(N->getZExtValue()); }]>;

// Temple Address Mode! SDNode frameindex could possibily be a match
// since load and store instructions from stack used it.
def addr
    : ComplexPattern<iPTR, 2, "SelectAddr", [frameindex], [SDNPWantParent]>;

def SDT_TempleCallSeqStart
    : SDCallSeqStart<[SDTCisVT<0, i16>, SDTCisVT<1, i16>]>;
def SDT_TempleCallSeqEnd : SDCallSeqEnd<[SDTCisVT<0, i16>, SDTCisVT<1, i16>]>;
def CallSeqStart : SDNode<"ISD::CALLSEQ_START", SDT_TempleCallSeqStart,
                          [SDNPHasChain, SDNPOutGlue]>;
def CallSeqEnd : SDNode<"ISD::CALLSEQ_END", SDT_TempleCallSeqEnd,
                        [SDNPHasChain, SDNPOptInGlue, SDNPOutGlue]>;

//===----------------------------------------------------------------------===//
// Pattern fragment for load/store
//===----------------------------------------------------------------------===//

class AlignedLoad<PatFrag Node> :
  PatFrag<(ops node:$ptr), (Node node:$ptr), [{
  LoadSDNode *LD = cast<LoadSDNode>(N);
  return LD->getMemoryVT().getSizeInBits() / 8 <= LD->getAlignment();
}]>;

class AlignedStore<PatFrag Node> :
  PatFrag<(ops node:$val, node:$ptr), (Node node:$val, node:$ptr), [{
  StoreSDNode *SD = cast<StoreSDNode>(N);
  return SD->getMemoryVT().getSizeInBits() / 8 <= SD->getAlignment();
}]>;

// Load/Store PatFrags.
// def load_a          : AlignedLoad<load>;
// def store_a         : AlignedStore<store>;

//===----------------------------------------------------------------------===//
// Instruction definition
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// Temple Instructions
//===----------------------------------------------------------------------===//
/// Bitwise Instructions
def NOR : FF<0x00, (outs),
             (ins GPRIn
              : $ra),
             "nor\t$ra", IIAlu, [(set ACC, (Templenor ACC, GPRIn
                                            : $ra))]>;

/// Arithmetic Instructions
// IR "add" defined in include/llvm/Target/TargetSelectionDAG.td, line 315
// (def add).
def ADD : FF<0x01, (outs),
             (ins GPRIn
              : $ra),
             "add\t$ra", IIAlu, [(set ACC, (Templeadd ACC, GPRIn
                                            : $ra))]>;
/// Load and Store Instructions
///  aligned
let canFoldAsLoad = 1, mayStore = 0, mayLoad = 1 in {
  def LD : FF<0x02, (outs),
              (ins GPRIn
               : $ra),
              "ld\t$ra", IILoad, [(set ACC, (load GPRIn
                                             : $ra))]>;
///  def LDi : Pseudo<(outs GPROut:$ra),(ins i16imm:$imm),[(set $ra,(load $imm))]>;
}



let mayStore = 0, mayLoad = 0 in {
  def MOVE : FF<0x03,
                (outs GPROut
                 : $ra),
                (ins), "move\t$ra", IIAlu, [(set GPROut
                                             : $ra, ACC)]>;
}

def SD : FF<0x4, (outs),
            (ins GPRIn
             : $ra),
            "sd\t$ra", IIStore, [(store ACC, GPRIn
                                  : $ra)]>;

let mayStore = 0, mayLoad = 0,hasSideEffects = 0 in {
  def SETI : FI<0x05, (outs),
                (ins simm16
                 : $imm16),
                "seti\t$imm16", IILoad, [(set ACC, i16
                                          : $imm16)]>;
  def SETIGPR : Pseudo<(outs GPROut:$ra),(ins simm16:$imm),[(set GPROut:$ra,simm16:$imm)]>;

}


/// Branch Instructions
let isIndirectBranch = 1, mayStore = 0, mayLoad = 0, hasSideEffects = 0 in {
  let Uses = [FLAG] in {
    def JL : FJ<0x6,
                (outs GPRIn
                 : $ra),
                (ins cond3
                 : $cond, GPRIn
                 : $rb),
                "jl\t$ra, $cond, $rb", IIBranch>;
    def JLI : Pseudo<(outs), (ins cond3
                              : $cond, GPRIn
                              : $rb)>,
              PseudoInstExpansion<(JL ALLONE, cond3
                                   : $cond, GPRIn
                                   : $rb)>;
    def JLZero : Pseudo<(outs GPRIn
                         : $ra),
                        (ins GPRIn
                         : $rb)>,
                 PseudoInstExpansion<(JL GPRIn
                                      : $ra, 4, GPRIn
                                      : $rb)>;
    def JLNeg : Pseudo<(outs GPRIn
                        : $ra),
                       (ins GPRIn
                        : $rb)>,
                PseudoInstExpansion<(JL GPRIn
                                     : $ra, 2, GPRIn
                                     : $rb)>;
    def JLCarry : Pseudo<(outs GPRIn
                          : $ra),
                         (ins GPRIn
                          : $rb)>,
                  PseudoInstExpansion<(JL GPRIn
                                       : $ra, 1, GPRIn
                                       : $rb)>;
  }
  let isBarrier = 1, isTerminator = 1 in {
    def JLAlways : Pseudo<(outs GPRIn
                           : $ra),
                          (ins GPRIn
                           : $rb)>,
                   PseudoInstExpansion<(JL GPRIn
                                        : $ra, 7, GPRIn
                                        : $rb)>;
    def branch : Pseudo<(outs),
                        (ins GPRIn
                         : $ra),
                        [(brind GPRIn
                          : $ra)]>,
                 PseudoInstExpansion<(JL ZERO, 7, GPRIn
                                      : $ra)>;
  }
  def JLNever : Pseudo<(outs GPRIn
                        : $ra),
                       (ins)>,
                PseudoInstExpansion<(JL GPRIn
                                     : $ra, 0, ZERO)>;
}

let isReturn = 1, isTerminator = 1,
    isBarrier = 1 in def RET : Pseudo<(outs), (ins), [(Templeret)]>,
    PseudoInstExpansion<(JL ZERO, 7, RA)>;

/// Shift Instructions
let mayStore = 0, mayLoad = 0 in {
  def SRL : FFnoOP<0x07, (outs), (ins), "srl", IIAlu>;
}

def : Pat<(srl ACC, 1), (SRL)>;

// Pessimistically assume the stack pointer will be clobbered
let Defs = [SP], Uses = [SP] in {
  def ADJCALLSTACKDOWN : Pseudo<(outs),
                                (ins i16imm
                                 : $amt1, i16imm
                                 : $amt2),
                                [(CallSeqStart timm
                                  : $amt1, timm
                                  : $amt2)]>;
  def ADJCALLSTACKUP : Pseudo<(outs),
                              (ins i16imm
                               : $amt1, i16imm
                               : $amt2),
                              [(CallSeqEnd timm
                                : $amt1, timm
                                : $amt2)]>;
} // Defs = [SP], Uses = [SP]

//===----------------------------------------------------------------------===//
//  Arbitrary patterns that map to one or more instructions
//===----------------------------------------------------------------------===//

// Small immediates
// def : Pat<(i32 immSExt16:$in),
//          (ADDiu ZERO, imm:$in)>;

def : Pat<(add ACC, 1), (ADD ONE)>;
def : Pat<(add ACC, -1), (ADD ALLONE)>;
def : Pat<(not ACC), (NOR ZERO)>;
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
// Uses: dependency
// Defs: write
//===----------------------------------------------------------------------===//
// Temple profiles and nodes
//===----------------------------------------------------------------------===//
def SDT_TempleCall : SDTypeProfile<0, -1, [SDTCisVT<0, i16>]>;
def SDT_TempleCp : SDTypeProfile<0, 2, [SDTCisSameAs<0, 1>]>;
def SDT_TempleSelectCC
    : SDTypeProfile<1, 3,
                    [SDTCisSameAs<0, 1>, SDTCisSameAs<1, 2>, SDTCisVT<3, i16>]>;
def SDT_TempleCallSeqStart
    : SDCallSeqStart<[SDTCisVT<0, i16>, SDTCisVT<1, i16>]>;
def SDT_TempleCallSeqEnd : SDCallSeqEnd<[SDTCisVT<0, i16>, SDTCisVT<1, i16>]>;

def Templeret : SDNode<"TempleISD::RET", SDTNone, [SDNPHasChain]>;
def Templecall
    : SDNode<"TempleISD::CALL", SDT_TempleCall,
             [SDNPHasChain, SDNPOptInGlue, SDNPOutGlue, SDNPVariadic]>;

def CallSeqStart : SDNode<"ISD::CALLSEQ_START", SDT_TempleCallSeqStart,
                          [SDNPHasChain, SDNPOutGlue]>;
def CallSeqEnd : SDNode<"ISD::CALLSEQ_END", SDT_TempleCallSeqEnd,
                        [SDNPHasChain, SDNPOptInGlue, SDNPOutGlue]>;
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
def simm16 : Operand<i16> {
  let ParserMatchClass = SImmAsmOperand<16>;
  let DecoderMethod = "decodeSimm16";
}

def cond3 : Operand<i8> {
  let ParserMatchClass = UImmAsmOperand<3>;
  let DecoderMethod = "decodeCond";
}

// Node immediate fits as 16-bit sign extended on target immediate.
def immSExt16 : PatLeaf<(imm), [{ return isInt<16>(N->getSExtValue()); }]>;

def immcond3 : PatLeaf<(imm), [{ return isInt<3>(N->getZExtValue()); }]>;

// Temple Address Mode! SDNode frameindex could possibily be a match
// since load and store instructions from stack used it.
// def addr
//     : ComplexPattern<iPTR, 2, "SelectAddr", [frameindex], [SDNPWantParent]>;


//===----------------------------------------------------------------------===//
// Pattern fragment for load/store
//===----------------------------------------------------------------------===//

// class AlignedLoad<PatFrag Node> :
//   PatFrag<(ops node:$ptr), (Node node:$ptr), [{
//   LoadSDNode *LD = cast<LoadSDNode>(N);
//   return LD->getMemoryVT().getSizeInBits() / 8 <= LD->getAlignment();
// }]>;

// class AlignedStore<PatFrag Node> :
//   PatFrag<(ops node:$val, node:$ptr), (Node node:$val, node:$ptr), [{
//   StoreSDNode *SD = cast<StoreSDNode>(N);
//   return SD->getMemoryVT().getSizeInBits() / 8 <= SD->getAlignment();
// }]>;

// Load/Store PatFrags.
// def load_a          : AlignedLoad<load>;
// def store_a         : AlignedStore<store>;

//===----------------------------------------------------------------------===//
// Instruction definition
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// Temple Instructions
//===----------------------------------------------------------------------===//

let Defs = [ACC, FLAG], Uses = [ACC], mayStore=0, mayLoad=0 in {
  def NOR : FF<0x00, (outs), (ins GPR : $ra), "nor\t$ra", IIAlu, []>;
  def ADD : FF<0x01, (outs), (ins GPR : $ra), "add\t$ra", IIAlu, []>;
}

/// Load and Store Instructions
///  aligned
let canFoldAsLoad = 1, mayStore = 0, mayLoad = 1 in {
  let Defs = [ACC] in {
    def LD : FF<0x02, (outs), (ins GPR : $ra), "ld\t$ra", IILoad, []>;
  }
}

let Uses = [ACC] in {
  let mayStore = 0, mayLoad = 0 in {
    def MOVE : FF<0x03, (outs GPR : $ra), (ins), "move\t$ra", IIStore, []>;
  }
  let mayStore = 1, mayLoad = 0 in {
    def SD : FF<0x4, (outs), (ins GPR : $ra), "sd\t$ra", IIStore, []>;
  }
}

let mayStore = 0, mayLoad = 0, hasSideEffects = 0, Defs = [ACC] in {
  def SETI : FI<0x05, (outs),
                (ins simm16
                 : $imm16),
                "seti\t$imm16", IILoad, []>;
}

/// Branch Instructions
let isIndirectBranch = 1, mayStore = 0, mayLoad = 0, hasSideEffects = 0 in {
  let Uses = [FLAG] in {
    def JL : FJ<0x6,
                (outs GPR
                 : $ra),
                (ins cond3
                 : $cond, GPR
                 : $rb),
                "jl\t$ra, $cond, $rb", IIBranch>;
  }
}

/// Shift Instructions
let mayStore = 0, mayLoad = 0, Uses = [ACC], Defs = [ACC, FLAG] in {
  def SRL : FFnoOP<0x07, (outs), (ins), "srl", IIAlu>;
}

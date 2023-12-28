// This file is copied and modified from The LLVM Compiler Infrastructure, which
// is distributed under the Apache License v2.0 with LLVM Exceptions (see
// LICENSE.TXT for details). This file is licensed under the same license.

#ifndef LLVM_LIB_TARGET_TEMPLE_TEMPLEISELLOWERING_H
#define LLVM_LIB_TARGET_TEMPLE_TEMPLEISELLOWERING_H

#include "Temple.h"
#include "llvm/CodeGen/SelectionDAG.h"
#include "llvm/CodeGen/TargetLowering.h"

namespace llvm {
class TempleSubtarget;
namespace TempleISD {
enum NodeType : unsigned {
  FIRST_NUMBER = ISD::BUILTIN_OP_END,
  ADD,
  NOR,
  BR_CC,
  CALL,
  SELECT_CC,
  RET,
};
}

class TempleTargetLowering : public TargetLowering {
private:
  const TempleSubtarget &Subtarget;

public:
  explicit TempleTargetLowering(const TargetMachine &TM,
                                const TempleSubtarget &STI);

  // Provide custom lowering hooks for some operations.
  SDValue LowerOperation(SDValue Op, SelectionDAG &DAG) const override;

  // This method returns the name of a target specific DAG node.
  const char *getTargetNodeName(unsigned Opcode) const override;

  MachineBasicBlock *
  EmitInstrWithCustomInserter(MachineInstr &MI,
                              MachineBasicBlock *BB) const override;

  std::pair<unsigned, const TargetRegisterClass *>
  getRegForInlineAsmConstraint(const TargetRegisterInfo *TRI,
                               StringRef Constraint, MVT VT) const override;

private:
  // Lower incoming arguments, copy physregs into vregs
  SDValue LowerFormalArguments(SDValue Chain, CallingConv::ID CallConv,
                               bool IsVarArg,
                               const SmallVectorImpl<ISD::InputArg> &Ins,
                               const SDLoc &DL, SelectionDAG &DAG,
                               SmallVectorImpl<SDValue> &InVals) const override;
  SDValue LowerReturn(SDValue Chain, CallingConv::ID CallConv, bool IsVarArg,
                      const SmallVectorImpl<ISD::OutputArg> &Outs,
                      const SmallVectorImpl<SDValue> &OutVals, const SDLoc &DL,
                      SelectionDAG &DAG) const override;
  SDValue LowerCall(TargetLowering::CallLoweringInfo &CLI,
                    SmallVectorImpl<SDValue> &InVals) const override;
  bool shouldConvertConstantLoadToIntImm(const APInt &Imm,
                                         Type *Ty) const override {
    return true;
  }

  SDValue LowerBR(SDValue Op, SelectionDAG &DAG) const;
  SDValue LowerGlobalAddress(SDValue Op, SelectionDAG &DAG) const;
  SDValue LowerBR_CC(SDValue Op, SelectionDAG &DAG) const;
  SDValue LowerSELECT(SDValue Op, SelectionDAG &DAG) const;
  SDValue LowerFRAMEADDR(SDValue Op, SelectionDAG &DAG) const;
  SDValue LowerRETURNADDR(SDValue Op, SelectionDAG &DAG) const;
  SDValue LowerADD(SDValue Op, SelectionDAG &DAG) const;
  SDValue LowerLOAD(SDValue Op, SelectionDAG &DAG) const;
  SDValue LowerSTORE(SDValue Op, SelectionDAG &DAG) const;
  SDValue EmitCMP(SDValue &LHS, SDValue &RHS, SDValue &TrueV, SDValue &FalseV,
                  SDValue &TempleCC, ISD::CondCode CC, SDLoc dl,
                  SelectionDAG &DAG) const;
};
} // namespace llvm

#endif

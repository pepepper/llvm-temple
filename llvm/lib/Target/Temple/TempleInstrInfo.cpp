// This file is copied and modified from The LLVM Compiler Infrastructure, which
// is distributed under the Apache License v2.0 with LLVM Exceptions (see
// LICENSE.TXT for details). This file is licensed under the same license.

#include "TempleInstrInfo.h"
#include "Temple.h"
#include "TempleSubtarget.h"
#include "TempleTargetMachine.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Support/ErrorHandling.h"

#define GET_INSTRINFO_CTOR_DTOR
#include "TempleGenInstrInfo.inc"

using namespace llvm;

TempleInstrInfo::TempleInstrInfo()
    : TempleGenInstrInfo(Temple::ADJCALLSTACKDOWN, Temple::ADJCALLSTACKUP) {}

void TempleInstrInfo::copyPhysReg(MachineBasicBlock &MBB,
                                  MachineBasicBlock::iterator MBBI,
                                  const DebugLoc &DL, MCRegister DstReg,
                                  MCRegister SrcReg, bool KillSrc) const {
  if (Temple::AccRegRegClass.contains(DstReg)) { // Copy to Acc Reg.
    BuildMI(MBB, MBBI, DL, get(Temple::SETI)).addImm(0);
    BuildMI(MBB, MBBI, DL, get(Temple::ADD)).addReg(SrcReg);
  } else if (Temple::AccRegRegClass.contains(SrcReg)) { // Copy from Acc Reg.
    BuildMI(MBB, MBBI, DL, get(Temple::MOVE)).addReg(DstReg);
  } else { // Copy between General Regs.
    BuildMI(MBB, MBBI, DL, get(Temple::SETI)).addImm(0);
    BuildMI(MBB, MBBI, DL, get(Temple::ADD)).addReg(SrcReg);
    BuildMI(MBB, MBBI, DL, get(Temple::MOVE)).addReg(DstReg);
  }
}

void TempleInstrInfo::storeRegToStackSlot(MachineBasicBlock &MBB,
                                          MachineBasicBlock::iterator MBBI,
                                          Register SrcReg, bool isKill, int FI,
                                          const TargetRegisterClass *RC,
                                          const TargetRegisterInfo *TRI,
                                          Register VReg) const {
  DebugLoc DL;
  if (MBBI != MBB.end())
    DL = MBBI->getDebugLoc();

  if (!Temple::GPRInRegClass.hasSubClassEq(RC))
    llvm_unreachable("Can't store this register to stack slot");
  BuildMI(MBB, MBBI, DL, get(Temple::SETI)).addImm(0);
  BuildMI(MBB, MBBI, DL, get(Temple::ADD)).addReg(SrcReg);
  BuildMI(MBB, MBBI, DL, get(Temple::SD)).addFrameIndex(FI);
}

void TempleInstrInfo::loadRegFromStackSlot(MachineBasicBlock &MBB,
                                           MachineBasicBlock::iterator MBBI,
                                           Register DstReg, int FI,
                                           const TargetRegisterClass *RC,
                                           const TargetRegisterInfo *TRI,
                                           Register VReg) const {
  DebugLoc DL;
  if (MBBI != MBB.end())
    DL = MBBI->getDebugLoc();

  if (!Temple::GPROutRegClass.hasSubClassEq(RC))
    llvm_unreachable("Can't load this register from stack slot");

  BuildMI(MBB, MBBI, DL, get(Temple::LD)).addFrameIndex(FI);
  BuildMI(MBB, MBBI, DL, get(Temple::MOVE)).addReg(DstReg);
}

void TempleInstrInfo::movImm16(MachineBasicBlock &MBB,
                               MachineBasicBlock::iterator MBBI,
                               const DebugLoc &DL, MCRegister DstReg,
                               uint64_t Val, MachineInstr::MIFlag Flag) const {
  assert(isInt<16>(Val) && "Can only materialize 16-bit constants");

  BuildMI(MBB, MBBI, DL, get(Temple::SETI)).addImm(Val).setMIFlag(Flag);
  BuildMI(MBB, MBBI, DL, get(Temple::MOVE)).addReg(DstReg).setMIFlag(Flag);
}

// // The contents of values added to Cond are not examined outside of
// // TempleInstrInfo, giving us flexibility in what to push to it. For Temple,
// we
// // push BranchOpcode, Reg1, Reg2.
// static void parseCondBranch(MachineInstr &LastInst, MachineBasicBlock
// *&Target,
//                             SmallVectorImpl<MachineOperand> &Cond) {
//   // Block ends with fall-through condbranch.
//   assert(LastInst.getDesc().isConditionalBranch() &&
//          "Unknown conditional branch");
//   Target = LastInst.getOperand(2).getMBB();
//   Cond.push_back(MachineOperand::CreateImm(LastInst.getOpcode()));
//   Cond.push_back(LastInst.getOperand(0));
//   Cond.push_back(LastInst.getOperand(1));
// }

// bool TempleInstrInfo::analyzeBranch(MachineBasicBlock &MBB,
//                                   MachineBasicBlock *&TBB,
//                                   MachineBasicBlock *&FBB,
//                                   SmallVectorImpl<MachineOperand> &Cond,
//                                   bool AllowModify) const {
//   TBB = FBB = nullptr;
//   Cond.clear();

//   // If the block has no terminators, it just falls into the block after it.
//   MachineBasicBlock::iterator I = MBB.getLastNonDebugInstr();
//   if (I == MBB.end() || !isUnpredicatedTerminator(*I))
//     return false;

//   // Count the number of terminators and find the first unconditional or
//   // indirect branch.
//   MachineBasicBlock::iterator FirstUncondOrIndirectBr = MBB.end();
//   int NumTerminators = 0;
//   for (auto J = I.getReverse(); J != MBB.rend() &&
//   isUnpredicatedTerminator(*J);
//        J++) {
//     NumTerminators++;
//     if (J->getDesc().isUnconditionalBranch() ||
//         J->getDesc().isIndirectBranch()) {
//       FirstUncondOrIndirectBr = J.getReverse();
//     }
//   }

//   // If AllowModify is true, we can erase any terminators after
//   // FirstUncondOrIndirectBR.
//   if (AllowModify && FirstUncondOrIndirectBr != MBB.end()) {
//     while (std::next(FirstUncondOrIndirectBr) != MBB.end()) {
//       std::next(FirstUncondOrIndirectBr)->eraseFromParent();
//       NumTerminators--;
//     }
//     I = FirstUncondOrIndirectBr;
//   }

//   // We can't handle blocks that end in an indirect branch.
//   if (I->getDesc().isIndirectBranch())
//     return true;

//   // We can't handle blocks with more than 2 terminators.
//   if (NumTerminators > 2)
//     return true;

//   // Handle a single unconditional branch.
//   if (NumTerminators == 1 && I->getDesc().isUnconditionalBranch()) {
//     TBB = I->getOperand(0).getMBB();
//     return false;
//   }

//   // Handle a single conditional branch.
//   if (NumTerminators == 1 && I->getDesc().isConditionalBranch()) {
//     parseCondBranch(*I, TBB, Cond);
//     return false;
//   }

//   // Handle a conditional branch followed by an unconditional branch.
//   if (NumTerminators == 2 && std::prev(I)->getDesc().isConditionalBranch() &&
//       I->getDesc().isUnconditionalBranch()) {
//     parseCondBranch(*std::prev(I), TBB, Cond);
//     FBB = I->getOperand(0).getMBB();
//     return false;
//   }

//   // Otherwise, we can't handle this.
//   return true;
// }

// unsigned TempleInstrInfo::removeBranch(MachineBasicBlock &MBB,
//                                      int *BytesRemoved) const {
//   if (BytesRemoved)
//     *BytesRemoved = 0;

//   MachineBasicBlock::iterator I = MBB.getLastNonDebugInstr();
//   if (I == MBB.end())
//     return 0;

//   if (!I->getDesc().isUnconditionalBranch() &&
//       !I->getDesc().isConditionalBranch())
//     return 0;

//   // Remove the branch.
//   I->eraseFromParent();
//   if (BytesRemoved)
//     *BytesRemoved += getInstSizeInBytes(*I);

//   I = MBB.end();

//   if (I == MBB.begin())
//     return 1;
//   --I;
//   if (!I->getDesc().isConditionalBranch())
//     return 1;

//   // Remove the branch.
//   I->eraseFromParent();
//   if (BytesRemoved)
//     *BytesRemoved += getInstSizeInBytes(*I);
//   return 2;
// }

// Inserts a branch into the end of the specific MachineBasicBlock, returning
// the number of instructions inserted.
unsigned TempleInstrInfo::insertBranch(
    MachineBasicBlock &MBB, MachineBasicBlock *TBB, MachineBasicBlock *FBB,
    ArrayRef<MachineOperand> Cond, const DebugLoc &DL, int *BytesAdded) const {
  if (BytesAdded)
    *BytesAdded = 0;

  // Shouldn't be a fall through.
  assert(TBB && "InsertBranch must not be told to insert a fallthrough");
  assert((Cond.size() == 3 || Cond.size() == 0) &&
         "Temple branch conditions have two components!");

  // Unconditional branch.
  if (Cond.empty()) {
    MachineInstr &MI = *BuildMI(&MBB, DL, get(Temple::JLAlways)).addMBB(TBB);
    if (BytesAdded)
      *BytesAdded = getInstSizeInBytes(MI);
    return 1;
  }

  // Either a one or two-way conditional branch.
  unsigned Opc = Cond[0].getImm();
  MachineInstr &CondMI = *BuildMI(&MBB, DL, get(Opc)).addMBB(TBB);
  if (BytesAdded)
    *BytesAdded += getInstSizeInBytes(CondMI);

  // One-way conditional branch.
  if (!FBB)
    return 1;

  // Two-way conditional branch.
  MachineInstr &MI = *BuildMI(&MBB, DL, get(Temple::JLAlways)).addMBB(FBB);
  if (BytesAdded)
    *BytesAdded += getInstSizeInBytes(MI);
  return 2;
}

// bool TempleInstrInfo::reverseBranchCondition(
//     SmallVectorImpl<MachineOperand> &Cond) const {
//   assert((Cond.size() == 3) && "Invalid branch condition!");

//   switch (Cond[0].getImm()) {
//   case Temple::JLEQ:
//     Cond[0].setImm(Temple::BNE);
//     break;
//   case Temple::JLNE:
//     Cond[0].setImm(Temple::BEQ);
//     break;
//   }

//   return false;
// }

// MachineBasicBlock *
// TempleInstrInfo::getBranchDestBlock(const MachineInstr &MI) const {
//   assert(MI.getDesc().isBranch() && "Unexpected opcode!");
//   // The branch target is always the last operand.
//   int NumOp = MI.getNumExplicitOperands();
//   return MI.getOperand(NumOp - 1).getMBB();
// }

bool TempleInstrInfo::isBranchOffsetInRange(unsigned BranchOp,
                                            int64_t BrOffset) const {
  return isInt<16>(BrOffset);
}

unsigned TempleInstrInfo::getInstSizeInBytes(const MachineInstr &MI) const {
  unsigned Opcode = MI.getOpcode();

  switch (Opcode) {
  default:
    return get(Opcode).getSize();

  case TargetOpcode::INLINEASM: {
    const MachineFunction &MF = *MI.getParent()->getParent();
    const auto &TM = static_cast<const TempleTargetMachine &>(MF.getTarget());
    return getInlineAsmLength(MI.getOperand(0).getSymbolName(),
                              *TM.getMCAsmInfo());
  }
  }
}

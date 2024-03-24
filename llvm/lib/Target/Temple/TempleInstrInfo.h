// This file is copied and modified from The LLVM Compiler Infrastructure, which
// is distributed under the Apache License v2.0 with LLVM Exceptions (see
// LICENSE.TXT for details). This file is licensed under the same license.

#ifndef LLVM_LIB_TARGET_TEMPLE_TEMPLEINSTRINFO_H
#define LLVM_LIB_TARGET_TEMPLE_TEMPLEINSTRINFO_H

#include "TempleRegisterInfo.h"
#include "llvm/CodeGen/TargetInstrInfo.h"

#define GET_INSTRINFO_HEADER
#include "TempleGenInstrInfo.inc"

namespace llvm {
namespace Temple {
enum CondCode {
  COND_Z,
  COND_C,
  COND_M,

  COND_INVALID
};
} // namespace Temple
class TempleInstrInfo : public TempleGenInstrInfo {
public:
  TempleInstrInfo();

  void copyPhysReg(MachineBasicBlock &MBB, MachineBasicBlock::iterator MBBI,
                   const DebugLoc &DL, MCRegister DstReg, MCRegister SrcReg,
                   bool KillSrc) const override;
  void storeRegToStackSlot(MachineBasicBlock &MBB,
                           MachineBasicBlock::iterator MI, Register SrcReg,
                           bool isKill, int FrameIndex,
                           const TargetRegisterClass *RC,
                           const TargetRegisterInfo *TRI,
                           Register VReg) const override;
  void loadRegFromStackSlot(MachineBasicBlock &MBB,
                            MachineBasicBlock::iterator MI, Register DestReg,
                            int FrameIndex, const TargetRegisterClass *RC,
                            const TargetRegisterInfo *TRI,
                            Register VReg) const override;

  // Materializes the given int16 Val into DstReg.
  void movImm16(MachineBasicBlock &MBB, MachineBasicBlock::iterator MBBI,
                const DebugLoc &DL, MCRegister DstReg, uint64_t Val,
                MachineInstr::MIFlag Flag = MachineInstr::NoFlags) const;

  bool analyzeBranch(MachineBasicBlock &MBB, MachineBasicBlock *&TBB,
                     MachineBasicBlock *&FBB,
                     SmallVectorImpl<MachineOperand> &Cond,
                     bool AllowModify) const override;

  unsigned insertBranch(MachineBasicBlock &MBB, MachineBasicBlock *TBB,
                        MachineBasicBlock *FBB, ArrayRef<MachineOperand>
                        Cond, const DebugLoc &dl, int *BytesAdded = nullptr)
                        const override;

  unsigned removeBranch(MachineBasicBlock &MBB,
                        int *BytesRemoved = nullptr) const override;

  // bool
  // reverseBranchCondition(SmallVectorImpl<MachineOperand> &Cond) const
  // override;

  unsigned getInstSizeInBytes(const MachineInstr &MI) const override;

  bool isBranchOffsetInRange(unsigned BranchOpc,
                             int64_t BrOffset) const override;

  MachineBasicBlock *getBranchDestBlock(const MachineInstr &MI) const
  override;
};
} // namespace llvm

#endif

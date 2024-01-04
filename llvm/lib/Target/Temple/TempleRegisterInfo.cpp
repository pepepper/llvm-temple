// This file is copied and modified from The LLVM Compiler Infrastructure, which
// is distributed under the Apache License v2.0 with LLVM Exceptions (see
// LICENSE.TXT for details). This file is licensed under the same license.

#include "TempleRegisterInfo.h"
#include "Temple.h"
#include "TempleSubtarget.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/RegisterScavenging.h"
#include "llvm/CodeGen/TargetFrameLowering.h"
#include "llvm/CodeGen/TargetInstrInfo.h"
#include "llvm/Support/ErrorHandling.h"

#define GET_REGINFO_TARGET_DESC
#include "TempleGenRegisterInfo.inc"

using namespace llvm;

TempleRegisterInfo::TempleRegisterInfo(unsigned HwMode)
    : TempleGenRegisterInfo(/* RA */ Temple::RA, /*DwarfFlavour*/ 0,
                            /*EHFlavor*/ 0,
                            /*PC*/ 0, HwMode) {}

const MCPhysReg *
TempleRegisterInfo::getCalleeSavedRegs(const MachineFunction *MF) const {
  return CSR_SaveList;
}

BitVector TempleRegisterInfo::getReservedRegs(const MachineFunction &MF) const {
  BitVector Reserved(getNumRegs());

  // Use markSuperRegs to ensure any register aliases are also reserved
  markSuperRegs(Reserved, Temple::ZERO);
  markSuperRegs(Reserved, Temple::ONE);
  markSuperRegs(Reserved, Temple::ALLONE);
  markSuperRegs(Reserved, Temple::RA); // ra
  markSuperRegs(Reserved, Temple::SP); // sp
  //   markSuperRegs(Reserved, Temple::R19); // fp
  assert(checkAllSuperRegsMarked(Reserved));
  return Reserved;
}

bool TempleRegisterInfo::eliminateFrameIndex(MachineBasicBlock::iterator II,
                                             int SPAdj, unsigned FIOperandNum,
                                             RegScavenger *RS) const {
  assert(SPAdj == 0 && "Unexpected non-zero SPAdj value");

  MachineInstr &MI = *II;
  MachineFunction &MF = *MI.getParent()->getParent();
  MachineRegisterInfo &MRI = MF.getRegInfo();
  const TempleInstrInfo *TII =
      MF.getSubtarget<TempleSubtarget>().getInstrInfo();
  DebugLoc DL = MI.getDebugLoc();

  int FrameIndex = MI.getOperand(FIOperandNum).getIndex();
  Register FrameReg;

  MI.print(dbgs());

  int Offset = getFrameLowering(MF)
                   ->getFrameIndexReference(MF, FrameIndex, FrameReg)
                   .getFixed() +
               (MI.getOperand(FIOperandNum + 1).isImm()
                    ? MI.getOperand(FIOperandNum + 1).getImm()
                    : 0);

  if (!isInt<16>(Offset))
    report_fatal_error(
        "Frame offsets outside of the signed 16-bit range not supported");

  MachineBasicBlock &MBB = *MI.getParent();

  unsigned ScratchReg = MRI.createVirtualRegister(&Temple::GPRRegClass);
  BuildMI(MBB, II, DL, TII->get(Temple::SETI)).addImm(Offset);
  BuildMI(MBB, II, DL, TII->get(Temple::ADD)).addReg(FrameReg);
  BuildMI(MBB, II, DL, TII->get(Temple::MOVE))
      .addReg(ScratchReg, RegState::Define);
  Offset = 0;
  FrameReg = ScratchReg;

  MI.getOperand(FIOperandNum).ChangeToRegister(FrameReg, false);
  return false;
}

Register TempleRegisterInfo::getFrameRegister(const MachineFunction &MF) const {
  const TargetFrameLowering *TFI = getFrameLowering(MF);
  // Return FP if any, SP otherwise.
  return TFI->hasFP(MF) ? Temple::R19 /* FP */ : Temple::SP /* SP */;
}

const uint32_t *
TempleRegisterInfo::getCallPreservedMask(const MachineFunction & /*MF*/,
                                         CallingConv::ID /*CC*/) const {
  return CSR_RegMask;
}

#include "Temple.h"
#include "TempleSubtarget.h"
#include "llvm/CodeGen/LivePhysRegs.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/MC/MCAsmInfo.h"
#include "llvm/Support/Debug.h"
using namespace llvm;

#define DEBUG_TYPE "temple-pseudo"
static cl::opt<bool> VerifyTemplePseudo(
    "verify-temple-pseudo-expand", cl::Hidden,
    cl::desc("Verify machine code after expanding Temple pseudos"));
#define TEMPLE_EXPAND_PSEUDO_NAME "Temple pseudo instruction expansion pass"

namespace {
class TempleExpandPseudo : public MachineFunctionPass {
public:
  static char ID;
  TempleExpandPseudo() : MachineFunctionPass(ID) {}

  const TargetRegisterInfo *TRI;
  const TempleSubtarget *STI;

  bool runOnMachineFunction(MachineFunction &Fn) override;

  MachineFunctionProperties getRequiredProperties() const override {
    return MachineFunctionProperties().set(
        MachineFunctionProperties::Property::NoVRegs);
  }

  StringRef getPassName() const override { return TEMPLE_EXPAND_PSEUDO_NAME; }

private:
  bool ExpandMI(MachineBasicBlock &MBB, MachineBasicBlock::iterator MBBI,
                MachineBasicBlock::iterator &NextMBBI);
  bool ExpandMBB(MachineBasicBlock &MBB);
  bool ExpandSETIGPR(MachineBasicBlock &MBB, MachineBasicBlock::iterator MBBI,
                     MachineBasicBlock::iterator &NextMBBI);
};
char TempleExpandPseudo::ID = 0;
} // namespace

INITIALIZE_PASS(TempleExpandPseudo, DEBUG_TYPE, TEMPLE_EXPAND_PSEUDO_NAME,
                false, false)

bool TempleExpandPseudo::ExpandMI(MachineBasicBlock &MBB,
                                  MachineBasicBlock::iterator MBBI,
                                  MachineBasicBlock::iterator &NextMBBI) {
  MachineInstr &MI = *MBBI;
  unsigned Opcode = MI.getOpcode();
  switch (Opcode) {
  default:
    return false;
  case Temple::SETIGPR: {
    BuildMI(MBB, MBBI, MI.getDebugLoc(),
            MBB.getParent()->getSubtarget().getInstrInfo()->get(Temple::SETI)).addImm(MI.getOperand(1).getImm());
    BuildMI(MBB, MBBI, MI.getDebugLoc(),
            MBB.getParent()->getSubtarget().getInstrInfo()->get(Temple::MOVE)).addReg(MI.getOperand(0).getReg());

    MI.eraseFromParent();
    return true;
  case Temple::ADDI: {
    BuildMI(MBB, MBBI, MI.getDebugLoc(),
            MBB.getParent()->getSubtarget().getInstrInfo()->get(Temple::SETI)).addImm(MI.getOperand(1).getImm());
    BuildMI(MBB, MBBI, MI.getDebugLoc(),
            MBB.getParent()->getSubtarget().getInstrInfo()->get(Temple::ADD)).addReg(MI.getOperand(0).getReg());

    MI.eraseFromParent();
    return true;
  }
  }
  }
}

bool TempleExpandPseudo::ExpandMBB(MachineBasicBlock &MBB) {
  bool Modified = false;

  MachineBasicBlock::iterator MBBI = MBB.begin(), E = MBB.end();
  while (MBBI != E) {
    MachineBasicBlock::iterator NMBBI = std::next(MBBI);
    Modified |= ExpandMI(MBB, MBBI, NMBBI);
    MBBI = NMBBI;
  }

  return Modified;
}

bool TempleExpandPseudo::runOnMachineFunction(MachineFunction &MF) {
  STI = &MF.getSubtarget<TempleSubtarget>();
  TRI = STI->getRegisterInfo();

  LLVM_DEBUG(
      dbgs() << "********** Temple EXPAND PSEUDO INSTRUCTIONS **********\n"
             << "********** Function: " << MF.getName() << '\n');

  bool Modified = false;
  for (MachineBasicBlock &MBB : MF)
    Modified |= ExpandMBB(MBB);
  if (VerifyTemplePseudo)
    MF.verify(this, "After expanding ARM pseudo instructions.");

  LLVM_DEBUG(dbgs() << "***************************************************\n");
  return Modified;
}
/// createTempleExpandPseudoPass - returns an instance of the pseudo instruction
/// expansion pass.
FunctionPass *llvm::createTempleExpandPseudoPass() {
  return new TempleExpandPseudo();
}

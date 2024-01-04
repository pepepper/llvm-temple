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

// companion macros
#define ImmBuilder(op, Imm)                                                    \
  BuildMI(MBB, MBBI, MI.getDebugLoc(),                                         \
          MBB.getParent()->getSubtarget().getInstrInfo()->get(Temple::op))     \
      .addImm(Imm)
#define RegBuilder(op, Reg)                                                    \
  BuildMI(MBB, MBBI, MI.getDebugLoc(),                                         \
          MBB.getParent()->getSubtarget().getInstrInfo()->get(Temple::op))     \
      .addReg(Reg)
#define GetOperandImm(opnum) MI.getOperand(opnum).getImm()
#define GetOperandReg(opnum) MI.getOperand(opnum).getReg()

bool TempleExpandPseudo::ExpandMI(MachineBasicBlock &MBB,
                                  MachineBasicBlock::iterator MBBI,
                                  MachineBasicBlock::iterator &NextMBBI) {
  MachineInstr &MI = *MBBI;
  unsigned Opcode = MI.getOpcode();
  switch (Opcode) {
  default:
    return false;
  case Temple::ADDi: {
    ImmBuilder(SETI, GetOperandImm(2)); // ACC = imm16
    RegBuilder(ADD, GetOperandReg(1));  // ACC = ACC + $rb

    RegBuilder(MOVE, GetOperandReg(0)); // move to $ra

    MI.eraseFromParent();
    return true;
  } // ADDi
  case Temple::ADDr: {
    RegBuilder(NOR, Temple::ALLONE); // clear ACC

    RegBuilder(ADD, GetOperandReg(1)); // ACC = ACC + $rb
    RegBuilder(ADD, GetOperandReg(2)); // ACC = ACC + $rc

    RegBuilder(MOVE, GetOperandReg(0)); // move to $ra

    MI.eraseFromParent();
    return true;
  }                                  // ADDr
  case Temple::SRLi: {               //$ra = SRL * imm16
    RegBuilder(NOR, Temple::ALLONE); // clear ACC

    RegBuilder(ADD, GetOperandReg(1)); // ACC = $rb
    for (int i = 0; i < MI.getOperand(2).getImm(); i++) {
      BuildMI(MBB, MBBI, MI.getDebugLoc(),
              MBB.getParent()->getSubtarget().getInstrInfo()->get(
                  Temple::SRL)); // ACC >> 1
    }                            // repeat SRL

    RegBuilder(MOVE, GetOperandReg(0)); // move to $ra

    MI.eraseFromParent();
    return true;
  }
  case Temple::ANDi: {               //$ra = ($rb NOR 0) NOR (imm16 NOR 0)
    RegBuilder(NOR, Temple::ALLONE); // clear ACC

    RegBuilder(NOR, GetOperandReg(1)); // ACC = not($rb)
    RegBuilder(MOVE, Temple::T0);      // stash to t0

    ImmBuilder(SETI, GetOperandImm(2));
    RegBuilder(NOR, Temple::ZERO); // ACC = not(imm16)

    RegBuilder(NOR, Temple::T0);

    RegBuilder(MOVE, GetOperandReg(0)); // move to $ra

    MI.eraseFromParent();
    return true;
  }                                  // ANDi
  case Temple::ANDr: {               //$ra = ($rb NOR 0) NOR ($rc NOR 0)
    RegBuilder(NOR, Temple::ALLONE); // clear ACC

    RegBuilder(NOR, GetOperandReg(1)); // ACC = not($rb)
    RegBuilder(MOVE, Temple::T0);      // stash to t0

    RegBuilder(NOR, Temple::ALLONE);   // clear ACC
    RegBuilder(NOR, GetOperandReg(2)); // ACC = not($rc)

    RegBuilder(NOR, Temple::T0);

    RegBuilder(MOVE, GetOperandReg(0)); // move to $ra

    MI.eraseFromParent();
    return true;
  }                   // ANDr
  case Temple::ORi: { //$ra = (imm16 NOR $rb) NOR 0
    ImmBuilder(SETI, GetOperandImm(2));
    RegBuilder(NOR, GetOperandReg(1));
    RegBuilder(NOR, Temple::ZERO);

    RegBuilder(MOVE, GetOperandReg(0)); // move to $ra

    MI.eraseFromParent();
    return true;
  }                                  // ORi
  case Temple::ORr: {                //$ra = ($rb NOR $rc) NOR 0
    RegBuilder(NOR, Temple::ALLONE); // clear ACC

    RegBuilder(ADD, GetOperandReg(1));
    RegBuilder(NOR, GetOperandReg(2));
    RegBuilder(NOR, Temple::ZERO);

    RegBuilder(MOVE, GetOperandReg(0)); // move to $ra

    MI.eraseFromParent();
    return true;
  }                    // ORr
  case Temple::XORi: { // $ra = (($rb NOR $rb) NOR (imm16 NOR imm16)) NOR
                       //($rb NOR imm16)
    RegBuilder(NOR, Temple::ALLONE); // clear ACC

    RegBuilder(ADD, GetOperandReg(1));
    RegBuilder(NOR, GetOperandReg(1)); //($rb NOR $rb)
    RegBuilder(MOVE, Temple::T0);      // stash to t0

    ImmBuilder(SETI, GetOperandImm(2));
    RegBuilder(MOVE, Temple::T1); // stash to t1
    RegBuilder(NOR, Temple::T1);  //(imm16 NOR imm16)

    RegBuilder(NOR, Temple::T0);
    RegBuilder(MOVE, Temple::T0); // stash to t0

    ImmBuilder(SETI, GetOperandImm(2));
    RegBuilder(NOR, GetOperandReg(1)); //($rb NOR imm16)

    RegBuilder(NOR, Temple::T0);

    RegBuilder(MOVE, GetOperandReg(0)); // move to $ra

    MI.eraseFromParent();
    return true;
  }                    // XORi
  case Temple::XORr: { // $ra = (($rb NOR $rb) NOR ($rc NOR $rc)) NOR
                       //($rb NOR $rc)
    RegBuilder(NOR, Temple::ALLONE); // clear ACC

    RegBuilder(ADD, GetOperandReg(1));
    RegBuilder(NOR, GetOperandReg(1)); //($rb NOR $rb)
    RegBuilder(MOVE, Temple::T0);      // stash to t0

    RegBuilder(NOR, Temple::ALLONE); // clear ACC
    RegBuilder(ADD, GetOperandReg(1));
    RegBuilder(NOR, GetOperandReg(1)); //($rc NOR $rc)

    RegBuilder(NOR, Temple::T0);
    RegBuilder(MOVE, Temple::T0); // stash to t0

    RegBuilder(NOR, Temple::ALLONE); // clear ACC
    RegBuilder(ADD, GetOperandReg(1));
    RegBuilder(NOR, GetOperandReg(2)); //($rb NOR $rc)

    RegBuilder(NOR, Temple::T0);

    RegBuilder(MOVE, GetOperandReg(0)); // move to $ra

    MI.eraseFromParent();
    return true;
  }
  case Temple::STORE: {
    BuildMI(MBB, MBBI, MI.getDebugLoc(),
            MBB.getParent()->getSubtarget().getInstrInfo()->get(Temple::SETI))
        .addImm(0);
    BuildMI(MBB, MBBI, MI.getDebugLoc(),
            MBB.getParent()->getSubtarget().getInstrInfo()->get(Temple::ADD))
        .addReg(MI.getOperand(1).getReg());
    BuildMI(MBB, MBBI, MI.getDebugLoc(),
            MBB.getParent()->getSubtarget().getInstrInfo()->get(Temple::SD))
        .addReg(MI.getOperand(0).getReg());

    MI.eraseFromParent();
    return true;
  } // STORE
  case Temple::LOAD: {
    BuildMI(MBB, MBBI, MI.getDebugLoc(),
            MBB.getParent()->getSubtarget().getInstrInfo()->get(Temple::LD))
        .addReg(MI.getOperand(0).getReg());
    BuildMI(MBB, MBBI, MI.getDebugLoc(),
            MBB.getParent()->getSubtarget().getInstrInfo()->get(Temple::MOVE))
        .addReg(MI.getOperand(1).getReg());
    MI.eraseFromParent();
    return true;
  } // LOAD
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

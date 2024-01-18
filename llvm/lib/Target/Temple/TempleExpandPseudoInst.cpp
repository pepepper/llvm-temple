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

#define ADDrPseudo(Reg1, Reg2)                                                 \
  RegBuilder(NOR, Temple::ALLONE);                                             \
  RegBuilder(ADD, Reg1);                                                       \
  RegBuilder(ADD, Reg2);
#define ADDiPseudo(Reg, Imm)                                                   \
  ImmBuilder(SETI, Imm);                                                       \
  RegBuilder(ADD, Reg);

#define SUBrPseudo(Reg1, Reg2)                                                 \
  RegBuilder(NOR, Temple::ALLONE);                                             \
  RegBuilder(NOR, Reg2);                                                       \
  RegBuilder(ADD, Temple::ONE);                                                \
  RegBuilder(ADD, Reg1);
#define SUBiPseudo(Reg, Imm)                                                   \
  ImmBuilder(SETI, Imm);                                                       \
  RegBuilder(NOR, Temple::ZERO);                                               \
  RegBuilder(ADD, Temple::ONE);                                                \
  RegBuilder(ADD, Reg);

#define COND_Z (0x4)
#define COND_N (0x2)
#define COND_C (0x1)
#define COND_ALWAYS (0x7)
#define COND_NEVER (0x0)

#define JLBuilder(pcdest, cond, jumpdest)                                      \
  BuildMI(MBB, MBBI, MI.getDebugLoc(),                                         \
          MBB.getParent()->getSubtarget().getInstrInfo()->get(Temple::JL))     \
      .addReg(pcdest)                                                          \
      .addImm(cond)                                                            \
      .addReg(jumpdest);

bool TempleExpandPseudo::ExpandMI(MachineBasicBlock &MBB,
                                  MachineBasicBlock::iterator MBBI,
                                  MachineBasicBlock::iterator &NextMBBI) {
  MachineInstr &MI = *MBBI;
  unsigned Opcode = MI.getOpcode();
  dbgs() << "TempleExpandPseudo:" << MI << "\n";
  switch (Opcode) {
  default:
    return false;
  case Temple::ADDi: {
    ADDiPseudo(GetOperandReg(1), GetOperandImm(2));

    RegBuilder(MOVE, GetOperandReg(0)); // move to $ra

    MI.eraseFromParent();
    return true;
  } // ADDi
  case Temple::ADDr: {
    ADDrPseudo(GetOperandReg(1), GetOperandReg(2));

    RegBuilder(MOVE, GetOperandReg(0)); // move to $ra

    MI.eraseFromParent();
    return true;
  } // ADDr

  case Temple::SUBi: {
    SUBiPseudo(GetOperandReg(1), GetOperandImm(2));

    RegBuilder(MOVE, GetOperandReg(0)); // move to $ra

    MI.eraseFromParent();
    return true;
  } // SUBi
  case Temple::SUBr: {
    SUBiPseudo(GetOperandReg(1), GetOperandReg(2));

    RegBuilder(MOVE, GetOperandReg(0)); // move to $ra

    MI.eraseFromParent();
    return true;
  } // SUBr

  case Temple::SRLi: {               //$ra = SRL x imm16
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
  }                                  // SRLi
  case Temple::SLLi: {               //$ra = ($rb+$rb) x imm16
    RegBuilder(NOR, Temple::ALLONE); // clear ACC

    RegBuilder(ADD, GetOperandReg(1)); // ACC = $rb
    for (int i = 0; i < MI.getOperand(2).getImm(); i++) {
      RegBuilder(MOVE, Temple::T0);
      RegBuilder(ADD, Temple::T0);
    } // ACC = ACC*2

    RegBuilder(MOVE, GetOperandReg(0)); // move to $ra

    MI.eraseFromParent();
    return true;
  } // SLLi

  case Temple::SRAi: {               //$ra = SRA * imm16
                                     // TODO: optimization
    RegBuilder(NOR, Temple::ALLONE); // clear ACC

    RegBuilder(ADD, Temple::ALLONE);
    BuildMI(MBB, MBBI, MI.getDebugLoc(),
            MBB.getParent()->getSubtarget().getInstrInfo()->get(Temple::SRL));
    RegBuilder(NOR, GetOperandReg(1));
    RegBuilder(MOVE, Temple::T1); // get MSB

    RegBuilder(NOR, Temple::ALLONE);   // clear ACC
    RegBuilder(ADD, GetOperandReg(1)); // ACC = $rb

    for (int i = 0; i < MI.getOperand(2).getImm(); i++) {
      BuildMI(MBB, MBBI, MI.getDebugLoc(),
              MBB.getParent()->getSubtarget().getInstrInfo()->get(
                  Temple::SRL));   // ACC >> 1
      RegBuilder(ADD, Temple::T1); // copy MSB
    }

    RegBuilder(MOVE, GetOperandReg(0)); // move to $ra

    MI.eraseFromParent();
    return true;
  } // SRAi

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
  } // ANDr

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
  } // ORr

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
  } // XORr

  case Temple::STOREr: {
    ImmBuilder(SETI, 0);
    RegBuilder(ADD, GetOperandReg(0));
    RegBuilder(SD, GetOperandReg(1));

    MI.eraseFromParent();
    return true;
  } // STOREr
  case Temple::STOREi: {
    ImmBuilder(SETI, GetOperandImm(0));
    RegBuilder(SD, GetOperandReg(1));

    MI.eraseFromParent();
    return true;
  } // STOREi
  case Temple::LOAD: {
    RegBuilder(LD, GetOperandReg(1));
    RegBuilder(MOVE, GetOperandReg(0));
    MI.eraseFromParent();
    return true;
  } // LOAD

  case Temple::BR: {
    BuildMI(MBB, MBBI, MI.getDebugLoc(),
            MBB.getParent()->getSubtarget().getInstrInfo()->get(Temple::SETI))
        .add(MI.getOperand(
            0)); // Can't use ImmBuilder because Operand(0) is branch target.
    RegBuilder(MOVE, Temple::T0); // copy destination to T0
    JLBuilder(Temple::ZERO, COND_ALWAYS, Temple::T0);
    MI.eraseFromParent();
    return true;
  }

  case Temple::BEQ: {
    BuildMI(MBB, MBBI, MI.getDebugLoc(),
            MBB.getParent()->getSubtarget().getInstrInfo()->get(Temple::SETI))
        .add(MI.getOperand(
            0)); // Can't use ImmBuilder because Operand(0) is branch target.
    RegBuilder(MOVE, Temple::T1); // copy destination to T1

    SUBrPseudo(GetOperandReg(1), GetOperandReg(2));

    JLBuilder(Temple::ZERO, COND_Z, Temple::T1); // branch Z__ $rc

    MI.eraseFromParent();
    return true;
  } // BEQ
  case Temple::BNE: {
    BuildMI(MBB, MBBI, MI.getDebugLoc(),
            MBB.getParent()->getSubtarget().getInstrInfo()->get(Temple::SETI))
        .add(MI.getOperand(
            0)); // Can't use ImmBuilder because Operand(0) is branch target.
    RegBuilder(MOVE, Temple::T1); // copy destination to T1

    JLBuilder(Temple::T0, COND_NEVER, Temple::ZERO); // load program counter
    ADDiPseudo(Temple::T0, 14);                      // need verification

    RegBuilder(MOVE, Temple::T0);

    SUBrPseudo(GetOperandReg(1), GetOperandReg(2)); //$ra-$rb

    JLBuilder(Temple::ZERO, COND_Z, Temple::T0);      // branch Z__ fallthrough
    JLBuilder(Temple::ZERO, COND_ALWAYS, Temple::T1); // branch always $rc

    MI.eraseFromParent();
    return true;
  } // BNE
  case Temple::BLT: {
    BuildMI(MBB, MBBI, MI.getDebugLoc(),
            MBB.getParent()->getSubtarget().getInstrInfo()->get(Temple::SETI))
        .add(MI.getOperand(
            0)); // Can't use ImmBuilder because Operand(0) is branch target.
    RegBuilder(MOVE, Temple::T1); // copy destination to T1

    SUBrPseudo(GetOperandReg(1), GetOperandReg(2)); // $ra-$rb

    JLBuilder(Temple::ZERO, COND_N, Temple::T1); // branch _N_ $rc

    MI.eraseFromParent();
    return true;
  } // BLT
  case Temple::BGE: {
    BuildMI(MBB, MBBI, MI.getDebugLoc(),
            MBB.getParent()->getSubtarget().getInstrInfo()->get(Temple::SETI))
        .add(MI.getOperand(
            0)); // Can't use ImmBuilder because Operand(0) is branch target.
    RegBuilder(MOVE, Temple::T1); // copy destination to T1

    JLBuilder(Temple::T0, COND_NEVER, Temple::ZERO); // load program counter
    ADDiPseudo(Temple::T0, 14);                      // need verification
    RegBuilder(MOVE, Temple::T0);

    SUBrPseudo(GetOperandReg(1), GetOperandReg(2)); //$ra-$rb

    JLBuilder(Temple::ZERO, COND_N, Temple::T0);      // branch _N_ fallthrough
    JLBuilder(Temple::ZERO, COND_ALWAYS, Temple::T1); // branch always $rc

    MI.eraseFromParent();
    return true;
  } // BGE
  case Temple::BLTU: {
    BuildMI(MBB, MBBI, MI.getDebugLoc(),
            MBB.getParent()->getSubtarget().getInstrInfo()->get(Temple::SETI))
        .add(MI.getOperand(
            0)); // Can't use ImmBuilder because Operand(0) is branch target.
    RegBuilder(MOVE, Temple::T1); // copy destination to T1

    SUBrPseudo(GetOperandReg(1), GetOperandReg(2)); //$ra-$rb

    JLBuilder(Temple::ZERO, COND_C, Temple::T1); // branch __C $rc

    MI.eraseFromParent();
    return true;
  } // BLTU
  case Temple::BGEU: {
    BuildMI(MBB, MBBI, MI.getDebugLoc(),
            MBB.getParent()->getSubtarget().getInstrInfo()->get(Temple::SETI))
        .add(MI.getOperand(
            0)); // Can't use ImmBuilder because Operand(0) is branch target.
    RegBuilder(MOVE, Temple::T1); // copy destination to T1

    JLBuilder(Temple::T0, COND_NEVER, Temple::ZERO); // load program counter
    ADDiPseudo(Temple::T0, 14);                      // need verification
    RegBuilder(MOVE, Temple::T0);

    SUBrPseudo(GetOperandReg(1), GetOperandReg(2)); //$ra-$rb

    JLBuilder(Temple::ZERO, COND_N, Temple::T0);      // branch __C fallthrough
    JLBuilder(Temple::ZERO, COND_ALWAYS, Temple::T1); // branch always $rc

    MI.eraseFromParent();
    return true;
  } // BGEU
  case Temple::SELECT: {
    RegBuilder(NOR, Temple::ALLONE); // clear ACC
    RegBuilder(ADD, GetOperandReg(3));
    RegBuilder(MOVE, GetOperandReg(0)); // $ra(result) = $rd(FALSEVAL)

    JLBuilder(Temple::T0, COND_NEVER, Temple::ZERO); // load program counter
    ADDiPseudo(Temple::T0, 13);                      // need verification
    RegBuilder(MOVE, Temple::T0);                    // jump address calculation

    RegBuilder(NOR, Temple::ALLONE);   // clear ACC
    RegBuilder(ADD, GetOperandReg(1)); // ACC = $rb (COND)
    JLBuilder(Temple::ZERO, COND_Z,
              Temple::T0); // branch Z__ (jump if cond is 0)

    RegBuilder(NOR, Temple::ALLONE); // clear ACC
    RegBuilder(ADD, GetOperandReg(2));
    RegBuilder(MOVE, GetOperandReg(0)); // $ra(result) = $rc(TRUEVAL)
                                        // jump here
    MI.eraseFromParent();
    return true;
  } // SELECT

  case Temple::SETEQi: {
    Register LHS = GetOperandReg(1);
    Register RHS = Temple::T1;

    ImmBuilder(SETI, GetOperandImm(2));
    RegBuilder(MOVE, Temple::T1); // copy to temp reg

    RegBuilder(NOR, Temple::ALLONE); // clear ACC
    RegBuilder(ADD, Temple::ONE);
    RegBuilder(MOVE, GetOperandReg(0)); // set default value(true) to $ra

    JLBuilder(Temple::T0, COND_NEVER, Temple::ZERO); // load PC
    ADDiPseudo(Temple::T0, 11); // jump address calculation, need verification

    SUBiPseudo(LHS, RHS); // $ra-$rb
    JLBuilder(Temple::ZERO, COND_Z, Temple::T0);

    MI.eraseFromParent();
    return true;
  }
  case Temple::SETNEi: {
    Register LHS = GetOperandReg(1);
    Register RHS = Temple::T1;

    ImmBuilder(SETI, GetOperandImm(2));
    RegBuilder(MOVE, Temple::T1); // copy to temp reg

    RegBuilder(NOR, Temple::ALLONE); // clear ACC
    RegBuilder(ADD, Temple::ZERO);
    RegBuilder(MOVE, GetOperandReg(0)); // set default value to $ra

    JLBuilder(Temple::T0, COND_NEVER, Temple::ZERO); // load PC
    ADDiPseudo(Temple::T0, 11); // jump address calculation, need verification

    SUBiPseudo(LHS, RHS); // $ra-$rb
    JLBuilder(Temple::ZERO, COND_Z, Temple::T0);
    MI.eraseFromParent();
    return true;
  }
  case Temple::SETLTi: {
    Register LHS = GetOperandReg(1);
    Register RHS = Temple::T1;

    ImmBuilder(SETI, GetOperandImm(2));
    RegBuilder(MOVE, Temple::T1); // copy to temp reg

    RegBuilder(NOR, Temple::ALLONE); // clear ACC
    RegBuilder(ADD, Temple::ONE);
    RegBuilder(MOVE, GetOperandReg(0)); // set default value to $ra

    JLBuilder(Temple::T0, COND_NEVER, Temple::ZERO); // load PC
    ADDiPseudo(Temple::T0, 11); // jump address calculation, need verification

    SUBiPseudo(LHS, RHS); // $ra-$rb
    JLBuilder(Temple::ZERO, COND_N, Temple::T0);
    MI.eraseFromParent();
    return true;
  }
  case Temple::SETGTi: {
    Register LHS = GetOperandReg(1);
    Register RHS = Temple::T1;

    ImmBuilder(SETI, GetOperandImm(2));
    RegBuilder(MOVE, Temple::T1); // copy to temp reg

    RegBuilder(NOR, Temple::ALLONE); // clear ACC
    RegBuilder(ADD, Temple::ONE);
    RegBuilder(MOVE, GetOperandReg(0)); // set default value to $ra

    JLBuilder(Temple::T0, COND_NEVER, Temple::ZERO); // load PC
    ADDiPseudo(Temple::T0, 11); // jump address calculation, need verification

    SUBiPseudo(RHS, LHS); // $rb-$ra
    JLBuilder(Temple::ZERO, COND_N, Temple::T0);
    MI.eraseFromParent();
    return true;
  }
  case Temple::SETLEi: {
    Register LHS = GetOperandReg(1);
    Register RHS = Temple::T1;

    ImmBuilder(SETI, GetOperandImm(2));
    RegBuilder(MOVE, Temple::T1); // copy to temp reg

    RegBuilder(NOR, Temple::ALLONE); // clear ACC
    RegBuilder(ADD, Temple::ZERO);
    RegBuilder(MOVE, GetOperandReg(0)); // set default value to $ra

    JLBuilder(Temple::T0, COND_NEVER, Temple::ZERO); // load PC
    ADDiPseudo(Temple::T0, 11); // jump address calculation, need verification

    SUBiPseudo(RHS, LHS); // $rb-$ra
    JLBuilder(Temple::ZERO, COND_N, Temple::T0);
    MI.eraseFromParent();
    return true;
  }
  case Temple::SETGEi: {
    Register LHS = GetOperandReg(1);
    Register RHS = Temple::T1;

    ImmBuilder(SETI, GetOperandImm(2));
    RegBuilder(MOVE, Temple::T1); // copy to temp reg

    RegBuilder(NOR, Temple::ALLONE); // clear ACC
    RegBuilder(ADD, Temple::ZERO);
    RegBuilder(MOVE, GetOperandReg(0)); // set default value to $ra

    JLBuilder(Temple::T0, COND_NEVER, Temple::ZERO); // load PC
    ADDiPseudo(Temple::T0, 11); // jump address calculation, need verification

    SUBiPseudo(LHS, RHS); // $ra-$rb
    JLBuilder(Temple::ZERO, COND_N, Temple::T0);
    MI.eraseFromParent();
    return true;
  }
  case Temple::SETULTi: {
    Register LHS = GetOperandReg(1);
    Register RHS = Temple::T1;

    ImmBuilder(SETI, GetOperandImm(2));
    RegBuilder(MOVE, Temple::T1); // copy to temp reg

    RegBuilder(NOR, Temple::ALLONE); // clear ACC
    RegBuilder(ADD, Temple::ONE);
    RegBuilder(MOVE, GetOperandReg(0)); // set default value to $ra

    JLBuilder(Temple::T0, COND_NEVER, Temple::ZERO); // load PC
    ADDiPseudo(Temple::T0, 11); // jump address calculation, need verification

    SUBiPseudo(LHS, RHS); // $ra-$rb
    JLBuilder(Temple::ZERO, COND_C, Temple::T0);
    MI.eraseFromParent();
    return true;
  }
  case Temple::SETUGTi: {
    Register LHS = GetOperandReg(1);
    Register RHS = Temple::T1;

    ImmBuilder(SETI, GetOperandImm(2));
    RegBuilder(MOVE, Temple::T1); // copy to temp reg

    RegBuilder(NOR, Temple::ALLONE); // clear ACC
    RegBuilder(ADD, Temple::ONE);
    RegBuilder(MOVE, GetOperandReg(0)); // set default value to $ra

    JLBuilder(Temple::T0, COND_NEVER, Temple::ZERO); // load PC
    ADDiPseudo(Temple::T0, 11); // jump address calculation, need verification

    SUBiPseudo(RHS, LHS); // $rb-$ra
    JLBuilder(Temple::ZERO, COND_C, Temple::T0);
    MI.eraseFromParent();
    return true;
  }
  case Temple::SETULEi: {
    Register LHS = GetOperandReg(1);
    Register RHS = Temple::T1;

    ImmBuilder(SETI, GetOperandImm(2));
    RegBuilder(MOVE, Temple::T1); // copy to temp reg

    RegBuilder(NOR, Temple::ALLONE); // clear ACC
    RegBuilder(ADD, Temple::ZERO);
    RegBuilder(MOVE, GetOperandReg(0)); // set default value to $ra

    JLBuilder(Temple::T0, COND_NEVER, Temple::ZERO); // load PC
    ADDiPseudo(Temple::T0, 11); // jump address calculation, need verification

    SUBiPseudo(RHS, LHS); // $rb-$ra
    JLBuilder(Temple::ZERO, COND_C, Temple::T0);
    MI.eraseFromParent();
    return true;
  }
  case Temple::SETUGEi: {
    Register LHS = GetOperandReg(1);
    Register RHS = Temple::T1;

    ImmBuilder(SETI, GetOperandImm(2));
    RegBuilder(MOVE, Temple::T1); // copy to temp reg

    RegBuilder(NOR, Temple::ALLONE); // clear ACC
    RegBuilder(ADD, Temple::ZERO);
    RegBuilder(MOVE, GetOperandReg(0)); // set default value to $ra

    JLBuilder(Temple::T0, COND_NEVER, Temple::ZERO); // load PC
    ADDiPseudo(Temple::T0, 11); // jump address calculation, need verification

    SUBiPseudo(LHS, RHS); // $ra-$rb
    JLBuilder(Temple::ZERO, COND_C, Temple::T0);
    MI.eraseFromParent();
    return true;
  }

  case Temple::SETEQr: {
    Register LHS = GetOperandReg(1);
    Register RHS = GetOperandReg(2);

    RegBuilder(NOR, Temple::ALLONE); // clear ACC
    RegBuilder(ADD, Temple::ONE);
    RegBuilder(MOVE, GetOperandReg(0)); // set default value(true) to $ra

    JLBuilder(Temple::T0, COND_NEVER, Temple::ZERO); // load PC
    ADDiPseudo(Temple::T0, 11); // jump address calculation, need verification

    SUBiPseudo(LHS, RHS); // $ra-$rb
    JLBuilder(Temple::ZERO, COND_Z, Temple::T0);
    MI.eraseFromParent();
    return true;
  }
  case Temple::SETNEr: {
    Register LHS = GetOperandReg(1);
    Register RHS = GetOperandReg(2);

    RegBuilder(NOR, Temple::ALLONE); // clear ACC
    RegBuilder(ADD, Temple::ZERO);
    RegBuilder(MOVE, GetOperandReg(0)); // set default value to $ra

    JLBuilder(Temple::T0, COND_NEVER, Temple::ZERO); // load PC
    ADDiPseudo(Temple::T0, 11); // jump address calculation, need verification

    SUBiPseudo(LHS, RHS); // $ra-$rb
    JLBuilder(Temple::ZERO, COND_Z, Temple::T0);
    MI.eraseFromParent();
    return true;
  }
  case Temple::SETLTr: {
    Register LHS = GetOperandReg(1);
    Register RHS = GetOperandReg(2);

    RegBuilder(NOR, Temple::ALLONE); // clear ACC
    RegBuilder(ADD, Temple::ONE);
    RegBuilder(MOVE, GetOperandReg(0)); // set default value to $ra

    JLBuilder(Temple::T0, COND_NEVER, Temple::ZERO); // load PC
    ADDiPseudo(Temple::T0, 11); // jump address calculation, need verification

    SUBiPseudo(LHS, RHS); // $ra-$rb
    JLBuilder(Temple::ZERO, COND_N, Temple::T0);
    MI.eraseFromParent();
    return true;
  }
  case Temple::SETGTr: {
    Register LHS = GetOperandReg(1);
    Register RHS = GetOperandReg(2);

    RegBuilder(NOR, Temple::ALLONE); // clear ACC
    RegBuilder(ADD, Temple::ONE);
    RegBuilder(MOVE, GetOperandReg(0)); // set default value to $ra

    JLBuilder(Temple::T0, COND_NEVER, Temple::ZERO); // load PC
    ADDiPseudo(Temple::T0, 11); // jump address calculation, need verification

    SUBiPseudo(RHS, LHS); // $rb-$ra
    JLBuilder(Temple::ZERO, COND_N, Temple::T0);
    MI.eraseFromParent();
    return true;
  }
  case Temple::SETLEr: {
    Register LHS = GetOperandReg(1);
    Register RHS = GetOperandReg(2);

    RegBuilder(NOR, Temple::ALLONE); // clear ACC
    RegBuilder(ADD, Temple::ZERO);
    RegBuilder(MOVE, GetOperandReg(0)); // set default value to $ra

    JLBuilder(Temple::T0, COND_NEVER, Temple::ZERO); // load PC
    ADDiPseudo(Temple::T0, 11); // jump address calculation, need verification

    SUBiPseudo(RHS, LHS); // $rb-$ra
    JLBuilder(Temple::ZERO, COND_N, Temple::T0);
    MI.eraseFromParent();
    return true;
  }
  case Temple::SETGEr: {
    Register LHS = GetOperandReg(1);
    Register RHS = GetOperandReg(2);

    RegBuilder(NOR, Temple::ALLONE); // clear ACC
    RegBuilder(ADD, Temple::ZERO);
    RegBuilder(MOVE, GetOperandReg(0)); // set default value to $ra

    JLBuilder(Temple::T0, COND_NEVER, Temple::ZERO); // load PC
    ADDiPseudo(Temple::T0, 11); // jump address calculation, need verification

    SUBiPseudo(LHS, RHS); // $ra-$rb
    JLBuilder(Temple::ZERO, COND_N, Temple::T0);
    MI.eraseFromParent();
    return true;
  }
  case Temple::SETULTr: {
    Register LHS = GetOperandReg(1);
    Register RHS = GetOperandReg(2);

    RegBuilder(NOR, Temple::ALLONE); // clear ACC
    RegBuilder(ADD, Temple::ONE);
    RegBuilder(MOVE, GetOperandReg(0)); // set default value to $ra

    JLBuilder(Temple::T0, COND_NEVER, Temple::ZERO); // load PC
    ADDiPseudo(Temple::T0, 11); // jump address calculation, need verification

    SUBiPseudo(LHS, RHS); // $ra-$rb
    JLBuilder(Temple::ZERO, COND_C, Temple::T0);
    MI.eraseFromParent();
    return true;
  }
  case Temple::SETUGTr: {
    Register LHS = GetOperandReg(1);
    Register RHS = GetOperandReg(2);

    RegBuilder(NOR, Temple::ALLONE); // clear ACC
    RegBuilder(ADD, Temple::ONE);
    RegBuilder(MOVE, GetOperandReg(0)); // set default value to $ra

    JLBuilder(Temple::T0, COND_NEVER, Temple::ZERO); // load PC
    ADDiPseudo(Temple::T0, 11); // jump address calculation, need verification

    SUBiPseudo(RHS, LHS); // $rb-$ra
    JLBuilder(Temple::ZERO, COND_C, Temple::T0);
    MI.eraseFromParent();
    return true;
  }
  case Temple::SETULEr: {
    Register LHS = GetOperandReg(1);
    Register RHS = GetOperandReg(2);

    RegBuilder(NOR, Temple::ALLONE); // clear ACC
    RegBuilder(ADD, Temple::ZERO);
    RegBuilder(MOVE, GetOperandReg(0)); // set default value to $ra

    JLBuilder(Temple::T0, COND_NEVER, Temple::ZERO); // load PC
    ADDiPseudo(Temple::T0, 11); // jump address calculation, need verification

    SUBiPseudo(RHS, LHS); // $rb-$ra
    JLBuilder(Temple::ZERO, COND_C, Temple::T0);
    MI.eraseFromParent();
    return true;
  }
  case Temple::SETUGEr: {
    Register LHS = GetOperandReg(1);
    Register RHS = GetOperandReg(2);

    RegBuilder(NOR, Temple::ALLONE); // clear ACC
    RegBuilder(ADD, Temple::ZERO);
    RegBuilder(MOVE, GetOperandReg(0)); // set default value to $ra

    JLBuilder(Temple::T0, COND_NEVER, Temple::ZERO); // load PC
    ADDiPseudo(Temple::T0, 11); // jump address calculation, need verification

    SUBiPseudo(LHS, RHS); // $ra-$rb
    JLBuilder(Temple::ZERO, COND_C, Temple::T0);
    MI.eraseFromParent();
    return true;
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

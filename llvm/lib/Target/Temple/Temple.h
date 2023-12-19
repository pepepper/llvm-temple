//===-- Temple.h - Top-level interface for Temple representation ----*- C++
//-*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains the entry points for global functions defined in
// the LLVM Temple back-end.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_TEMPLE_TEMPLE_H
#define LLVM_LIB_TARGET_TEMPLE_TEMPLE_H

#include "MCTargetDesc/TempleBaseInfo.h"
#include "llvm/PassRegistry.h"

namespace llvm {
class AsmPrinter;
class TempleTargetMachine;
class FunctionPass;
class MCInst;
class MCOperand;
class MachineInstr;
class MachineOperand;

void LowerTempleMachineInstrToMCInst(const MachineInstr *MI, MCInst &OutMI,
                                     const AsmPrinter &AP);
bool LowerTempleMachineOperandToMCOperand(const MachineOperand &MO,
                                          MCOperand &MCOp,
                                          const AsmPrinter &AP);

FunctionPass *createTempleISelDag(TempleTargetMachine &TM);
FunctionPass *createTempleExpandPseudoPass();

void initializeTempleExpandPseudoPass(PassRegistry &);
} // namespace llvm

#endif
//===-- TempleTargetMachine.cpp - Define TargetMachine for Temple
//-------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Implements the info about Temple target spec.
//
//===----------------------------------------------------------------------===//

#include "TempleTargetMachine.h"
#include "Temple.h"

#include "TempleSubtarget.h"
#include "llvm/CodeGen/Passes.h"
#include "llvm/CodeGen/TargetLoweringObjectFileImpl.h"
#include "llvm/CodeGen/TargetPassConfig.h"
#include "llvm/IR/Attributes.h"
#include "llvm/IR/Function.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Support/CodeGen.h"
#include "llvm/Target/TargetOptions.h"

using namespace llvm;

#define DEBUG_TYPE "temple"

extern "C" void LLVMInitializeTempleTarget() {
  RegisterTargetMachine<TempleTargetMachine> X(getTheTempleTarget());
}

static std::string computeDataLayout(const Triple &TT) {
  return "E-m:e-p:16:16-i16:16-n16-a:16-S16";
}

static Reloc::Model getEffectiveRelocModel(const Triple &TT,
                                           std::optional<Reloc::Model> RM) {
  if (!RM.has_value())
    return Reloc::Static;
  return *RM;
}

TempleTargetMachine::TempleTargetMachine(const Target &T, const Triple &TT,
                                         StringRef CPU, StringRef FS,
                                         const TargetOptions &Options,
                                         std::optional<Reloc::Model> RM,
                                         std::optional<CodeModel::Model> CM,
                                         CodeGenOpt::Level OL, bool JIT)
    : LLVMTargetMachine(T, computeDataLayout(TT), TT, CPU, FS, Options,
                        getEffectiveRelocModel(TT, RM),
                        getEffectiveCodeModel(CM, CodeModel::Small), OL),
      TLOF(std::make_unique<TargetLoweringObjectFileELF>()),
      Subtarget(TT, CPU, FS, *this) {
  initAsmInfo();
}

namespace {
class TemplePassConfig : public TargetPassConfig {
public:
  TemplePassConfig(TempleTargetMachine &TM, PassManagerBase &PM)
      : TargetPassConfig(TM, PM) {}

  TempleTargetMachine &getTempleTargetMachine() const {
    return getTM<TempleTargetMachine>();
  }

  bool addInstSelector() override;
  void addPreEmitPass() override;
};
} // namespace

TargetPassConfig *TempleTargetMachine::createPassConfig(PassManagerBase &PM) {
  return new TemplePassConfig(*this, PM);
}

bool TemplePassConfig::addInstSelector() {
  addPass(createTempleISelDag(getTempleTargetMachine()));

  return false;
}

void TemplePassConfig::addPreEmitPass() { addPass(&BranchRelaxationPassID); }

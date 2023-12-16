// This file is copied and modified from The LLVM Compiler Infrastructure, which
// is distributed under the Apache License v2.0 with LLVM Exceptions (see
// LICENSE.TXT for details). This file is licensed under the same license.

#include "TempleMCTargetDesc.h"
#include "TempleInstPrinter.h"
#include "TempleMCAsmInfo.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/MC/MCAsmInfo.h"
#include "llvm/MC/MCInstrInfo.h"
#include "llvm/MC/MCRegisterInfo.h"
#include "llvm/MC/MCStreamer.h"
#include "llvm/MC/MCSubtargetInfo.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Support/ErrorHandling.h"

#define GET_INSTRINFO_MC_DESC
#include "TempleGenInstrInfo.inc"

#define GET_REGINFO_MC_DESC
#include "TempleGenRegisterInfo.inc"

#define GET_SUBTARGETINFO_MC_DESC
#include "TempleGenSubtargetInfo.inc"

using namespace llvm;

static MCInstrInfo *createTempleMCInstrInfo() {
  MCInstrInfo *X = new MCInstrInfo();
  InitTempleMCInstrInfo(X);
  return X;
}

static MCRegisterInfo *createTempleMCRegisterInfo(const Triple &TT) {
  MCRegisterInfo *X = new MCRegisterInfo();

  // X0 is the return address register.
  InitTempleMCRegisterInfo(X, Temple::RA);

  return X;
}

static MCAsmInfo *createTempleMCAsmInfo(const MCRegisterInfo &MRI,
                                        const Triple &TT,
                                        const MCTargetOptions &Options) {
  return new TempleMCAsmInfo(TT);
}

static MCSubtargetInfo *
createTempleMCSubtargetInfo(const Triple &TT, StringRef CPU, StringRef FS) {
  StringRef CPUName = CPU;
  if (CPUName.empty())
    CPUName = "generic";
  return createTempleMCSubtargetInfoImpl(TT, CPUName, CPUName, FS);
}

static MCInstPrinter *createTempleMCInstPrinter(const Triple &T,
                                                unsigned SyntaxVariant,
                                                const MCAsmInfo &MAI,
                                                const MCInstrInfo &MII,
                                                const MCRegisterInfo &MRI) {
  return new TempleInstPrinter(MAI, MII, MRI);
}

extern "C" void LLVMInitializeTempleTargetMC() {
  Target &T = getTheTempleTarget();
  TargetRegistry::RegisterMCAsmInfo(T, createTempleMCAsmInfo);
  TargetRegistry::RegisterMCInstrInfo(T, createTempleMCInstrInfo);
  TargetRegistry::RegisterMCRegInfo(T, createTempleMCRegisterInfo);
  TargetRegistry::RegisterMCAsmBackend(T, createTempleAsmBackend);
  TargetRegistry::RegisterMCSubtargetInfo(T, createTempleMCSubtargetInfo);
  TargetRegistry::RegisterMCCodeEmitter(T, createTempleMCCodeEmitter);
  TargetRegistry::RegisterMCInstPrinter(T, createTempleMCInstPrinter);
}
// This file is copied and modified from The LLVM Compiler Infrastructure, which
// is distributed under the Apache License v2.0 with LLVM Exceptions (see
// LICENSE.TXT for details). This file is licensed under the same license.

#include "TempleSubtarget.h"
#include "Temple.h"
#include "TempleFrameLowering.h"
#include "llvm/MC/TargetRegistry.h"

using namespace llvm;

#define DEBUG_TYPE "temple-subtarget"

#define GET_SUBTARGETINFO_TARGET_DESC
#define GET_SUBTARGETINFO_CTOR
#include "TempleGenSubtargetInfo.inc"

void TempleSubtarget::anchor() {}

TempleSubtarget &
TempleSubtarget::initializeSubtargetDependencies(StringRef CPU, StringRef FS) {
  // Determine default and user-specified characteristics
  StringRef CPUName = CPU;
  if (CPUName.empty())
    CPUName = "generic";
  ParseSubtargetFeatures(CPUName, CPUName, FS);
  return *this;
}

TempleSubtarget::TempleSubtarget(const Triple &TT, const StringRef CPU,
                                 const StringRef FS, const TargetMachine &TM)
    : TempleGenSubtargetInfo(TT, CPU, CPU, FS),
      FrameLowering(initializeSubtargetDependencies(CPU, FS)), InstrInfo(),
      RegInfo(getHwMode()), TLInfo(TM, *this) {}

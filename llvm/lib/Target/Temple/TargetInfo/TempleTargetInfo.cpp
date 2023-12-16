//===-- TempleTargetInfo.cpp - Temple Target Implementation
//-------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "Temple.h"
#include "llvm/IR/Module.h"
#include "llvm/MC/TargetRegistry.h"
using namespace llvm;

namespace llvm {
Target &getTheTempleTarget() {
  static Target TheTempleTarget;
  return TheTempleTarget;
}
} // namespace llvm

extern "C" void LLVMInitializeTempleTargetInfo() {
  RegisterTarget<Triple::temple> X(getTheTempleTarget(), "temple", "Temple",
                                   "Temple");
}

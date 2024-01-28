//===--- Temple.cpp - Implement Temple target feature support -----------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file implements Temple TargetInfo objects.
//
//===----------------------------------------------------------------------===//

#include "Temple.h"
#include "Targets.h"
#include "clang/Basic/Diagnostic.h"
#include "clang/Basic/MacroBuilder.h"
#include "clang/Basic/TargetBuiltins.h"
#include "llvm/ADT/StringSwitch.h"

using namespace clang;
using namespace clang::targets;

const Builtin::Info TempleTargetInfo::BuiltinInfo[] = {
#define BUILTIN(ID, TYPE, ATTRS)                                               \
  {#ID, TYPE, ATTRS, nullptr, ALL_LANGUAGES, nullptr},
#define LIBBUILTIN(ID, TYPE, ATTRS, HEADER)                                    \
  {#ID, TYPE, ATTRS, HEADER, ALL_LANGUAGES, nullptr},
// #include "clang/Basic/BuiltinsTemple.def"
};

static constexpr llvm::StringLiteral ValidCPUNames[] = {
    {"temple"}};

bool TempleTargetInfo::isValidCPUName(StringRef Name) const {
  return llvm::find(ValidCPUNames, Name) != std::end(ValidCPUNames);
}

unsigned TempleTargetInfo::getISARev() const {
  return llvm::StringSwitch<unsigned>(getCPU())
             .Case("temple", 1)
             .Default(0);
}

void TempleTargetInfo::getTargetDefines(const LangOptions &Opts,
                                      MacroBuilder &Builder) const {
    DefineStd(Builder, "TEMPLE", Opts);
    Builder.defineMacro("_TEMPLE");

  Builder.defineMacro("__temple__");
  Builder.defineMacro("_temple");
  if (Opts.GNUMode)
    Builder.defineMacro("temple");

  if (ABI == "o16" || ABI == "s16") {
    Builder.defineMacro("__temple", "16");
    Builder.defineMacro("_TEMPLE_ISA", "_TEMPLE_ISA_TEMPLE16");
  } else {
    llvm_unreachable("Invalid ABI.");
  }

  const std::string ISARev = std::to_string(getISARev());

  if (!ISARev.empty())
    Builder.defineMacro("__temple_isa_rev", ISARev);

  if (ABI == "o16") {
    Builder.defineMacro("__temple_o16");
    Builder.defineMacro("_ABIO16", "1");
    Builder.defineMacro("_TEMPLE_SIM", "_ABIO16");
  } else if (ABI == "s16") {
    Builder.defineMacro("__temple_n16");
    Builder.defineMacro("_ABIS16", "2");
    Builder.defineMacro("_TEMPLE_SIM", "_ABIN16");
  } else
    llvm_unreachable("Invalid ABI.");

  Builder.defineMacro("__REGISTER_PREFIX__", "");

  switch (FloatABI) {
  case HardFloat:
    llvm_unreachable("HardFloat is not support in Temple");
    break;
  case SoftFloat:
    Builder.defineMacro("__temple_soft_float", Twine(1));
    break;
  }
}

bool TempleTargetInfo::hasFeature(StringRef Feature) const {
  return llvm::StringSwitch<bool>(Feature)
      .Case("temple", true)
      .Default(false);
}

ArrayRef<Builtin::Info> TempleTargetInfo::getTargetBuiltins() const {
  return std::nullopt;
}

unsigned TempleTargetInfo::getUnwindWordWidth() const {
  return 16;
}

bool TempleTargetInfo::validateTarget(DiagnosticsEngine &Diags) const {
  if (CPU != "temple") {
    Diags.Report(diag::err_target_unknown_cpu) << ABI << CPU;
    return false;
  }

  return true;
}
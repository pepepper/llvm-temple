//===--- Temple.h - Declare Temple target feature support -----------*- C++
//-*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file declares Temple TargetInfo objects.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_LIB_BASIC_TARGETS_TEMPLE_H
#define LLVM_CLANG_LIB_BASIC_TARGETS_TEMPLE_H

#include "clang/Basic/TargetInfo.h"
#include "clang/Basic/TargetOptions.h"
#include "llvm/ADT/Triple.h"
#include "llvm/Support/Compiler.h"

namespace clang {
namespace targets {

class LLVM_LIBRARY_VISIBILITY TempleTargetInfo : public TargetInfo {
  void setDataLayout() { resetDataLayout("E-m:e-p:16:16-i16:16-n16-a:16-S16"); }

  static const Builtin::Info BuiltinInfo[];
  std::string CPU;

protected:
  std::string ABI;
  enum TempleFloatABI { HardFloat, SoftFloat } FloatABI;

public:
  TempleTargetInfo(const llvm::Triple &Triple, const TargetOptions &Opt)
      : TargetInfo(Triple) {
    TLSSupported = false;
    PointerWidth = 16;
    PointerAlign = 16;
    IntWidth = 16;
    IntAlign = 16;
    
    TheCXXABI.set(TargetCXXABI::GenericItanium); // ?

    setABI("o16");

    CPU = "temple";
  }

  StringRef getABI() const override { return ABI; }

  bool setABI(const std::string &Name) override {
    if (Name == "o16") {
      ABI = Name;
      return true;
    }
    return false;
  }

  bool isValidCPUName(StringRef Name) const override;

  bool setCPU(const std::string &Name) override {
    CPU = Name;
    return isValidCPUName(Name);
  }

  const std::string &getCPU() const { return CPU; }
  bool
  initFeatureMap(llvm::StringMap<bool> &Features, DiagnosticsEngine &Diags,
                 StringRef CPU,
                 const std::vector<std::string> &FeaturesVec) const override {
    // if (CPU.empty())
    //   CPU = getCPU();
    // if (CPU == "temple32II")
    //   Features["HasCmp"] = Features["HasSlt"] = true;
    // else if (CPU == "temple32I")
    //   Features["HasCmp"] = true;
    // else
    //   assert(0 && "incorrect CPU");
    return TargetInfo::initFeatureMap(Features, Diags, CPU, FeaturesVec);
  }

  unsigned getISARev() const;

  void getTargetDefines(const LangOptions &Opts,
                        MacroBuilder &Builder) const override;

  ArrayRef<Builtin::Info> getTargetBuiltins() const override;

  bool hasFeature(StringRef Feature) const override;

  BuiltinVaListKind getBuiltinVaListKind() const override {
    return TargetInfo::VoidPtrBuiltinVaList;
  }

  ArrayRef<const char *> getGCCRegNames() const override {
    static const char *const GCCRegNames[] = {
        // CPU register names
        // Must match second column of GCCRegAliases
        "$0", "$1", "$2",  "$3",  "$4",  "$5",  "$6",  "$7",
        "$8", "$9", "$10", "$11", "$12", "$13", "$14", "$15",
    };
    return llvm::makeArrayRef(GCCRegNames);
  }

  bool validateAsmConstraint(const char *&Name,
                             TargetInfo::ConstraintInfo &Info) const override {
    switch (*Name) {
    default:
      return false;
    case 'r': // CPU registers.
    case 'd': // Equivalent to "r" unless generating MIPS16 code.
    case 'y': // Equivalent to "r", backward compatibility only.
    // case 'f': // floating-point registers.
    case 'c': // $6 for indirect jumps
    case 'l': // lo register
    case 'x': // hilo register pair
      Info.setAllowsRegister();
      return true;
    case 'I': // Signed 16-bit constant
    case 'J': // Integer 0
    case 'K': // Unsigned 16-bit constant
    case 'L': // Signed 32-bit constant, lower 16-bit zeros (for lui)
    case 'M': // Constants not loadable via lui, addiu, or ori
    case 'N': // Constant -1 to -65535
    case 'O': // A signed 15-bit constant
    case 'P': // A constant between 1 go 65535
      return true;
    case 'R': // An address that can be used in a non-macro load or store
      Info.setAllowsMemory();
      return true;
    case 'Z':
      if (Name[1] == 'C') { // An address usable by ll, and sc.
        Info.setAllowsMemory();
        Name++; // Skip over 'Z'.
        return true;
      }
      return false;
    }
  }

  const char *getClobbers() const override { return ""; }

  bool handleTargetFeatures(std::vector<std::string> &Features,
                            DiagnosticsEngine &Diags) override {
    FloatABI = SoftFloat;

    setDataLayout();

    return true;
  }

  ArrayRef<TargetInfo::GCCRegAlias> getGCCRegAliases() const override {
    static const TargetInfo::GCCRegAlias RegAliases[] = {
        {{"r1"}, "$1"},  {{"r2"}, "$2"}, {{"r3"}, "$3"},   {{"r4"}, "$4"},
        {{"r5"}, "$5"},  {{"r6"}, "$6"}, {{"r11"}, "$11"}, {{"fp"}, "$12"},
        {{"sp"}, "$13"}, {{"ra"}, "$14"}};
    return llvm::makeArrayRef(RegAliases);
  }

  bool hasInt128Type() const override { return false; }

  unsigned getUnwindWordWidth() const override;

  bool validateTarget(DiagnosticsEngine &Diags) const override;
};
} // namespace targets
} // namespace clang

#endif // LLVM_CLANG_LIB_BASIC_TARGETS_TEMPLE_H
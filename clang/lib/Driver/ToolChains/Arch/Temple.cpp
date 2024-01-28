//===--- Temple.cpp - Tools Implementations -----------------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "Temple.h"
#include "ToolChains/CommonArgs.h"
#include "clang/Driver/Driver.h"
#include "clang/Driver/DriverDiagnostic.h"
#include "clang/Driver/Options.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/Option/ArgList.h"

using namespace clang::driver;
using namespace clang::driver::tools;
using namespace clang;
using namespace llvm::opt;

// Get CPU and ABI names. They are not independent
// so we have to calculate them together.
void temple::getTempleCPUAndABI(const ArgList &Args, const llvm::Triple &Triple,
                            StringRef &CPUName, StringRef &ABIName) {
  if (Arg *A = Args.getLastArg(clang::driver::options::OPT_march_EQ,
                               options::OPT_mcpu_EQ))
    CPUName = A->getValue();

  if (Arg *A = Args.getLastArg(options::OPT_mabi_EQ)) {
    ABIName = A->getValue();
    // Convert a GNU style Temple ABI name to the name
    // accepted by LLVM Temple backend.
    ABIName = llvm::StringSwitch<llvm::StringRef>(ABIName)
                  .Case("16", "o16")
                  .Default(ABIName);
  }

  // Setup default CPU and ABI names.
  if (CPUName.empty()) {
    switch (Triple.getArch()) {
    default:
      llvm_unreachable("Unexpected triple arch name");
    case llvm::Triple::temple:
      CPUName = "temple";
      break;
    }
  }

  if (ABIName.empty())
    ABIName = "o16";
}
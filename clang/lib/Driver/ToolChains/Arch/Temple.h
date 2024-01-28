//===--- Temple.h - Temple-specific Tool Helpers ----------------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_LIB_DRIVER_TOOLCHAINS_ARCH_TEMPLE_H
#define LLVM_CLANG_LIB_DRIVER_TOOLCHAINS_ARCH_TEMPLE_H

#include "clang/Driver/Driver.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/Triple.h"
#include "llvm/Option/Option.h"
#include <string>
#include <vector>

namespace clang {
namespace driver {
namespace tools {

namespace temple {

void getTempleCPUAndABI(const llvm::opt::ArgList &Args,
                      const llvm::Triple &Triple, StringRef &CPUName,
                      StringRef &ABIName);

} // end namespace Temple
} // end namespace target
} // end namespace driver
} // end namespace clang

#endif // LLVM_CLANG_LIB_DRIVER_TOOLCHAINS_ARCH_TEMPLE_H
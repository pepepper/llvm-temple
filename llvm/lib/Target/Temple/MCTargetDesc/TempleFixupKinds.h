// This file is copied and modified from The LLVM Compiler Infrastructure, which
// is distributed under the Apache License v2.0 with LLVM Exceptions (see
// LICENSE.TXT for details). This file is licensed under the same license.

#ifndef LLVM_LIB_TARGET_TEMPLE_MCTARGETDESC_TEMPLEFIXUPKINDS_H
#define LLVM_LIB_TARGET_TEMPLE_MCTARGETDESC_TEMPLEFIXUPKINDS_H

#include "llvm/MC/MCFixup.h"

#undef Temple

namespace llvm {
namespace Temple {
enum Fixups {
  // fixup_Temple_invalid - used as a sentinel and a marker, must be last
  // fixup
  fixup_Temple_invalid = FirstTargetFixupKind,
  NumTargetFixupKinds = fixup_Temple_invalid - FirstTargetFixupKind
};
} // end namespace Temple
} // end namespace llvm

#endif
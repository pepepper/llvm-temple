// This file is copied and modified from The LLVM Compiler Infrastructure, which
// is distributed under the Apache License v2.0 with LLVM Exceptions (see
// LICENSE.TXT for details). This file is licensed under the same license.

#ifndef LLVM_LIB_TARGET_TEMPLE_MCTARGETDESC_TEMPLEMCTARGETDESC_H
#define LLVM_LIB_TARGET_TEMPLE_MCTARGETDESC_TEMPLEMCTARGETDESC_H

#include "llvm/Config/config.h"
#include "llvm/MC/MCTargetOptions.h"
#include "llvm/Support/DataTypes.h"
#include <memory>

namespace llvm {
class MCAsmBackend;
class MCCodeEmitter;
class MCContext;
class MCInstrInfo;
class MCObjectTargetWriter;
class MCRegisterInfo;
class MCSubtargetInfo;
class StringRef;
class Target;
class Triple;
class raw_ostream;
class raw_pwrite_stream;

Target &getTheTempleTarget();

MCCodeEmitter *createTempleMCCodeEmitter(const MCInstrInfo &MCII,
                                         MCContext &Ctx);

MCAsmBackend *createTempleAsmBackend(const Target &T,
                                     const MCSubtargetInfo &STI,
                                     const MCRegisterInfo &MRI,
                                     const MCTargetOptions &Options);

std::unique_ptr<MCObjectTargetWriter>
createTempleELFObjectWriter(uint8_t OSABI);
} // namespace llvm

// Defines symbolic names for Temple registers.
#define GET_REGINFO_ENUM
#include "TempleGenRegisterInfo.inc"

// Defines symbolic names for Temple instructions.
#define GET_INSTRINFO_ENUM
#include "TempleGenInstrInfo.inc"

#endif
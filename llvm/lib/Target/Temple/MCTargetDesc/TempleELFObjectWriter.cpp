// This file is copied and modified from The LLVM Compiler Infrastructure, which
// is distributed under the Apache License v2.0 with LLVM Exceptions (see
// LICENSE.TXT for details). This file is licensed under the same license.

#include "MCTargetDesc/TempleFixupKinds.h"
#include "MCTargetDesc/TempleMCTargetDesc.h"
#include "llvm/MC/MCELFObjectWriter.h"
#include "llvm/MC/MCFixup.h"
#include "llvm/MC/MCObjectWriter.h"
#include "llvm/Support/ErrorHandling.h"

using namespace llvm;

namespace {
class TempleELFObjectWriter : public MCELFObjectTargetWriter {
public:
  TempleELFObjectWriter(uint8_t OSABI);

  ~TempleELFObjectWriter() override;

protected:
  unsigned getRelocType(MCContext &Ctx, const MCValue &Target,
                        const MCFixup &Fixup, bool IsPCRel) const override;
};
} // namespace

TempleELFObjectWriter::TempleELFObjectWriter(uint8_t OSABI)
    : MCELFObjectTargetWriter(false, OSABI, ELF::EM_TEMPLE,
                              /*HasRelocationAddend*/ true) {}

TempleELFObjectWriter::~TempleELFObjectWriter() {}

unsigned TempleELFObjectWriter::getRelocType(MCContext &Ctx,
                                             const MCValue &Target,
                                             const MCFixup &Fixup,
                                             bool IsPCRel) const {
  // Determine the type of the relocation
  switch ((unsigned)Fixup.getKind()) {
  default:
    llvm_unreachable("invalid fixup kind!");
    case Temple::fixup_Temple_regavoid:
      return ELF::R_Temple_ADDR;
  }
}

std::unique_ptr<MCObjectTargetWriter>
llvm::createTempleELFObjectWriter(uint8_t OSABI) {
  return std::make_unique<TempleELFObjectWriter>(OSABI);
}
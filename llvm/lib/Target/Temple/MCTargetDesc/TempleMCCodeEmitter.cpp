// This file is copied and modified from The LLVM Compiler Infrastructure, which
// is distributed under the Apache License v2.0 with LLVM Exceptions (see
// LICENSE.TXT for details). This file is licensed under the same license.

#include "MCTargetDesc/TempleFixupKinds.h"
#include "MCTargetDesc/TempleMCExpr.h"
#include "MCTargetDesc/TempleMCTargetDesc.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/MC/MCAsmInfo.h"
#include "llvm/MC/MCCodeEmitter.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCInst.h"
#include "llvm/MC/MCInstBuilder.h"
#include "llvm/MC/MCInstrDesc.h"
#include "llvm/MC/MCInstrInfo.h"
#include "llvm/MC/MCRegisterInfo.h"
#include "llvm/MC/MCSymbol.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/EndianStream.h"
#include "llvm/Support/raw_ostream.h"

using namespace llvm;

#define DEBUG_TYPE "mccodeemitter"

STATISTIC(MCNumEmitted, "Number of MC instructions emitted");
STATISTIC(MCNumFixups, "Number of MC fixups created");

namespace {
class TempleMCCodeEmitter : public MCCodeEmitter {
  TempleMCCodeEmitter(const TempleMCCodeEmitter &) = delete;
  void operator=(const TempleMCCodeEmitter &) = delete;
  MCContext &Ctx;
  MCInstrInfo const &MCII;

public:
  TempleMCCodeEmitter(MCContext &ctx, const MCInstrInfo &MCII)
      : Ctx(ctx), MCII(MCII) {}

  ~TempleMCCodeEmitter() override {}

  void encodeInstruction(const MCInst &MI, raw_ostream &OS,
                         SmallVectorImpl<MCFixup> &Fixups,
                         const MCSubtargetInfo &STI) const override;

  /// TableGen'erated function for getting the binary encoding for an
  /// instruction.
  uint64_t getBinaryCodeForInstr(const MCInst &MI,
                                 SmallVectorImpl<MCFixup> &Fixups,
                                 const MCSubtargetInfo &STI) const;

  /// Return binary encoding of operand. If the machine operand requires
  /// relocation, record the relocation and return zero.
  unsigned getMachineOpValue(const MCInst &MI, const MCOperand &MO,
                             SmallVectorImpl<MCFixup> &Fixups,
                             const MCSubtargetInfo &STI) const;
  unsigned encodeSimm16(const MCInst &MI, unsigned opno,
                        SmallVectorImpl<MCFixup> &Fixups,
                        const MCSubtargetInfo &STI) const;


private:
  void expandHlt(const MCInst &MI, raw_ostream &OS,
                 SmallVectorImpl<MCFixup> &Fixups,
                 const MCSubtargetInfo &STI) const;
};
} // end anonymous namespace

MCCodeEmitter *llvm::createTempleMCCodeEmitter(const MCInstrInfo &MCII,
                                               MCContext &Ctx) {
  return new TempleMCCodeEmitter(Ctx, MCII);
}

// void TempleMCCodeEmitter::expandHlt(const MCInst &MI, raw_ostream &OS,
//                                   SmallVectorImpl<MCFixup> &Fixups,
//                                   const MCSubtargetInfo &STI) const {
//   // Emit `js 0`
//   MCInst TmpInst = MCInstBuilder(Temple::JS).addImm(0);
//   uint16_t Bits = getBinaryCodeForInstr(TmpInst, Fixups, STI);
//   support::endian::write<uint16_t>(OS, Bits, support::little);
// }

void TempleMCCodeEmitter::encodeInstruction(const MCInst &MI, raw_ostream &OS,
                                            SmallVectorImpl<MCFixup> &Fixups,
                                            const MCSubtargetInfo &STI) const {
  const MCInstrDesc &Desc = MCII.get(MI.getOpcode());

  //   if (MI.getOpcode() == Temple::PseudoHLT) {
  //     expandHlt(MI, OS, Fixups, STI);
  //     MCNumEmitted += 2;
  //     return;
  //   }

  // Get byte count of instruction.
  unsigned Size = Desc.getSize();

  switch (Size) {
  default:
    llvm_unreachable("Unhandled encodeInstruction length!");
  case 1: {
    uint8_t Bits = getBinaryCodeForInstr(MI, Fixups, STI);
    support::endian::write<uint8_t>(OS, Bits, support::big);
    break;
  }
  case 2: {
    uint16_t Bits = getBinaryCodeForInstr(MI, Fixups, STI);
    support::endian::write<uint16_t>(OS, Bits, support::big);
    break;
  }
  case 3: {
    uint32_t Bits = getBinaryCodeForInstr(MI, Fixups, STI);
    support::endian::write<uint16_t>(OS, (Bits >> 8) & 0xffff, support::big);
    OS.write(Bits & 0xff);
    break;
  }
  }

  ++MCNumEmitted; // Keep track of the # of mi's emitted.
}

unsigned
TempleMCCodeEmitter::getMachineOpValue(const MCInst &MI, const MCOperand &MO,
                                       SmallVectorImpl<MCFixup> &Fixups,
                                       const MCSubtargetInfo &STI) const {

  if (MO.isReg())
    return Ctx.getRegisterInfo()->getEncodingValue(MO.getReg());

  if (MO.isImm())
    return static_cast<unsigned>(MO.getImm());

  llvm_unreachable("Unhandled expression!");
  return 0;
}

unsigned TempleMCCodeEmitter::encodeSimm16(const MCInst &MI, unsigned opno,
                                           SmallVectorImpl<MCFixup> &Fixups,
                                           const MCSubtargetInfo &STI) const {
  if (MI.getOperand(opno).isImm()) {
    assert(isInt<16>(MI.getOperand(opno).getImm()) && "Invalid immediate");
    uint16_t imm = MI.getOperand(opno).getImm();
    return static_cast<unsigned>((imm << 8) | (imm >> 8));
  } else if (MI.getOperand(opno).isExpr()) {
    const MCExpr *Expr = MI.getOperand(opno).getExpr();
    MCExpr::ExprKind Kind = Expr->getKind();
    Temple::Fixups FixupKind = Temple::fixup_Temple_invalid;
    unsigned Offset = 0;

    if (Kind == MCExpr::Target) {
      const TempleMCExpr *TempleExpr = cast<TempleMCExpr>(Expr);

      switch (TempleExpr->getKind()) {
      case TempleMCExpr::VK_Temple_None:
      case TempleMCExpr::VK_Temple_Invalid:
        llvm_unreachable("Unhandled fixup kind!");

        // case TempleMCExpr::VK_Temple_LO:
        //   FixupKind = Temple::fixup_Temple_lo10;
        //   break;

        // case TempleMCExpr::VK_Temple_HI:
        //   FixupKind = Temple::fixup_Temple_hi6;
        //   break;
      }
    } else if (Kind == MCExpr::SymbolRef &&
               cast<MCSymbolRefExpr>(Expr)->getKind() ==
                   MCSymbolRefExpr::VK_None) {
          FixupKind = Temple::fixup_Temple_regavoid;
    }

    assert(FixupKind != Temple::fixup_Temple_invalid &&
           "Unhandled expression!");

    Fixups.push_back(MCFixup::create(
        Offset, Expr, static_cast<MCFixupKind>(FixupKind), MI.getLoc()));
    ++MCNumFixups;

    return 0;
  }
  llvm_unreachable("not a immediate!");
  return 0;
}

#include "TempleGenMCCodeEmitter.inc"